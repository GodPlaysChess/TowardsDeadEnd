package functionalex.part3

import functionalex.part1.{Branch, Leaf, Tree}

trait Foldable[F[_]] {

  def foldRight[A, B](as: F[A])(z: B)(f: (A, B) => B): B

  def foldLeft[A, B](as: F[A])(z: B)(f: (B, A) => B): B

  def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B

  def concatenate[A](as: F[A])(m: Monoid[A]): A =
    foldLeft(as)(m.zero)(m.op)

  def toList[A](fa: F[A]): List[A] =
    foldRight(fa)(List.empty[A])(_ :: _)

}

object Foldables {
  def foldableList: Foldable[List] = new Foldable[List] {
    override def foldRight[A, B](as: List[A])(z: B)(f: (A, B) => B): B =
      foldLeft(as.reverse)(z)((a, b) => f(b, a))

    override def foldLeft[A, B](as: List[A])(z: B)(f: (B, A) => B): B = as match {
      case Nil => z
      case x :: xs => foldLeft(xs)(f(z, x))(f)
    }

    override def foldMap[A, B](as: List[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))
  }

  // taking advantage over splitting of indexed seq
  def foldableIndexedSeq: Foldable[IndexedSeq] = new Foldable[IndexedSeq] {
    override def foldLeft[A, B](as: IndexedSeq[A])(z: B)(f: (B, A) => B): B = {
      if (as.length == 0) z
      else if (as.length == 1) f(z, as.head)
      else {
        val (l, r) = as.splitAt(2)
        foldLeft(r)(foldLeft(l)(z)(f))(f)
      }
    }

    override def foldMap[A, B](as: IndexedSeq[A])(f: (A) => B)(mb: Monoid[B]): B =
      Monoids.foldMapV(as, mb)(f)

    override def foldRight[A, B](as: IndexedSeq[A])(z: B)(f: (A, B) => B): B = {
      if (as.length == 0) z
      else if (as.length == 1) f(as.head, z)
      else {
        val (l, r) = as.splitAt(2)
        foldRight(l)(foldRight(r)(z)(f))(f)
      }
    }
  }

  def foldableStream: Foldable[Stream] = new Foldable[Stream] {
    override def foldLeft[A, B](as: Stream[A])(z: B)(f: (B, A) => B): B = {
      if (as.isEmpty) z
      else foldLeft(as.tail)(f(z, as.head))(f)
    }

    override def foldMap[A, B](as: Stream[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((b, a) => mb.op(b, f(a)))

    override def foldRight[A, B](as: Stream[A])(z: B)(f: (A, B) => B): B = {
      if (as.isEmpty) z
      else f(as.head, as.tail.foldRight(z)(f))
    }
  }

  def foldableTree: Foldable[Tree] = new Foldable[Tree] {
    override def foldLeft[A, B](as: Tree[A])(z: B)(f: (B, A) => B): B = as match {
      case Leaf(a) => f(z, a)
      case Branch(l, r) => foldLeft(r)(foldLeft(l)(z)(f))(f)
    }

    override def foldMap[A, B](as: Tree[A])(f: (A) => B)(mb: Monoid[B]): B = as match {
      case Leaf(a) => mb.op(f(a), mb.zero)
      case Branch(l, r) => mb.op(foldMap(l)(f)(mb), foldMap(r)(f)(mb))
    }

    override def foldRight[A, B](as: Tree[A])(z: B)(f: (A, B) => B): B = as match {
      case Leaf(a) => f(a, z)
      case Branch(l, r) => foldRight(l)(foldRight(r)(z)(f))(f)
    }
  }

  def foldableOption: Foldable[Option] = new Foldable[Option] {
    override def foldLeft[A, B](as: Option[A])(z: B)(f: (B, A) => B): B =
      as.map(v => f(z, v)) getOrElse z

    override def foldMap[A, B](as: Option[A])(f: (A) => B)(mb: Monoid[B]): B =
      foldLeft(as)(mb.zero)((a, b) => mb.op(a, f(b)))

    override def foldRight[A, B](as: Option[A])(z: B)(f: (A, B) => B): B =
      foldLeft(as)(z)((a, b) => f(b, a))
  }
}

