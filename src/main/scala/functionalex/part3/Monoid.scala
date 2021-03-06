package functionalex.part3

import functionalex.part2.Par.Par
import functionalex.part2.Prop._
import functionalex.part2.{Gen, Par, Prop}

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoids {

//    def map[A, B](m: Monoid[A])(f: A => B): Monoid[B] = new Monoid[B] {
//      override def op(a1: B, a2: B): B = endoMonoid[B].op(x => a1, x2 => a2)
//
//      override def zero: B = f(m.zero)
//    }

  def par[A](m: Monoid[A]): Monoid[Par[A]] =
    new Monoid[Par[A]] {
      override def op(a1: Par[A], a2: Par[A]): Par[A] = Par.map2(a1, a2)(m.op)

      override def zero: Par[A] = Par.unit(m.zero)
    }

  val intAddition: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2

    override def zero: Int = 0
  }

  val intMultiplication: Monoid[Int] = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2

    override def zero: Int = 1
  }

  val booleanOr: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2

    override def zero: Boolean = false
  }

  val booleanAnd: Monoid[Boolean] = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 && a2

    override def zero = true
  }

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] =
      a1.orElse(a2)

    override def zero: Option[A] = None
  }

  def endoMonoid[A]: Monoid[A => A] = new Monoid[(A) => A] {
    override def op(f: (A) => A, g: (A) => A): (A) => A =
      x => f(g(x))

    override def zero: (A) => A = x => x
  }

  val wcMonoid: Monoid[WC] = new Monoid[WC] {
    override def op(a1: WC, a2: WC): WC = (a1, a2) match {
      case (Part(ls, w1, _), Part(_, w2, rs)) => Part(ls, w1 + w2 + 1, rs)
      case (Part(ls, w1, rs), Stub(r)) => Part(ls, w1, rs + r)
      case (Stub(l), Part(ls, w2, rs)) => Part(l + ls, w2, rs)
      case (Stub(l), Stub(r)) => Stub(l + r)
    }

    override def zero: WC = Stub("")
  }

  def _countWords(in: String) =
    foldMapV(in, wcMonoid)(c =>
      if (c == ' ') Part("", 0, "")
      else Stub(c.toString)
    ).count

  // ez implementation ;)
  def countWords(in: String) =
    in.par.count(_ == ' ') + 1


  def monoidLaws[A](m: Monoid[A], gen: Gen[A]): Prop =
    associativity(m)(gen) && transitivity(m)(gen)

  private def associativity[A](m: Monoid[A])(in: Gen[A]): Prop = forAll(in)(a =>
    m.op(m.zero, a) == a &&
      m.op(a, m.zero) == a
  )

  private def transitivity[A](m: Monoid[A])(in: Gen[A]) = forAll(Gen.listOfN(3, in))(list => {
    val (a, b, c) = (list.head, list(1), list(2))
    m.op(a, m.op(b, c)) == m.op(m.op(a, b), c)
  })

  def foldMap[A, B](as: List[A], m: Monoid[B])(f: A => B): B =
    as.map(f).foldLeft(m.zero)(m.op)

  def foldLeftViaFoldMap[A, B](z: B)(li: List[A], f: (A, B) => B): B =
    foldMap(li, endoMonoid[B])(f.curried)(z)


  def foldMapV[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    val len = v.length
    if (len == 1) f(v.head)
    else if (len == 0) m.zero
    else {
      val (l, r) = v.splitAt(len / 2)
      m.op(foldMapV(l, m)(f), foldMapV(r, m)(f))
    }
  }

  //  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] =
  //    foldMapV(v, par(m))(f andThen Par.unit)      //check this in answers

  def parFoldMap[A, B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): Par[B] = {
    Par.flatMap(Par.parMap(v)(f)) { bs =>
      foldMapV(bs, par(m))(b => Par.lazyUnit(b))
    }
  }

  def productMonoid[A, B](A: Monoid[A], B: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
    override def op(a1: (A, B), a2: (A, B)): (A, B) =
      A.op(a1._1, a2._1) -> B.op(a1._2, a2._2)

    override def zero: (A, B) = A.zero -> B.zero
  }

  def functionMonoid[A, B](B: Monoid[B]): Monoid[A => B] = new Monoid[(A) => B] {
    override def op(a1: (A) => B, a2: (A) => B): (A) => B = a => B.op(a1(a), a2(a))

    override def zero: (A) => B = a => B.zero
  }

  def bag[A](as: IndexedSeq[A]): Map[A, Int] =
    foldMapV[A, Map[A, Int]](as, mapMergeMonoid(intAddition))(a => Map(a -> 1))

  def mapMergeMonoid[K, V](V: Monoid[V]): Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    def zero = Map[K, V]()

    def op(a: Map[K, V], b: Map[K, V]) =
      (a.keySet ++ b.keySet).foldLeft(zero) { (acc, k) =>
        acc.updated(k, V.op(a.getOrElse(k, V.zero),
          b.getOrElse(k, V.zero)))
      }
  }

  def compare(a: Int, b: Int): Ordering = {
    if (a > b) Desc()
    else if (a < b) Asc()
    else Eq()
  }

  def isOrdered(seq: IndexedSeq[Int]): Ordering =
    foldMapV(seq.zip(seq.tail), ordMonoid) { case (t1: Int, t2: Int) => compare(t1, t2)}

  /**
   *
   * simple way to write 'almost' the same
   * if (a1 == a2) a1
   * else Unordered()
   */
  def ordMonoid = new Monoid[Ordering] {
    override def op(a1: Ordering, a2: Ordering): Ordering =
      (a1, a2) match {
        case (s1, s2) if s1 == s2 => s1
        case (s, Eq()) => s
        case (Eq(), s) => s
        case _ => Unordered()
      }

    override def zero: Ordering = Eq()
  }

  sealed trait Ordering

  case class Desc() extends Ordering

  case class Asc() extends Ordering

  case class Eq() extends Ordering

  case class Unordered() extends Ordering


}


object MonoidLaws {

  import functionalex.part2.Prop._

  def associativity[A](m: Monoid[A])(in: Gen[A]) = forAll(in)(a =>
    m.op(m.zero, a) == a &&
      m.op(a, m.zero) == a
  )

  //  def transitivity[A](m: Monoid[A])(in: Gen[A]) = forAll(in)(
  //
  //  =>
  //  m.op(a, m.op(b, c)) == m.op(m.op(a, b), c))
  //  )

}