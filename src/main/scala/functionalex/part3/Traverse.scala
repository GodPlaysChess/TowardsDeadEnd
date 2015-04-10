package functionalex.part3

import scala.collection.immutable.Nil

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>

  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

//  def reverse[A](F: F[A]): F[A] =
//    traverse()

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
    override def unit[A](a: => A): M = M.zero

    override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
  }

  type Const[A, B] = A
  type Id[A] = A

  val idMonad = new Monad[Id] {
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)

    override def unit[A](a: => A): Id[A] = a
  }
}

object ListTraverse extends Traverse[List] {
  override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
    as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
}

object OptionTraverse extends Traverse[Option] {
  override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
    oa match {
      case None => M.unit(None)
      case Some(m) => M.map(f(m))(Some(_))
    }
}
case class Tree[A](head: A, tail: List[Tree[A]])

object TreeTraverse extends Traverse[Tree] {
  override def traverse[M[_], A, B](ta: Tree[A])(f: A => M[B])(implicit M: Applicative[M]): M[Tree[B]] =
    ta.tail match {
      case Nil => M.map(f(ta.head))(Tree[B](_, List()))
      case ::(head, tl) => {
        val mtail: List[M[Tree[B]]] = ta.tail.map { t => traverse[M, A, B](t)(f) }
        val mhead: M[B] = f(ta.head)
        M.map2(mhead, ListTraverse.sequence(mtail))(Tree(_, _))
      }
    }

}
