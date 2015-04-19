package functionalex.part4

import functionalex.part2.Par.Par
import functionalex.part3.Monad

import scala.annotation.tailrec

sealed trait Free[F[_], A] {
  def map[B](f: A => B): Free[F, B] =
    flatMap[B](x => Return1(f(x)))

  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    FlatMap1(this, f)

}

case class Return1[F[_], A](a: A) extends Free[F, A]
case class Suspend1[F[_], A](s: F[A]) extends Free[F, A]
case class FlatMap1[F[_], A, B](fa: Free[F, A], f: A => Free[F, B]) extends Free[F, B]

object Free {
  type Async[A] = Free[Par, A]
  type TailRec[A] = Free[Function0, A]


  trait Translate[F[_], G[_]] {
    def apply[A](f: F[A]): G[A]
  }

  type ~>[F[_], G[_]] = Translate[F, G]

  def freeMonad[F[_]] = new Monad[({type f[x] = Free[F, x]})#f]() {
    override def flatMap[A, B](fa: Free[F, A])(f: A => Free[F, B]): Free[F, B] =
      fa flatMap f

    override def unit[A](a: => A): Free[F, A] =
      Return1(a)
  }

  @tailrec def step[F[_], A](fm: Free[F, A]): Free[F, A] = fm match {
    case FlatMap1(FlatMap1(x, f), g) => step(freeMonad.flatMap(x)(a => freeMonad.flatMap(f(a))(g)))
    case FlatMap1(Return1(x), f) => step(f(x))
    case _ => fm
  }

  def runFree[F[_], G[_], A](fm: Free[F, A])(t: F ~> G)(implicit G: Monad[G]): G[A] = step(fm) match {
    case Return1(a) => G.unit(a)
    case Suspend1(r) => t(r)
    case FlatMap1(x, f) => x match {
      case Suspend1(r) => G.flatMap(t(r))(a => runFree(f(a))(t))
      case _ => sys.error("Impossible, since `step` eliminates these cases")
    }
  }

  @tailrec
  def runTrampoline[A](a: Free[Function0, A]): A = a match {
    case Return1(a) => a
    case Suspend1(s) => s()
    case FlatMap1(fa, f) => fa match {
      case Return1(a) => runTrampoline { f(a) }
      case Suspend1(r) => runTrampoline { f(r()) }
      case FlatMap1(a0, g) => runTrampoline { a0 flatMap { a0 => g(a0) flatMap f } }
    }
  }
}


