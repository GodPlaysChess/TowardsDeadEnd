package functionalex.part1

/**
 * Created by Gleb on 2/26/2015.
 */
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => h() :: t().toList
  }

  def take(n: Int): List[A] = this match {
    case Empty => Nil
    case Cons(h, t) => if (n == 0) Nil
    else h() :: t().take(n - 1)
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 1 => t().drop(n - 1)
    case Cons(h, t) => t()
    case Empty => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)

  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((el, str) =>
      if (p(el)) Cons(() => el, () => str)
      else Empty)

  def headOption1: Option[A] =
    foldRight(None: Option[A])((el, str) => Some(el))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((el, str) => Cons(() => f(el), () => str))


  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((el, str) =>
      if (p(el)) Cons(() => el, () => str)
      else str)



}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))

}