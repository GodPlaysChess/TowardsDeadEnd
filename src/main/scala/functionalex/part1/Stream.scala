package functionalex.part1

/**
 * Created by Gleb on 2/26/2015.
 */
sealed trait Stream[+A] {
  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  // not sure about infering the A type in h.
  def toList: List[A] = this match {
    case Empty => Nil
    case Cons(h: A, t: Stream[A]) => h :: t.toList
  }

  def take(n: Int): List[A] = this match {
    case Empty => Nil
    case Cons(h: A, t) => if (n == 0) Nil
    else h :: take(n - 1)
  }


  def drop(n: Int): Stream[A] = {
    case Empty => Nil
    case Cons(h: A, t) => if (n == 0) t
    else t.apply().drop(n - 1)
  }

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