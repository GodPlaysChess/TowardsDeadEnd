package functionalex.part1

/**
 * Created by Gleb on 2/26/2015.
 */
sealed trait Stream[+A] {

  def headOption: Option[A] = this match {
    case Empty => None
    case Cons(h, t) => Some(h())
  }

  def tailOption: Option[Stream[A]] = this match {
    case Empty => None
    case Cons(h, t) => Some(t())
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

  def take1(n: Int): Stream[A] =
    Stream.unfold((this, n))(seed => seed._1 match {
      case Cons(h, t) if seed._2 > 0 => Some((h(), (t(), n - 1)))
      case _ => None
    })

  def drop(n: Int): Stream[A] = this match {
    case Cons(h, t) if n != 1 => t().drop(n - 1)
    case Cons(h, t) => t()
    case Empty => Empty
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => Cons(h, () => t().takeWhile(p))
    case _ => Empty
  }

  def takeWhile2(p: A => Boolean): Stream[A] =
    Stream.unfold(this) {
      case Cons(h, t) if p(h()) => Some((h(), t()))
      case _ => None
    }

  def takeWhile1(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((el, str) =>
      if (p(el)) Cons(() => el, () => str)
      else Empty)

  def foldRight[B](z: => B)(f: (A, => B) => B): B = this match {
    case Cons(h, t) => f(h(), t().foldRight(z)(f))
    case _ => z
  }

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)(p(_) && _)


  // O(n) ?
  def headOption1: Option[A] =
    foldRight(None: Option[A])((el, str) => Some(el))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(Stream.empty[B])((el, str) => Stream.appendStream(f(el), str))

  def map[B](f: A => B): Stream[B] =
    foldRight(Stream.empty[B])((el, str) => Cons(() => f(el), () => str))

  def map1[B](f: A => B): Stream[B] =
    Stream.unfold(this) {
      case Cons(h, t) => Some((f(h()), t()))
      case Empty => None
    }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(Stream.empty[A])((el, str) =>
      if (p(el)) Cons(() => el, () => str)
      else str)

  def find(p: A => Boolean): Option[A] =
    filter(p).headOption

  def zipWith[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, that))(ab => {
      val a: Option[C] = ab._1.headOption.map2(ab._2.headOption)(f)
      val b: Option[(Stream[A], Stream[B])] = ab._1.tailOption.liftTuple(ab._2.tailOption)
      a.liftTuple(b)
    })

  /* with pattern matching */
  def zipWith1[B, C](that: Stream[B])(f: (A, B) => C): Stream[C] =
    Stream.unfold((this, that))(ab => ab._1 match {
      case Cons(ha, ta) => ab._2 match {
        case Cons(hb, tb) => Some((f(ha(), hb()), (ta(), tb())))
        case _ => None
      }
      case _ => None
    })


  def zipAll[B](that: Stream[B]): Stream[(Option[A], Option[B])] =
    Stream.unfold(this, that)(ab => Some((
      (ab._1.headOption, ab._2.headOption),
      ab._1.tailOption.liftTuple(ab._2.tailOption).getOrElse(Empty, Empty)
      ))
    )

  def startsWith[A](that: Stream[A]): Boolean = {
    (this zipWith that)((_, _)).forAll(x => x._1 == x._2)
  }

  def tails: Stream[Stream[A]] = {
    Stream.unfold(this) {
      case sa@Cons(_, t) => Some(sa, t())
      case _ => None
    }
  }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)(p(_) || _)

  def hasSubsequence[A](s: Stream[A]): Boolean =
    tails exists (_ startsWith s)

  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails map (_.foldRight(z)(f))

  //    def append[A](a: => A): Stream[A] = {
  //      val acc: Stream[A] = Stream.cons(a, Stream.empty)
  //      foldRight(acc)((el: () => A, z: Stream[A]) => Stream.cons[A](el, z))
  //    }
  //

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

  //lol, that I did not make it for Trait! TODO
  //  def append[A](a: => A, str: Stream[A]): Stream[A] =
  //    str.foldRight(cons[A](a, empty): Stream[A])(cons[A])
  //
  //  def appendStream[A](as: Stream[A], bs: Stream[A]): Stream[A] =
  //    as.foldRight(bs)(cons)

  def append[A](a: => A, str: Stream[A]): Stream[A] = {
    val acc: Stream[A] = Cons(() => a, () => Stream.empty[A])
    str.foldRight(acc)((el, z) => Cons(() => el, () => z))
  }

  def appendStream[A](as: Stream[A], bs: Stream[A]): Stream[A] =
    as.foldRight(bs)((el, z) => Cons(() => el, () => z))

  def constant[A](a: A): Stream[A] =
    cons(a, constant(a))

  def constant1[A](a: A): Stream[A] = unfold(a)(x => Some(x, x))

  def from(n: Int): Stream[Int] =
    cons(n, from(n + 1))

  def from1(n: Int): Stream[Int] = {
    lazy val s: Stream[Int] = cons(n, s.map(_ + 1))
    s
  }

  def from2(n: Int): Stream[Int] = unfold(n)(x => Some(x + 1, x + 1))

  def ones = constant1(1)

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = f(z) match {
    case None => Empty
    case Some(acc) => cons[A](acc._1, unfold(acc._2)(f))
  }

}