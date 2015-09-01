package datastructures


import scalaz.Maybe.Empty
import scalaz._
import Scalaz._

sealed abstract class FingerTree[T, A] {

  def tag(implicit M: Monoid[T]): T = this match {
    case Leaf(t, v) => t
    case Branch(t, left, right) => M.append(left.tag, right.tag)
  }

  def branch(tr: FingerTree[T, A])(implicit M: Monoid[T]): FingerTree[T, A] =
    Branch(M.append(tag, tr.tag), this, tr)

  def leaf(a: A)(measure: A => T): FingerTree[T, A] =
    Leaf(measure(a), a)

  def search(p: T => Boolean)(implicit M: Monoid[T]): Maybe[A] = {
    def go(t: T, tr: FingerTree[T, A]): A = tr match {
      case Leaf(_, v) => v
      case Branch(_, left, right) => p(t |+| left.measure) ? go(t, left) | go(M.zero |+| left.measure, right)
    }
    p(measure) ? go(M.zero, this).just | Empty()
  }

  def measure: T = this match {
    case Leaf(t, v) => t
    case Branch(t, left, right) => t
  }
}

case class Leaf[T, A](t: T, v: A) extends FingerTree[T, A]

case class Branch[T, A](t: T, left: FingerTree[T, A], right: FingerTree[T, A]) extends FingerTree[T, A]


object ExampleFingerTree extends App {
  //  winner t = search (== measure t)
  val ftree: FingerTree[Int, String] = Branch(5,
    Branch(2, Leaf(1, "a0"), Leaf(1, "ab1")),
    Branch(3, Leaf(1, "abc2"),
      Branch(2, Leaf(1, "abcde3"), Leaf(1, "abcdef4"))))

  val el: Maybe[String] = ftree.search(_ > 2)
  el println

  val list = 1 |-> 9
  val cond = None
//  val cond = 10.some
  val res = for {
    el <- list.filter(_ % 2 == 0)
    c <- cond
  } yield el + c.toString
  res println

}


