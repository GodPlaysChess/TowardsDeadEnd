package datastructures

import scalaz.Monoid

sealed trait FingerTree[T, A] {
  val M: Monoid[T]
  def measure: A => T

  def tag: T = this match {
    case Leaf(t, v) => t
    case Branch(t, left, right) => M.append(left.tag, right.tag)
  }

  def branch(tr: FingerTree[T, A]): FingerTree[T, A] =
    Branch(M.append(tag, tr.tag), this, tr)

  def leaf(a: A): FingerTree[T, A] =
    Leaf(measure(a), a)
}

case class Leaf[T, A](t: T, v: A) extends FingerTree[T, A]

case class Branch[T, A](t: T, left: FingerTree[T, A], right: FingerTree[T, A]) extends FingerTree[T, A]

