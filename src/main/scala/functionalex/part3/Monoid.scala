package functionalex.part3

import functionalex.part2.Gen

trait Monoid[A] {
  def op(a1: A, a2: A): A

  def zero: A
}

object Monoids {
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
}


object MonoidLaws {

  import functionalex.part2.Prop._

  def associativity[A](m: Monoid[A])(in: Gen[A]) = forAll(in)(a =>
    m.op(m.zero, a) == a &&
      m.op(a, m.zero) == a
  )

  def transitivity[A](m: Monoid[A])(in: Gen[A]) = forAll(in)(

  =>
  m.op(a, m.op(b, c)) == m.op(m.op(a, b), c))
  )

}