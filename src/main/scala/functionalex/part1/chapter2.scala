package functionalex.part1

import scala.annotation.tailrec

object Chapter2 {

  /**
   * write tailrec fib
   */
  def fib(n: Int): Int = {
    @tailrec
    def go(f2: Int, f1: Int, count: Int): Int = {
      if (count < 0) -1
      else if (count == n) f1
      else go(f1, f1 + f2, count + 1)
    }
    go(0, 1, 1)
  }

  // non tail rec
  def fib1(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case x => if (x < 0) -1 else fib1(n - 2) + fib1(n - 1)
  }

  def isSorted[A](as: Array[A], ordered: (A, A) => Boolean): Boolean = {
    (as zip as.tail).forall(p => ordered(p._1, p._2))
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    (b: B) => f(a, b)
  }

  def curry[A, B, C](f: (A, B) => C): A => (B => C) = {
    (a: A) => (b: B) => f(a, b) //don't forget about the right associativity of '=>' operator
  }

  def uncurry[A, B, C](f: => A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    (a: A) => f(g(a))
  }

  def main(args: Array[String]) {
    val f = (x: Double) => 1 + x
    val g = (x: Double) => x.toString
    println(compose(g, f)(1))
  }

}
