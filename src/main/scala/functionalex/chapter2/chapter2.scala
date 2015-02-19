package functionalex.chapter2

import scala.annotation.tailrec

object Chapter2 {

  /**
   * write tailrec fib
   */
  def fib(n: Int): Int = {
    @tailrec
    def go(f1: Int, acc: Int, count: Int): Int = {
      if (count < 0) return 0
      if (count == n) acc
      else go(acc, acc + f1, count + 1)
    }
    go(0, 1, n - 1)
  }

  // non tail rec
  def fib1(n: Int): Int = n match {
    case 1 => 0
    case 2 => 1
    case x => if (x < 0) -1 else fib1(n - 2) + fib1(n - 1)
  }

}
