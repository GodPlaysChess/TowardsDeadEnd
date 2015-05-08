package functionalex.part4

import scalaz._
import Scalaz._

case class Writer1[A, W](run: (A, W)) {
  implicit val M: Monoid[W] = ???
  
  def flatMap[B](a: A)(f: A => Writer1[B, W]): Writer1[B, W] =
    Writer1(f(run._1).run._1 -> (run._2 |+| f(run._1).run._2))

}

object WriterExamples {
  def applyLog[A, B, L](a: (A, L))(f: A => (B, L))(implicit m: Semigroup[L]): (B, L) =
      f(a._1).map(a._2 |+| _)
  
  def addDrink(price: Int): (Int, String) = price match {
    case 5 => price + 5 -> "beans"
    case _ => price + 7 -> " -> Bought milk"
  }
    

  def main(args: Array[String]) {
    val x = 3 -> " appending 3"
    println(applyLog(applyLog(x)(addDrink))(addDrink))
  }
}
