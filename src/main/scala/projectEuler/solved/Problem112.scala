package projectEuler.solved

import projectEuler.utils.Problem

/**
 * Created by GlebP on 23-Dec-2014.
 */
object Problem112 extends Problem{

  override def solve(): Unit = {
    var bNums = 1
    var total = 101l
    val ratio = 99
    while (bNums * 100 != ratio * total) {
      total += 1
      if (bouncy(total)) bNums += 1
    }
    println(total)
  }

  def increasing(n: Long): Boolean =
    n.toString.zip(n.toString.tail).forall(twoChars => twoChars._1.toInt <= twoChars._2.toInt)

  def decreasing(n: Long): Boolean =
    n.toString.zip(n.toString.tail).forall(twoChars => twoChars._1.toInt >= twoChars._2.toInt)

  def bouncy(n: Long): Boolean = !increasing(n) && !decreasing(n)

}
