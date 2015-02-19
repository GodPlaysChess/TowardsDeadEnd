package projectEuler.unsolved

import projectEuler.utils.Problem

/**
 * Created by Gleb on 12/13/2014.
 * // 612220032 -> # 17
 */
object Problem119 extends Problem {

  override def solve(): Unit = {
    var c: Int = 16
    var num = BigInt(612220031)
    while (c < 30) {
      if (isRoot(num)) {
        println(num)
        c+=1
      }
      num += 1
    }
  }

  private def sum(number: BigInt): Int = {
    number.toString() map (_.toInt - 48) reduce(_ + _)
  }

  private def isRoot(number: BigInt): Boolean = {
    val base = sum(number)
    def isRoot1(b: BigInt): Boolean = {
      val b2 = if (b * b > number) b * base else b * b
      if (b2 == number) true
      else if (b2 > number) false
      else isRoot1(b2)
    }
    if (base == 0 || base == 1 || base % 10 == 7) false
    else isRoot1(base)
  }


}
