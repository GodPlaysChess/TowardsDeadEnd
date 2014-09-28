package projectEuler.unsolved

import scala.collection.mutable.ArrayBuffer

object P104 {
  private val cache = ArrayBuffer[BigInt](1, 1)
  val fibs: Stream[BigInt] = BigInt(0) #:: BigInt(1) #:: fibs.zip(fibs.tail).map { n => n._1 + n._2}

  def findFibonacci(n: Int): BigInt =
    if (cache.size >= n) cache(n - 1)
    else if (cache.size == n - 1) {
      val x = cache(n - 2) + cache(n - 3)
      cache += x
      x
    }
    else {
      val x = findFibonacci(n - 1) + findFibonacci(n - 2)
      cache += x
      x
    }

  def checkProperty(n: String): Boolean =
    n.length > 9 && isPandigital(n.takeRight(9).toInt) && isPandigital(n.take(9).toInt)


  def isPandigital(number: Int): Boolean = {
    if (number % 9 != 0) return false
    val string = number.toString
    if (string.size != 9 || string.contains('0')) false
    else if (string.toCharArray.toSet.size == 9) true
    else false
  }

  def solve {
    var n = 0
    var number = BigInt(0)
    do {
      n += 1
      //number = fibs(n)
      number = findFibonacci(n)
      println(n)
    } while (!checkProperty(number.toString()))
    //    println(findFibonacci(541))
    //    println(isPandigital(123456789))
    //    println(isPandigital(123456782))
    //    println(isPandigital(123456709))
    //    println(isPandigital(103456989))
    //    println(isPandigital(1234567899))
    //    println(isPandigital(1234567890))
  }

}
