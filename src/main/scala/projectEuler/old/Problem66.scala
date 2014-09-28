package projectEuler.old

import scala.collection.mutable.ListBuffer


object Problem66 {
  val squares = List.tabulate(33)(x => x * x)

  def squareZ(root: Int) = math.sqrt(root).floor.toInt

  def sequence(number: Int, maxLength: Int): List[Int] = {
//    val cache = new mutable.ListBuffer[(Int, Int)]()
    val result: scala.collection.mutable.ListBuffer[Int] = ListBuffer(squareZ(number))
    var repr = new Repr(number, -squareZ(number), 1)
    def nextNumber(repr: Repr): (Int, Repr) = {
      val d = (repr.root - repr.addition * repr.addition) / repr.divisor
      val n = (squareZ(number) - repr.addition) / d
      val a = -repr.addition - n * d
      (n, new Repr(repr.root, a, d))
    }
    while (/*!cache.contains(repr.addition, repr.divisor) && */result.size < maxLength) {
  //    cache += Pair(repr.addition, repr.divisor)
      val pair = nextNumber(repr)
      result += pair._1
      repr = pair._2
    }
    result.toList
    //cache.size - cache.indexOf((repr.addition, repr.divisor))
  }

  def getSequenceValue(i: Int): Int =
    if (i == 1) 2
    else if (i % 3 == 0) 2 * i / 3
    else 1

  def calculateRationalFromConvergent(i: Int, sequence: List[Int]): BigRational = {
    def calculateRat(i: Int, n: BigRational): BigRational = {
      if (i == 0 || i >= sequence.size) n
      else calculateRat(i - 1, (n + sequence(i - 1)).inverse)
    }
    calculateRat(i - 1, new BigRational(1, sequence(i - 1)))
  }

  def solved(x: BigInt, y: BigInt, D: Int) = x * x - D * y * y == BigInt(1)

  // x^2 - D * y^2 = 1
  def solvePellEquation(D: Int): (BigInt, BigInt) = {
    var limit = 1
    var solution: BigRational = new BigRational(1, 1)
    while (!solved(solution.denom, solution.numer, D)) {
      limit += 1
      solution = calculateRationalFromConvergent(limit, sequence(D, limit))
    }
    (solution.denom, solution.numer)
  }


  def main(args: Array[String]) {
    var max: BigInt = 0
    var maxD: Int = 0
    for (d <- 1 to 10000) {
      if (!squares.contains(d)) {
        val x = solvePellEquation(d)._1
        if (x > max) { max = x; maxD = d}
        println(d + " -> " + x)
      }
    }
    println("Max = " + maxD)
  }


  class Repr(val root: Int, val addition: Int, val divisor: Int) {
    require(divisor > 0)
  }


}
