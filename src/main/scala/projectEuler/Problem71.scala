package projectEuler

import projectEuler.old.BigRational

/**
 *   428570
 */
object Problem71 {
  val rightLimit = new BigRational(3, 7)
  val maxD =  1000000

  def highLimitNumeratorForD(d: Int): Int = 3 * d / 7

  def goDown(num: Int, d: Int): BigRational = {
    if (d != 7 && num != 0) {
      var fraction = new BigRational(num, d)
      while (d != fraction.denom.toInt && fraction.numer >= 1) {
        fraction = new BigRational(fraction.numer - 1, d)
      }
      return fraction
    }
    new BigRational(1, 7)
  }

  def closestFraction(d: Int): BigRational = {
    val num = highLimitNumeratorForD(d)
    goDown(num, d)
  }

  def solve() = {
    val closest: BigRational = (1 to maxD map closestFraction).reduceLeft((x, y) => x.max(y))
    println(closest)
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    solve()
    val finish = System.currentTimeMillis() - start
    println("took " + finish + " msec")
  }


}
