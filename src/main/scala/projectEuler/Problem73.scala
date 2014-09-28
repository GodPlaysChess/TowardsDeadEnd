package projectEuler

import projectEuler.old.BigRational

/**
 * First solutions with creating set of BigRationals was taken 102309 msec
 * This solution is taking 932 msec (100 times faster)
 * Answer is 7295372
 * Solved 03.09.2014
 */
object Problem73 {
  val lowLimit = new BigRational(1, 3)
  val hiLimit = new BigRational(1, 2)
  val maxD = 12000

  def highLimitNumeratorForD(d: Int): Int = (d - 1) / 2

  def lowLimitNumeratorForD(d: Int): Int = d / 3 + 1

  def fractionsInRange(d: Int): Int = {
    val lowLimNum = lowLimitNumeratorForD(d)
    val hiLimNum = highLimitNumeratorForD(d)
    if (lowLimNum > hiLimNum) 0
    else (lowLimNum to hiLimNum).count(x => gcd(x, d) == 1)
  }

  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)

  def solve() = {
    val size = (1 to maxD).map(fractionsInRange).sum
    println(size)
  }

  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    solve()
    val finish = System.currentTimeMillis() - start
    println("took " + finish + " msec")
  }


}
