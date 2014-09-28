package projectEuler.utils

/**
 * Created by Gleb on 9/7/14.
 * All convenience method are using throughout the PE Problems
 */
object UtilMethods {

  /**
  returns List of primes, which product results in a given integer
    */
  def primeDecompositionFor(n: Int): List[Int] = {
    def addToPrime(n: Int, primes: List[Int]): List[Int] = {
      if (n == 1) return primes
      if (n % 2 == 0) return addToPrime(n / 2, primes :+ 2)
      for (d <- 3 to math.sqrt(n).toInt by 2) {
        if (n % d == 0) return addToPrime(n / d, primes :+ d)
      }
      primes :+ n
    }
    addToPrime(n, List())
  }

  /**
   * greatest common divisor
   */
  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)


  def isPrime(n: Long) = {
    if (n <= 1)
      false
    else if (n == 2)
      true
    else
      !(2 to math.sqrt(n - 1).toInt).exists(x => n % x == 0)
  }

  def totient(number: Int): Int = {
    (2 to number).count(x => gcd(number,x) == 1)
  }

}
