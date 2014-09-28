package projectEuler.unsolved

object Problem111 {
  def main(args: Array[String]) {
    println(sumOfSFunctions(10))
    //println(sieveOfEratosthenes(math.pow(10, 3).toLong))
  }

  def countRepeatedDigits(number: Long, digit: Int): Int = number.toString.count(_ == (digit + 48).toChar)

  def mFunction(primeLength: Int, digit: Int) = getMapOfPrimesWithMaxDigitsNumber(primeLength, digit)._1

  def nFunction(primeLength: Int, digit: Int): Int = getMapOfPrimesWithMaxDigitsNumber(primeLength, digit)._2.size

  def sFunction(primeLength: Int, digit: Int): BigInt = getMapOfPrimesWithMaxDigitsNumber(primeLength, digit)._2.sum

  def getMapOfPrimesWithMaxDigitsNumber(primeLength: Int, digit: Int): (Int, List[Long]) = {
    getListOfPrimesWithLength(primeLength).groupBy(x => countRepeatedDigits(x, digit)).maxBy(_._1)
  }

  def sumOfSFunctions(primeLength: Int): BigInt = (0 to 9).map(x => sFunction(primeLength, x)).sum

  def getListOfPrimesWithLength(length: Int) = sieveOfEratosthenes(math.pow(10, length).toLong).toList

  //filter(x => x >= math.pow(10, length - 1) && x < math.pow(10, length)).toList

  def isPrime(l: Long): Boolean =
    if (l <= 1)
      false
    else if (l == 2)
      true
    else
      !(2l to (l - 1)).exists(x => l % x == 0)

  def sieveOfEratosthenes(nTo: Long) = {
    Stream.range(nTo/10, nTo).filter(x => isPrime(x))
  }


  def sieve(s: Stream[Int]): Stream[Long] = {
    s.head #:: sieve(s.tail.filter(_ % s.head != 0))
  }

}

