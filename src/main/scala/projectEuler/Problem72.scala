package projectEuler


import scala.collection.mutable.{ArrayBuffer, Map, Set}

import UtilMethods._

object Problem72 extends Problem {
  val maxD = 1000000
  val primeDecompositions: Map[Int, scala.collection.immutable.Set[Int]] = Map[Int, scala.collection.immutable.Set[Int]]()
  //cache them first
  lazy val dmap: Map[Int, Set[Int]] = (2 to maxD).foldLeft(Map[Int, Set[Int]]())((map, x) => map += x -> Set[Int]())

  def solve() = {
    val c = speedOfLight()
    println(c)
  }

  // brute force - 3673 -> 2327 for par msec for 10K | 425941msec for 100K
  def count(): Int = {
    (2 to maxD)./:(0)((acc, d) => acc + (1 until d).par.count(n => gcd(n, d) == 1))
  }

  //3028msec for 10K bad for higer D cause isPrime is rather expensive operation
  def supersmartcount = {
    var counter = 0
    for (d <- 2 to maxD) {
      if (isPrime(d)) counter += d - 1
      else {
        for (n <- 1 until d) {
          if (gcd(n, d) == 1) counter += 1
        }
      }
      println(d)
    }
    counter
  }


  //2251 msec 10K  |  221795 msec 100K -> 101735 msec == 190745 is use contains-put
  def ultrasupersmartcount = {
    var counter: BigInt = 0
    for (d <- 2 to maxD) {
      println(d)
      val div: scala.collection.immutable.Set[Int] = {
        if (primeDecompositions.contains(d)) primeDecompositions.get(d).get
        else {
          val set = primeDecompositionFor(d).toSet
          fillMap(d, set)
          set
        }
      }
      counter += (d - 1) - (1 until d).par.count(x => div.exists(m => x % m == 0))
    }
    counter
  }

  private def fillMap(d: Int, set: scala.collection.immutable.Set[Int]) = {
    (2 to maxD / d).foreach(mult => if (!primeDecompositions.contains(mult * d)) primeDecompositions.put(mult * d, set + mult))
  }

  // 1747 for 10K    77575 for 100K
  // SOLVED! 303963552391 took 7885890 msec ~2hrs
  def walkThroughDCount(): BigInt = {
    var counter: BigInt = 0
    //    for ((d, factors) <- dmap) {
    for (d <- 2 to maxD) {
      val factors = dmap(d)
      if (factors.isEmpty) {
        counter += d - 1
        (2 * d to maxD).by(d).foreach(fac => dmap(fac) += d)
      } else {
        counter += (d - 1) - (1 until d).par.count(x => factors.exists(m => x % m == 0))
      }
      println(d)
    }
    counter
  }

  //calculate the sum of totient fucntions 207 msec
  def speedOfLight(): BigInt = {
    //initializing array, where the value is totient function
    val phis = (2 to maxD).toBuffer

    for (n <- 2 to maxD) {
      if (phis(n - 2) == n) {
        (n to maxD).by(n).foreach(m => phis(m - 2) -= phis(m - 2) / n)
      }
    }

    val counter: BigInt = 0
    phis.foldLeft(counter)(_ + _)
  }


  def gcd(a: Int, b: Int): Int =
    if (b == 0) a else gcd(b, a % b)


}
