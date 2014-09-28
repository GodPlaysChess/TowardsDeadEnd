package projectEuler

import UtilMethods.gcd

/**
 * Created by GlebP on 05-Sep-2014.
 * L - Answer
 * 100 - 11
 * 1500 - 161
 *
 */
object Problem75 extends Problem {

  val maxL = 1500000//0000 //00 //000//000

  override def solve: Unit = {
    val c = smartTraverseThrough()
    println("smart - " + c)
    //    println("bruteforce - " + bruteforce())
    //
  }

  // 100K - 16706 triangles 5429msec  // 161 for 1500L 184msec // 17320 150000L 13772msec
  def smartTraverseThrough(): Long = {
    /* Array is Length -> Amount of Right triangles could be build with such length wire */
    val seq: Array[Int] = Array.fill(maxL)(0)
    //    val seq = s.par
    for (l <- 1 to maxL) {
      if (seq(l - 1) < 2) {
        val a = findABforL(l, seq(l - 1)) // probably the bottleneck, cause we don't care about the value of A
        if (a > 0) {
          (l to maxL).by(l).foreach(i => seq(i - 1) += a)
        }
      }
    println(l)

    }
    val c = seq.par.count(_ == 1)
    c
  }

  def findABforL(l: Int, known: Int): Int = {
    var diff = known
//    val bees = scala.collection.mutable.Set[Int]()
    for (a <- 1 to l / 3) {
      //      if ((l * l - 2 * a * l) % (2 * l - 2 * a) == 0) diff += 1
      /* a-b formed right triangle */
      if ((l * l - 2 * a * l) % (2 * l - 2 * a) == 0) {
        val b = (l * l - 2 * a * l) / (2 * l - 2 * a)
        /* this is for taking in account that we're not considering factorized a-b tuples */
        if (gcd(a, b) == 1 && a < b) {
          /* and this is for being sure that we're not considering both (a, b) and (b, a) pairs */
//          if (!bees.contains(a)) {
//            bees += b
            diff += 1
//          }
        }
      }
    }
    diff - known
  }

  //bruteforce 1835msec for 1500L. Answer: 161.
  // This is auxilarry algorithm to produce answers for small L for verification
  def bruteforce(): Int = {
    val q = for {
      l <- 3 to maxL
      c <- l / 3 to l / 2
      a <- 1 until c
      b = l - a - c
      if b <= a && a * a + b * b == c * c
    } yield {
      //      printf(" %d = %d + %d + %d \n ", l, c, a, b)
      l
    }
    //    println(q)
    q.groupBy(x => x).count(entry => entry._2.size == 1)
  }

  def onlyOneTriangle(l: Int): Boolean = {
    var count = 0
    for {
      c <- l / 3 to l / 2
      a <- 1 until c
      b = l - a - c
      if b <= a && a * a + b * b == c * c
    } yield {
      printf(" %d = %d + %d + %d \n ", l, c, a, b)
      if (count == 1) return false
      count += 1
    }
    count == 1
  }

  //1663msec 161 for 1500
  def anotherBruteforce(): Int = {
    (4 to maxL).count(onlyOneTriangle)
  }

  //  def g: Int = {
  //    (3 to maxL).map(l => (1 to l / 3).flatMap(a => (1 to l / 3).flatMap(b => (1 to l / 3).filter(a * a + b * b == (l - a - b) * (l - a - b))))).count(x => x.size == 1)
  //  }


}
