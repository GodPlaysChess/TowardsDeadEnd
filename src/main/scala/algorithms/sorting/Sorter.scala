package algorithms.sorting

/**
 * Various sort algorithms.
 * For initial simplicity, all of them are sorting array of Integers.
 * Of course this could be easily changed to Collection[Comparable] but
 * the goal is not to build fancy library, but learn and compare certain
 * algorithms
 *
 */
object Sorter {

  /**
   * O(N&#94;2) / O(N)
   * fuck, this is boring :).
   * Probably I'll just skip this part and start with more sophisticated algorithms ;)
   */
  def insertsort(ints: Array[Int]): Array[Int] = {
    val result = new Array[Int](ints.size)
    while (ints.size > 0) {
      val max = findMax(ints)
      result.update(ints.size - 1, max._2)
      ints diff Array(max._1)
    }


  }

  def bubblesort(ints: Array[Int]): Array[Int] = ???


  def heapsort(shuffled: Array[Int]): Array[Int] = {
    ???
  }

  def mergesort(shuffled: Array[Int]): Array[Int] = {
    ???
  }

  // O(N) / O(1)
  private def findMax(ints: Array[Int]): Int = {
    ints.reduce((x, y) => if (x >= y) x else y)
  }

  private def findMin(ints: Array[Int]): (Int, Int) = {
    var min = (ints(0), 0)
    for (i <- 1 until ints.size) {
      min = if (ints(i) < min._1) (ints(i), i) else min
    }
    min
  }

}
