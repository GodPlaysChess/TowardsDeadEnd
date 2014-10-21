package algorithms.sorting

import datastructures.MaxHeap

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
   * O(N&#94;2) / O(1)
   */
  def insertsort(toSort: Array[Int]): Array[Int] = {
    for (i <- 1 to toSort.length - 1) {
      var j = i
      while (j > 0 && toSort(j - 1) > toSort(j)) {
        swap(j, j - 1, toSort)
        j -= 1
      }
    }
    toSort
  }

  /**
   * O(N&#94;2) / O(1)
   */
  def bubblesort(toSort: Array[Int]): Array[Int] = {
    for (i <- toSort.length - 1  to 1 by -1) {
      for (j <- 1 to i) {
        if (toSort(j) < toSort(j - 1)) swap(j, j - 1, toSort)
      }
    }
    toSort
  }

  def mergesort(shuffled: Array[Int]): Array[Int] = {
    ???
  }


  def heapsort(shuffled: Array[Int]): Array[Int] = {
    val heap = new MaxHeap(shuffled)
    heap.sort()
    heap.asArray
  }


  private def swap(i: Int, j: Int, array: Array[Int]): Unit = {
    val t = array(i)
    array(i) = array(j)
    array(j) = t
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
