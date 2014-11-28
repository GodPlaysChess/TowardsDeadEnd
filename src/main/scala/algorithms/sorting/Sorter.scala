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
    for (i <- toSort.length - 1 to 1 by -1) {
      for (j <- 1 to i) {
        if (toSort(j) < toSort(j - 1)) swap(j, j - 1, toSort)
      }
    }
    toSort
  }

  def mergesort(shuffled: Array[Int]): Array[Int] = {
    def merge(sortedOne: Array[Int], sortedAnother: Array[Int]): Array[Int] = {
      val result = new Array[Int](sortedOne.length + sortedAnother.length) //probably bad cause causes to create additional array
      var i = 0
      var j = 0
      while (i + j < result.length) {
        if (i < sortedOne.length && (j == sortedAnother.length || sortedOne(i) < sortedAnother(j))) {
          result(i + j) = sortedOne(i)
          i += 1
        } else {
          result(i + j) = sortedAnother(j)
          j += 1
        }
      }
      result
    }

    if (shuffled.length < 2) shuffled
    else {
      val (s1, s2) = shuffled.splitAt(shuffled.length / 2)
      merge(mergesort(s1), mergesort(s2))
    }
  }

  def heapsort(shuffled: Array[Int]): Array[Int] = {
    val heap = new MaxHeap(shuffled)
    heap.sort()
    heap.asArray
  }

  /**
   * Worst O(n&#94;2), average O(n * log n)
   */
  def quicksort(shuffled: List[Int]): List[Int] = {
    if (shuffled.size < 2) return shuffled
    val pivot = shuffled.last
    val (left, right) = shuffled.partition(_ < pivot)
    quicksort(left).:+(pivot) ::: quicksort(right.init)
  }

  // implemented in a low-level way
  def quicksort(shuffled: Array[Int]): Array[Int] = {
    def qs(shuffled: Array[Int], p: Int, r: Int): Array[Int] = {
      if (p < r) {
        val q = partition(shuffled, p, r)
        qs(shuffled, p, q - 1)
        qs(shuffled, q + 1, r)
      }
      shuffled
    }

    def partition(array: Array[Int], p: Int, r: Int): Int = {
      // for randomized quicksort, the pivot is chosen randomly, and swapped with array(r)
      val x = array(r)
      var i = p - 1
      for (j <- p to r - 1; if shuffled(j) <= x) {
        i += 1
        swap(i, j, shuffled)
      }
      swap(i + 1, r, shuffled)
      i + 1
    }

    qs(shuffled, 0, shuffled.length - 1)
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

  /**
   * Assuming that each of n input elements is an integer in a range of [0 - maxElement), where maxElement = O(n)
   * O(n) / O(n)
   */
  def countingSort(toSort: Array[Int], maxElements: Int): Array[Int] = {
    val length: Int = toSort.length
    val result = new Array[Int](length)
    val temp = new Array[Int](maxElements + 1) // temp now contains number of elements equal to i
    toSort.foreach(i => temp(i) += 1)
    (1 to maxElements).foreach(i => temp(i) += temp(i - 1)) // temp contains the number of elements <= i
    toSort.reverseIterator foreach (elem => {
      result(temp(elem) - 1) = elem
      temp(elem) = temp(elem) - 1
    })
    result
  }


}
