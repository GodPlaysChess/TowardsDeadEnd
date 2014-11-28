package algorithms.sorting

import org.junit.{Test, Before}

/**
 * Created by GlebP on 17-Oct-2014.
 */
class SorterTest() {

  var shuffled: Array[Int] = null

  @Before
  def setUp() = {
    shuffled = Array(2, 1, 3, 5, 7, 12, -2)
  }

  @Test
  def quicksort = {
    val sorted = Sorter.quicksort(shuffled.toList)
    assertSorted(sorted.toArray)
  }

  @Test
  def primiteivequicksort = {
    val sorted = Sorter.quicksort(shuffled)
    assertSorted(sorted.toArray)
  }

  @Test
  def mergeSort() = {
    val sorted = Sorter.mergesort(shuffled)
    assertSorted(sorted)
  }

  @Test
  def bubbleSort() = {
    val sorted = Sorter.bubblesort(shuffled)
    assertSorted(sorted)
  }

  @Test
  def insertSort() = {
    val sorted = Sorter.insertsort(shuffled)
    assertSorted(sorted)
  }

  @Test
  def heapSort() = {
    val sorted = Sorter.heapsort(shuffled)
    assertSorted(sorted)
  }

  @Test
  def countingSort() = {
    var sorted = Sorter.countingSort(Array(0, 2, 3, 5, 5, 1, 2, 4), 6)
    assertSorted(sorted)
    sorted = Sorter.countingSort(Array(2, 5, 3, 0, 2, 3, 0, 3), 5)
    assertSorted(sorted)
  }

  @Test
  def countingSortOnDigit() = {
    val sorted = Sorter.countingSortOnDigit(Array(10, 12, 13, 15, 15, 11, 12, 14), 1)
    assertSorted(sorted)
  }

  @Test
  def radixSort() = {
    val sorted = Sorter.radixSort(Array(329, 457, 657, 839, 436, 720, 355), 3)
    assertSorted(sorted)
  }

  private def assertSorted(ints: Array[Int]) =
    ints zip ints.tail foreach (pair => assert(pair._2 >= pair._1))

}
