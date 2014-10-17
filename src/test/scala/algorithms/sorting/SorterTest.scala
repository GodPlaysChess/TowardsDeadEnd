package algorithms.sorting

import org.junit.{Test, Before}

/**
 * Created by GlebP on 17-Oct-2014.
 */
class SorterTest(var shuffled: Array[Int]) {

  @Before
  def setUp() = {
    shuffled = Array(1, 2, 3)
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

  private def assertSorted(ints: Array[Int]) =
    ints zip ints.tail foreach (pair => assert(pair._2 >= pair._1))

}
