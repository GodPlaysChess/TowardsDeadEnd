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
  def mergeSort() = {
    val sorted = Sorter.mergesort(shuffled)
    assertSorted(sorted)
  }

  @Test
  def bubbleSort() = {
    val sorted = Sorter.bubblesort(shuffled)
    println(sorted.toList)
    assertSorted(sorted)
  }

  @Test
  def insertSort() = {
    val sorted = Sorter.insertsort(shuffled)
    print(sorted.toList)
    assertSorted(sorted)
  }

  private def assertSorted(ints: Array[Int]) =
    ints zip ints.tail foreach (pair => assert(pair._2 >= pair._1))

}
