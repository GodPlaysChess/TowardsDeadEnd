package algorithms.sorting

/**
 * Created by Gleb on 11/29/2014.
 */
object OrderStatistics {

  def minAndMax[T <% Ordered[T]](list: List[T]): (T, T) = {
    if (list.isEmpty) throw new Error("List is empty")
    minAndMax(list, list(0), list(0))
  }

  /**
   * returns min element and max element from a list, using 3*n/2 comparisons
   */
  private def minAndMax[T <% Ordered[T]](list: List[T], min: T, max: T): (T, T) = list match {
    case Nil => (min, max)
    case x :: Nil => if (x < min) (x, max) else if (x > max) (min, x) else (min, max)
    case x :: y :: xs =>
      val (maxXY, minXY) = if (x > y) (x, y) else (y, x)
      val a = if (maxXY > max) maxXY else max
      val i = if (minXY < min) minXY else min
      minAndMax(xs, a, i)
  }

  def randomizedSelect[T <% Ordered[T]](array: Array[T], i: Int): T = {
    randomizedSelect(array, 0, array.length, i)
  }

  private def randomizedSelect[T <% Ordered[T]](array: Array[T], leftBorder: Int, rightBorder: Int, index: Int): T =
    if (leftBorder == rightBorder) array(rightBorder)
    else {
      val q: Int = randomizedPartition(array, leftBorder, rightBorder)
      val k = q - leftBorder + 1
      if (index == k) array(q)
      else if (index < k) randomizedSelect(array, leftBorder, q - 1, index)
      else randomizedSelect(array, q + 1, rightBorder, index - k)

    }

  private def randomizedPartition[T <% Ordered[T]](array: Array[T], left: Int, right: Int): Int = {
    //partition from sorts
    ???
  }


  /**
   * Returns a median of an array in O(n)
   * For sake of clarity additional O(n) space is used, but of course can be performed in place/
   * @tparam T
   * @return
   */

  def med[T <% Ordered[T]](list: List[T]): T = {
    val x = medianOfMedians(list)
    val (smaller, bigger) = list.partition(el => el <= x)
    if (smaller.length == bigger.length) x
    else if (smaller.length < bigger.length) ithOrderStatistics(bigger, list.length / 2 - smaller.length)
    else ithOrderStatistics(smaller, list.length / 2)
  }

  def medianOfMedians[T <% Ordered[T]](list: List[T]): T = list match {
    case x :: Nil => x
    case _ =>
      val res = splitBy5(list, List[List[T]]()).map(l => smallMedian(l))
      medianOfMedians(res)
  }

  private def splitBy5[T <% Ordered[T]](list: List[T], acc: List[List[T]]): List[List[T]] = list match {
    case Nil => acc
    case _ => splitBy5(list.drop(5), list.take(5) +: acc)
  }

  private def smallMedian[T <% Ordered[T]](list: List[T]): T = {
    val sorted = list.sorted
    sorted.drop(sorted.length / 2).head
  }

  def ithOrderStatistics[T <% Ordered[T]](list: List[T], i: Int): T = {
    val (smaller, bigger) = list.partition(el => el <= list.head)
    if (smaller.length == i + 1) list.head
    else if (i < smaller.length) ithOrderStatistics(smaller, i)
    else ithOrderStatistics(bigger, i - smaller.length)
  }

}
