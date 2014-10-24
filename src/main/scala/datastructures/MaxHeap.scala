package datastructures

import scala.annotation.tailrec

//TODO split into ArrayHeap and TreeHeap
/**
 * Created by Gleb Parakhosnkiy on 9/30/14.

 * Definition:
 * heap[parent(i)] >= heap[i]
 */
class MaxHeap(val elements: Array[Int]) {

  def asArray: Array[Int] = elements

  /**
   *
   * @return how many elements are stored in heap
   */
  var size: Int = elements.length

  /**
   * 0 <= size <= length
   *
   */
  val length: Int = elements.length

  //constructor to make the heap
  ((elements.length - 1) / 2)  to 0 by -1 foreach heapify


  def parent(nodeNumber: Int) = nodeNumber >> 1

  def left(i: Int) = i << 1

  def right(i: Int) = (i << 1) + 1

  /**
   * maintains max heap property of the chosen element.
   * basically stoke this element until all the elements above is higher
   * O(lg n) time
   */
  @tailrec
  private def heapify(index: Int): Unit = {
    val l = left(index)
    val r = right(index)
    var largest =
      if (l < size && elements(l) > elements(index)) l // l always < size
      else index
    if (r < size && elements(r) > elements(largest))
      largest = r
    if (largest != index) {
      swap(index, largest)
      heapify(largest)
    }
  }

  private def swap(i1: Int, i2: Int) = {
    val t = elements(i1)
    elements(i1) = elements(i2)
    elements(i2) = t
  }

  /**
   * O(n*lg n) - time, O(n) - space complexity
   */
  def sort() = {
    for (i <- (length - 1) to 1 by -1) {
      swap(0, i)
      size -= 1
      heapify(0)
    }
  }

  /**
   * All of the following operations run in O(lg n) time
   */
  def insert(element: Int) = ???

  def extractMax: Int = ???

  def increaseKey = ???

  def max: Int = ???

  private def show = println(elements.toList)


}

