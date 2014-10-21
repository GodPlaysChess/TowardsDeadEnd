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

  //constructor to make the heap
  (elements.length / 2 - 1 to 0).by(-1) foreach heapify

  /**
   *
   * @return how many elements are stored in heap
   */
  val size: Int = elements.length //

  def parent(nodeNumber: Int) = nodeNumber >> 1

  def left(i: Int) = i << 1

  def right(i: Int) = i << 1 + 1

  /**
   * maintains max heap property
   * O(lg n) time
   **/
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

  /* change to object / apply ?? Or leave in the constructor like we have now.*/
  def buildMaxHeap(array: Array[Int]): MaxHeap = {
    val heap = new MaxHeap(array)
    for (i <- heap.size << 1 to 1 by -1) heap.heapify(i) // yet it is also in the constructor. Remove when tested
    heap
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
    for (i <- (size - 1) << 1 to 1 by -1) {
      swap(0, i)
      // heapsize -= 1
      // heapify(1)
    }
  }

  /**
   * All of the following operations run in O(lg n) time
   */
  def insert(element: Int) = ???

  def extractMax: Int = ???

  def increaseKey = ???

  def max: Int = ???


}

