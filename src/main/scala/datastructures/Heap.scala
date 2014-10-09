package datastructures

import scala.annotation.tailrec

//TODO split into ArrayHeap and TreeHeap
/**
 * Created by Gleb Parakhosnkiy on 9/30/14.

 * Definition:
 * heap[parent(i)] >= heap[i]
 */
class Heap(val heap: Array[Int]) {

  //constructor to make the heap
  (heap.length / 2 to 1).by(-1) foreach heapify

  /**
   *
   * @return how many elements are stored in heap
   */
  private var size: Int = heap.length //

  def parent(nodeNumber: Int) = heap(nodeNumber >> 1)

  def left(i: Int) = heap(i << 1)

  def right(i: Int) = heap(i << 1 + 1)

  /**
   * maintains heap property
   * O(lg n) time
   **/
  @tailrec
  private def heapify(index: Int): Unit = {
    val l = left(index)
    val r = right(index)
    var largest =
      if (l < size && heap(l) > heap(index)) l
      else index
    if (r < size && heap(r) > heap(largest))
      largest = r
    if (largest != index) {
      swap(index, largest)
      heapify(largest)
    }
  }

  private def swap(i1: Int, i2: Int) = {
    val t = heap(i1)
    heap(i1) = heap(i2)
    heap(i2) = t
  }


  /**
   * O(n*lg n) - time, O(n) - space complexity
   */
  def sort() = {

  }

  /**
   * All of the following operations run in O(lg n) time
   */
  def insert(element: Int) = ???

  def extractMax: Int = ???

  def increaseKey = ???

  def max: Int = ???


}

