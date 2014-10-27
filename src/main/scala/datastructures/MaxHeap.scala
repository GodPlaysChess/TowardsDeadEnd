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
  ((elements.length - 1) / 2) to 0 by -1 foreach heapify


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

  def insert(element: Int) = {
    size += 1
    elements(size) = Int.MinValue
    increaseKey(size, element)
  }

  // O(lg(n))
  def extractMax: Int = {
    if (size < 1) throw new Error("heap underflow")
    val max = elements(0)
    elements(0) = elements.last
    size -= 1
    heapify(0)
    max
  }

  def increaseKey(i: Int, elem: Int) = {
    if (elem < elements(i)) throw new Error("heap underflow")
    elements(i) = elem
    var k = i
    while (k > 0 && elements(parent(k)) < elements(k)) {
      swap(k, parent(k))
      k = parent(k)
    }
  }

  // O(1)
  def max: Int = elements(0)

  override def toString = elements.mkString(" ")
}

