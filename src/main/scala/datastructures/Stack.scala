package datastructures

/**
 * Created by Gleb on 12/7/2014.
 * Based on Array
 */
class Stack[T](private val elems: Array[T]) {
  private var top = 0

  def isEmpty() = elems.isEmpty

  def push(elem: T) = {
    elems(top) = elem
    top += 1           // check for capacity here and increase the array of needed
  }

  def pop(): T = {
    if (isEmpty()) throw new Error("Underflow")
    else {
      top -= 1
      elems(top + 1)
    }
  }

}

/*object Stack {
  def apply[T](xs: T*): Stack[T] = {
    val array = new Array[T](xs.length)
    var i = 0
    for (x <- xs.iterator) {
      array(i) = x; i += 1
    }
    new Stack[T](array)
  }
}*/
