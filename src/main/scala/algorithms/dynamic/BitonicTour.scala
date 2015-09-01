package algorithms.dynamic


class BitonicTour {
  val optimalSubSolutions: scala.collection.mutable.Map[Int, (Seq[(Int, Int)], Seq[(Int, Int)])] = new scala.collection.mutable.HashMap
  // burteforce solution takes O(2^n)  (the task required to split seq into two, and there's 2^n-1 combinations of doing so)

  def shortestPath(points: IndexedSeq[(Int,Int)]): Seq[(Int, Int)] = {
    def sub(points: IndexedSeq[(Int, Int)], number: Int): Unit = {
      if (number == 2) {
        val path = Seq(points.head, points(1))
        optimalSubSolutions.update(2, path → path)
      } else if (!optimalSubSolutions.contains(number)) {
        sub(points.tail, number - 1)  // solve it without first point
        val (forth, back) = optimalSubSolutions(number - 1)
        if (distance(points.head, forth.head) > distance(points.head, back.head)) {
          optimalSubSolutions.update(number, (points.head +: forth.tail) → back)
        } else {
          optimalSubSolutions.update(number, forth → (points.head +: back.tail))
        }
      }
    }
    sub(points, points.size)
    val (forth, back) = optimalSubSolutions(points.size - 1)
    forth ++ back
  }

  def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
    Math.hypot(p1._1 - p2._1, p1._1 - p2._2)
  }
}

object BionicTour {
  def main(args: Array[String]) {
    val input = Vector(1 → 1, 2 → 7, 8 → 3, 9 → 6, 10 → 2, 11 → 5)
    val BT = new BitonicTour()
    println(BT.shortestPath(input))
  }
}
