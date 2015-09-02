package algorithms.dynamic


class BitonicTour {
  // contains optimal solution for Key last points
  val optimalSubSolutions: scala.collection.mutable.Map[Int, (Seq[(Int, Int)], Seq[(Int, Int)])] = new scala.collection.mutable.HashMap
  // burteforce solution takes O(2^n)  (the task required to split seq into two, and there's 2^n-1 combinations of doing so)

  def shortestPath(points: IndexedSeq[(Int,Int)]): Seq[(Int, Int)] = {
    def sub(points: IndexedSeq[(Int, Int)]): Unit = {
      val pointsLeft = points.size
      if (pointsLeft == 2) {
        val path = Seq(points.head, points(1))
        optimalSubSolutions.update(2, path → path)
      } else if (!optimalSubSolutions.contains(pointsLeft)) {
        sub(points.tail)  // solve it without first point
        val (forth, back) = optimalSubSolutions(pointsLeft - 1) // the first point should belong to both sequences
        if (distancesCondition(points.head, forth.head, back.tail.head, forth.tail.head)) {
          optimalSubSolutions.update(pointsLeft, (points.head +: forth) → (points.head +: back.tail))
        } else {
          optimalSubSolutions.update(pointsLeft, (points.head +: forth.tail) → (points.head +: back))
        }
      }
    }
    sub(points)
    val (forth, back) = optimalSubSolutions(points.size)
    forth ++ back.reverse.tail
  }

  def distancesCondition (p1: (Int, Int), p2: (Int, Int), p3: (Int, Int), p4: (Int, Int)): Boolean = {
    distance(p1, p3) + distance(p2, p4) < distance(p1, p4) + distance(p2, p3)
  }

  def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
    Math.hypot(p1._1 - p2._1, p1._1 - p2._2)
  }
  
  def length(path: Seq[(Int, Int)]): Double = {
    val pp = path zip path.tail
    pp.foldLeft(0d)((d, p) ⇒ d + distance(p._1, p._2))
  }
}

object BionicTour {
  def main(args: Array[String]) {
    val input = Vector(1 → 1, 2 → 7, 3 → 4, 6 → 2, 7 → 6, 8 → 2, 9 → 5)
    val BT = new BitonicTour()
    val sol = BT.shortestPath(input)
    println(sol)
    println(BT.length(sol))
  }
  
  
}
