package algorithms.dynamic


class BitonicTour {
  // contains optimal solution for Key last points
  val optimalSubSolutions: scala.collection.mutable.Map[Int, (Seq[(Int, Int)], Seq[(Int, Int)])] = new scala.collection.mutable.HashMap
  // burteforce solution takes O(2^n)  (the task required to split seq into two, and there's 2^n-1 combinations of doing so)


  // DO implementation which takes O(n^2) time
  // we have N^2 recursions each of which takes const time
  def shortestPath(points: IndexedSeq[(Int,Int)]): Seq[(Int, Int)] = {
    def sub(points: IndexedSeq[(Int, Int)]): Unit = {
      val pointsLeft = points.size  // const
      if (pointsLeft == 2) {
        val path = Seq(points.head, points(1))
        optimalSubSolutions.update(2, path → path)  // const
      } else if (!optimalSubSolutions.contains(pointsLeft)) { //const
        sub(points.tail)  // solve it without first point o(x)
        val (forth, back) = optimalSubSolutions(pointsLeft - 1) // the first point should belong to both sequences
        if (distancesCondition(points.head, forth.head, forth.tail.head, back.tail.head)) { // const
          optimalSubSolutions.update(pointsLeft, (points.head +: forth.tail) → (points.head +: back)) // const
        } else {
          optimalSubSolutions.update(pointsLeft, (points.head +: forth) → (points.head +: back.tail))         //const
        }
      }
    }
    sub(points)
    val (forth, back) = optimalSubSolutions(points.size)
    forth ++ back.reverse.tail
  }

  def distancesCondition (newPoint: (Int, Int), oldHead: (Int, Int), firstForth: (Int, Int), firstBack: (Int, Int)): Boolean = {
    distance(newPoint, firstForth) + distance(oldHead, firstBack) < distance(newPoint, firstBack) + distance(oldHead, firstForth)
  }

  def distance(p1: (Int, Int), p2: (Int, Int)): Double = {
    Math.hypot(p1._1 - p2._1, p1._2 - p2._2)
  }
  
  def length(path: Seq[(Int, Int)]): Double = {
    val pp = path zip path.tail
    pp.foldLeft(0d)((d, p) ⇒ d + distance(p._1, p._2))
  }
}

object BionicTour {
  def main(args: Array[String]) {
    val input = Vector(1 → 1, 2 → 7, 3 → 4, 6 → 3, 7 → 6, 8 → 2, 9 → 5)
    val BT = new BitonicTour()
    val sol = BT.shortestPath(input)
    println(sol)
    println(BT.length(sol))
  }
  
  
}
