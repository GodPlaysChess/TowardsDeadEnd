package projectEuler.unsolved

import projectEuler.utils.Problem

/**
 * Created by Gleb on 12/5/2014.
 */
object Problem75a extends Problem {

  override def solve(): Unit = {
    println((10 to 50).count(rightTrianglesForHyp))
  }

  private def rightTrianglesForHyp(hyp: Int): Boolean = findAllSquares(hyp).size == 1

  private def findAllSquares(hyp: Int): IndexedSeq[(Int, Int)] = for {
      i <- 1 to math.sqrt(hyp / 2).toInt
      tmp = math.sqrt(hyp - i * i).toInt
      if tmp * tmp == hyp - i * i
    } yield {
      (tmp * tmp - i * i, 2 * tmp * i)
    }

}
