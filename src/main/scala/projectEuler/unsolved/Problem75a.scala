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


  private def adasd(table: List[List[String]]): List[(String, String)] = {
    table.tail.map(studentRow => forAStundent(table.head, studentRow)).flatten
  }

  private def forAStundent(topics: List[String], studendRow: List[String]): List[(String, String)] = {
    topics.zip(studendRow).tail.filter(topicmark => topicmark._2.toInt >= 5).map(_._1).map(topic => (studendRow.head, topic))
  }

  private def studentThema(studentThema: List[(String, String)], student: String, thema: String): Boolean =
    studentThema.filter(_._1 == student).filter(_._2 == thema).filter(_._2.toInt > 5).isEmpty

}
