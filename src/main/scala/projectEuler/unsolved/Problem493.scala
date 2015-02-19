package projectEuler.unsolved

import projectEuler.utils.Problem
import scala.collection.immutable.HashMap

/**
 * Created by Gleb on 12/13/2014.
 */
object Problem493 extends Problem {

  val TOTAL_BALLS = 70
  val EACH_BALL = 10
  val DRAWN_BALLS = 2

  val TOTAL_COMB: BigInt = (TOTAL_BALLS - DRAWN_BALLS + 1 to TOTAL_BALLS).product

  override def solve(): Unit = {
    var probMap: Map[Int, Double] = HashMap[Int, Double]()
    //    for (ballNumber <- 2 to howManyWeTake) { // assuming one ball is on the table.
    //      val probabilityMap: Map[Int, Double] = findProbabilityDistribution(ballNumber - 1, probMap) //then balls, laying on the table are one less then we've taken.
    //      probMap = probabilityMap
    //    }
    for (dcolors <- 1 to 7) {
      probMap += dcolors -> probabilityHaving(dcolors)
    }
    val answer = probMap.map(kv => kv._2 * kv._1).reduce(_ + _)
    println(answer)
    "hello;sveta".split(';')

  }

  def probabilityHaving(distinctColors: Int): Double = {
    val comb1: BigInt = (1 until distinctColors).map(x => BigInt(TOTAL_BALLS - x + 1)).product
    val rest = TOTAL_BALLS - distinctColors * EACH_BALL
    val comb2 = (1 to (DRAWN_BALLS - distinctColors)).map(x => BigInt(rest - x + 1)).product
    (BigDecimal(comb1) * BigDecimal(comb2) / BigDecimal(TOTAL_COMB)).toDouble
  }


  /*

    def findProbabilityDistribution(amountOfBallsOnTable: Int, prevDistribution: Map[Int, Double]): Map[Int, Double] = {
      val nextDistribution = scala.collection.mutable.Map[Int, Double]()
      for (distinctColorsOnTable <- 1 to 7) {
        val dcolorsOnTable = if (distinctColorsOnTable < 8) distinctColorsOnTable else 7
        val otherColoredBalls = if (TOTAL_BALLS - dcolorsOnTable * 10 > 0) TOTAL_BALLS - dcolorsOnTable * 10 else 0
        val pSameColors = prevDistribution.getOrElse(dcolorsOnTable, 0d)
        val pOneLessColor = prevDistribution.getOrElse(dcolorsOnTable - 1, 0d)
        val prob: Double = probabilityHaving(otherColoredBalls, TOTAL_BALLS - amountOfBallsOnTable, pOneLessColor, pSameColors)
        if (nextDistribution.contains(dcolorsOnTable)) nextDistribution(dcolorsOnTable) += prob
        else nextDistribution += dcolorsOnTable -> prob
      }
      // very ugly, but fastfix for checking the probability of having the ball of completely distinct color
      /*

       val otherColoredBalls = if (TOTAL_BALLS - amountOfBallsOnTable * 10 > 0) TOTAL_BALLS - amountOfBallsOnTable * 10 else 0
       val pSameColors = prevDistribution.getOrElse(amountOfBallsOnTable + 1, 0d)
       val pOneLessColor = prevDistribution.getOrElse(amountOfBallsOnTable, 0d)

       */
      //    nextDistribution += (amountOfBallsOnTable + 1) -> probabilityHaving(otherColoredBalls, TOTAL_BALLS - amountOfBallsOnTable, pOneLessColor, pSameColors) // probability, that number of colors increases, i.e the ball in hand is of a different color
      nextDistribution += 0 -> 0
      nextDistribution.toMap
    }

    def probabilityHaving(otherColoredBalls: Int, totalBalls: Int, pOneLessColor: Double, pSameColors: Double): Double = {
      val pToTakeDifferentBall: Double = otherColoredBalls.toDouble / totalBalls
      val pToTakeSameBall: Double = (totalBalls - otherColoredBalls).toDouble / totalBalls
      val pResult = pToTakeDifferentBall * pOneLessColor + pToTakeSameBall * pSameColors
      pResult
    }
  */


}
