package projectEuler

/**
 * Created by GlebP on 05-Sep-2014.
 */
trait Problem {

  def main(args: Array[String]) {
      val start = System.currentTimeMillis()
      solve
      val finish = System.currentTimeMillis() - start
      println("took " + finish + " msec")
  }

  def solve
}
