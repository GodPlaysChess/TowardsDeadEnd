package projectEuler.old

object Solution {
  def main(args: Array[String]) {
    val start = System.currentTimeMillis()
    val problem = new P104
    problem.solve
    val finish = System.currentTimeMillis() - start
    println("took " + finish + " msec")
  }
}
