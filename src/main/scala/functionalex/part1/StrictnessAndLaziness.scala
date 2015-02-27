package functionalex.part1

/**
 * Created by Gleb on 2/26/2015.
 */
object StrictnessAndLaziness {

  def main(args: Array[String]) {
    val as = Stream(1, 2, 3, 4)
    println(as.take(2).toList)
    println(as)
    println(as.toList)
    println("should be List(2,3,4):  " + as.drop(1).toList)
    println(as.takeWhile1(_ < 3).toList)
    println(as.headOption)
  }
}
