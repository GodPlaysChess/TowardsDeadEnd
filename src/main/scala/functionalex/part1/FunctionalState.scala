package functionalex.part1

object FunctionalState {



  def main(args: Array[String]) {
//    ints(5)(new SimpleRNG(212l))._1.foreach(println)
  }


  def tupleMap[A, B, C](t: (A, B))(f: A => C): (C, B) =
    (f(t._1), t._2)


}
