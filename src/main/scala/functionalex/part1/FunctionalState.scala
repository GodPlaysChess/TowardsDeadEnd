package functionalex.part1

object FunctionalState {


  def main(args: Array[String]) {
    val r = new SimpleRNG(212l)
    r.ints(0)._1.foreach(println)
    r.ints1(0)._1.foreach(println)

    r.ints1(5)._1.zip(r.ints(5)._1).foreach(println)

  }


  def tupleMap[A, B, C](t: (A, B))(f: A => C): (C, B) =
    (f(t._1), t._2)


}
