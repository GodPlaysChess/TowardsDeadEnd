package functionalex.part1

object FunctionalState {


  def main(args: Array[String]) {
    val r = new SimpleRNG(212l)
    r.ints(0)._1.foreach(println)
    r.ints1(0)._1.foreach(println)
    r.ints1(5)._1.zip(r.ints(5)._1).foreach(println)
    println(r.nonNegativeLessThan(1)(r))

    println("machine state\n\n")
    val machine = new Machine(true, 5, 10)
    val inputs = List(Coin, Turn, Coin, Turn, Coin, Turn, Coin, Turn)
    println(machine.simulate(inputs))
    println(machine.simulateMachine(inputs).run(machine))
    println(Candy.simulateMachine(inputs).run(machine))
  }

}
