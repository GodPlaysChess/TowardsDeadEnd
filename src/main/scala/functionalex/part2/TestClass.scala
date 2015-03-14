package functionalex.part2

import functionalex.part1.{State, SimpleRNG}

object TestClass {
  def main(args: Array[String]) {
    val r = new SimpleRNG(212l)
    val gen: Gen[Int] = Gen(State.unit(5))
    println(gen.choose(1, 100))
  }

}
