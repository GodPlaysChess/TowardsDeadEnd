package functionalex.part2

import functionalex.part1.SimpleRNG

object TestClass {
  def main(args: Array[String]) {
    val r = new SimpleRNG(213l)
    val x = Gen.choose2(1, 100).sample
    println(x.run(r))
    println(Gen.string(5).sample.run(r))
//    println(Gen.listOfN())
  }

}
