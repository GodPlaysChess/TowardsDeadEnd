package functionalex.part2

import functionalex.part2.Prop._

object TestClass {
  def main(args: Array[String]) {
    //    val r = new SimpleRNG(213l)
    //    val x = Gen.choose2(1, 100).sample
    //    println(x.run(r))
    //    println(Gen.string(5).sample.run(r))
    //    println(Gen.listOfN())
    val smallInt = Gen.choose(-10, 10)
    val maxProp = forAll(Gen.listOf1(smallInt)) { ns =>
      val max = ns.max
      !ns.exists(_ > max)
    }
    run(maxProp)

    val sortedProp = forAll(Gen.listOf1(smallInt)) { ns =>
      val min = ns.sorted.head
      !ns.exists(_ < min)
    } && forAll(Gen.listOf1(smallInt)) { ns =>
      val zipped = ns.sorted.zip(ns.sorted.tail)
      zipped.forall { case (a, b) => a >= b }
    }
  }

}
