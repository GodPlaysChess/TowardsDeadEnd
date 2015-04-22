package functionalproj.dpssim

object Main {

  def main(args: Array[String]) {
    val en = Enemy(100)
//    val io1 = for {
//      _ <- IOActions.PrintLine(Simulator.findStrategy(en))
//    } yield ()
//    io1.run()

//    val io = for {
//      _ <- IOActions.PrintLine(Simulator.simulate(List(ShadowBolt(), ShadowBolt())).run(en))
//      _ <- IOActions.PrintLine(Simulator.simulate(List(SearingPain(), SearingPain())).run(en))
//    } yield ()
//    io.run()
    println(Simulator.simulateSeq(List(ShadowBolt())).run(en -> List(SearingPain())))
    println(Simulator.simulateSeq1(List(ShadowBolt())).run(en))

//    println(Simulator.applySpell(ShadowBolt()).eval(en))
//    println(Simulator.simulate(List(ShadowBolt(), ShadowBolt())).run(en))
//    println(Simulator.simulate(List(SearingPain(), SearingPain())).run(en))
  }

}
