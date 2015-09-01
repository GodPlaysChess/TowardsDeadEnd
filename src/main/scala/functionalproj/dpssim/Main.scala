package functionalproj.dpssim

import scalaz.NonEmptyList

object Main {

  def main(args: Array[String]) {
    val en = Enemy(100)
    val spells = NonEmptyList(ShadowBolt(), SearingPain())
//    val io1 = for {
//      _ <- IOActions.PrintLine(Simulator.findStrategy(en))
//    } yield ()
//    io1.run()

//    val io = for {
//      _ <- IOActions.PrintLine(Simulator.simulate(List(ShadowBolt(), ShadowBolt())).run(en))
//      _ <- IOActions.PrintLine(Simulator.simulate(List(SearingPain(), SearingPain())).run(en))
//    } yield ()
//    io.run()
//    println(Simulator.applySpell(ShadowBolt()).eval(en))
//    println(Simulator.simulate(List(ShadowBolt(), ShadowBolt())).run(en))
//    println(Simulator.simulate(List(SearingPain(), SearingPain())).run(en))

//    println(makeString(DpsSimSimple.findStrategy(en)))
//    println(makeString(FunctionalDpsSim.findStrategy(en)))

    println(makeString(DpsSimStates.findStrategy(en)))
    println(makeString(DpsSimStatesV2.findStrategy(en, spells)))
  }

  def makeString(s: Seq[Spell]): String =
    s.mkString(" -> ")

}

/*
  sealed trait WorldMonad[S] { // state thread s
    private def blocks : Array[Byte]

    def getBlockId(x:Int, y:Int) : ST[S, Byte]
    def setBlockId(x:Int, y:Int, b:Byte) : ST[S, WorldMonad[S]]
    def getEntity(entityId:Int) : ST[S, Entity]
  }

  // ST[S, A] is a "state transformer" roughly: S => (S, A)
  // transforms state indexed by type S, delivers type A

  object WorldMonad {
    def apply[S](blocks:Array[Byte]) = new WorldMonad[S] {
      val blocks = blocks
    } // called once at the start of game to construct the world
  }

  def updateEntity[S](entity:Entity) : WorldMonad[S] => WorldMonad[S] // ???
* */
