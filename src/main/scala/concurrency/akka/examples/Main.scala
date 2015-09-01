package concurrency.akka.examples

import akka.actor.{ActorSystem, Props}
import scalaz._
import Scalaz._

/**
 * First goal is to do simple distributed computations.
 * Also worth to do it with scalaz.concurrent.Task.
 */
object Main extends App {
  val system = ActorSystem("Distributedsum")
  val master = system.actorOf(Props[Master], name = "Master")
  val biglist = 1 |-> 100
  val res = master ! Calculate(biglist)
  println(res)

}

