package concurrency.akka.picalculation

import akka.actor.Actor

class Listener extends Actor {

  override def receive: Receive = {
    case PiApproximation(pi, duration) =>
      println("\n\tPi approximation: \t\t%s\n\tCalculation time: \t%s".format(pi, duration))
      context.system.shutdown()
  }
}
