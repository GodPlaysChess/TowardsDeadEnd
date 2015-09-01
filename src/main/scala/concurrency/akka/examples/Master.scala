package concurrency.akka.examples

import akka.actor.{Props, Actor}
import akka.routing.RoundRobinRouter

class Master extends Actor {
  val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(100)), "workerRouter")
  private[this] var result = 0l
  private[this] val maxWorkers = 10
  private[this] var shutDownWorkers = 0

  override def receive: Receive = {
    case Calculate(list) ⇒ list.sliding(maxWorkers).foreach(workerRouter ! Work(_))
    case Result(l) ⇒ {
      result += l
      shutDownWorkers += 1
    }
  }
}


case class Calculate(bigList: Seq[Int])

