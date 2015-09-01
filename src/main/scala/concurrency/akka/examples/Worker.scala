package concurrency.akka.examples

import akka.actor.Actor

class Worker extends Actor {
  override def receive: Receive = {
    case Work(list) ⇒ sender ! Result(list.sum)
  }
}

case class Work(list: Seq[Int])

case class Result(res: Long)
