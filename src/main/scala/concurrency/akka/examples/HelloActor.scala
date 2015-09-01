package concurrency.akka.examples

import akka.actor.{Props, ActorSystem, Actor}

class HelloActor extends Actor {

  override def receive: Receive = {
    case "hello" ⇒ println("Hello back!")
    case _ ⇒ println("what ?")
  }

}

object HelloApp extends App {
  val system = ActorSystem("Hellosystem")
  val helloActor = system.actorOf(Props[HelloActor], name = "helloactor")
  helloActor ! "hello"
  helloActor ! "say what!"
}
