package concurrency.akka.picalculation

import java.util.concurrent.TimeUnit

import akka.actor.{Props, Actor, ActorRef}
import akka.routing.RoundRobinRouter

import scala.concurrent.duration.Duration

class Master(nrOfWorkers: Int, nrOfMessages: Int, nrOfElements: Int, listener: ActorRef) extends Actor {
  var pi: Double = _
  var nrOfResults: Int = _
  var start: Long = System.currentTimeMillis()

  val workerRouter = context.actorOf(Props[Worker].withRouter(RoundRobinRouter(nrOfWorkers)), "workerRouter")

  override def receive: Receive = {
    case Calculate =>
      for (i <- 0 until nrOfMessages) workerRouter ! Work(i * nrOfElements, nrOfElements)
    case Result(value) =>
      pi += value
      nrOfResults += 1
      if (nrOfResults == nrOfMessages) {
        // Send the result to the listener
        listener ! PiApproximation(pi, duration = Duration(System.currentTimeMillis - start, TimeUnit.MICROSECONDS))
        // Stops this actor and all its supervised children
        context.stop(self)
      }

  }
}
