package concurrency.akka.picalculation

import akka.actor.Actor

import scalaz.{Scalaz, StreamT}
import Scalaz._

class Worker extends Actor {
  override def receive: Receive = {
    case Work(start, nrOfElements) =>
      sender ! Result(calculatePiFor(start, nrOfElements))
  }

  def calculatePiFor(start: Int, nrElems: Int): Double =
    StreamT.unfold[Double, Int](0)(piFunc).take(nrElems).toStream.sum

  def piFunc(n: Int): Option[(Double, Int)] =
    (math.pow(-1, n) / (2 * n + 1) -> (n + 1)).some
}
