package functionalproj.learningscalaz

import scalaz._
import Scalaz._

/* writer monad*/
object WriterExample {

  def main(args: Array[String]) {
    val writer: Writer[String, Int] = 2.set("Writer")
    val w1: WriterT[Id.Id, String, Int] = writer :++> " add a bit"
    w1.tell

    output(writer.run, writer.run, "normal writer")
    output(writer.run, w1.run, "with appended \"a bit\"")
    output(writer.tell, w1.tell, "with appended \"a bit\" Telling")
  }

  def output(was: Any, now: Any, descr: String): Unit = {
    println(descr + ": " + was + " -> " + now)
  }

}
