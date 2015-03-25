package functionalex.part2

import scala.util.matching.Regex

trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}

class Parser[JSON] {
  implicit def string(s: String): Parser[String] = ???

  def flatMap(f: JSON => Parser[JSON]): Parser[JSON] = ???

  def or(s2: => Parser[JSON]): Parser[JSON] = ???

  implicit def regex(r: Regex): Parser[String] = ???

  def slice(): Parser[String] = ???

  def succeed(a: JSON): Parser[JSON] = ???
}

