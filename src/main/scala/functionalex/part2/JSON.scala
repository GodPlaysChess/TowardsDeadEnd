package functionalex.part2

import functionalex.part1.Either

import scala.util.matching.Regex

/**
 * Created by Gleb on 3/24/2015.
 */
trait JSON

object JSON {

  case object JNull extends JSON

  case class JNumber(get: Double) extends JSON

  case class JString(get: String) extends JSON

  case class JBool(get: Boolean) extends JSON

  case class JArray(get: IndexedSeq[JSON]) extends JSON

  case class JObject(get: Map[String, JSON]) extends JSON

}

class JsonParser extends Parsers[String, JSON] {
  override implicit def string(s: String): String = ???

  // counts given strings
  override def count[A](s: JSON[A]): JSON[Int] = ???

  override def oneOrMore[A](c: JSON[A]): JSON[Int] = ???

  override def or[A](s1: JSON[A], s2: => JSON[A]): JSON[A] = ???

  override def orString(s1: String, s2: String): JSON[String] = ???

  override def and[A](value: JSON[A], value1: => JSON[A]): JSON[A] = ???

  override def run[A](p: JSON[A])(input: String): Either[String, A] = ???

  override implicit def regex(r: Regex): JSON[String] = ???

  override def slice[A](p: JSON[A]): JSON[String] = ???
}
