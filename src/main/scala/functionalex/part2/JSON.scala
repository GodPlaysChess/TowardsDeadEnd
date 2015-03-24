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

