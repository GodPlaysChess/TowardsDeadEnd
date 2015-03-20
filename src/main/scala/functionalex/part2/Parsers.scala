package functionalex.part2

import scala.language.implicitConversions

// for any Char c : run(char(c))(c.toString) == Right(c)
trait Parsers[ParseError, Parser[+ _]] {
  self =>
  def char(c: Char): Parser[Char] =
    ???

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A] =
    ???

  def orString(s1: String, s2: String): Parser[String] =
    ???

  def or[A](s1: Parser[A], s2: Parser[A]): Parser[A] =
    ???

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)
  }

}
