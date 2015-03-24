package functionalex.part2

import functionalex.part1.Either

import scala.language.implicitConversions
import scala.util.matching.Regex

// for any Char c : run(char(c))(c.toString) == Right(c)
trait Parsers[ParseError, Parser[+ _]] {
  self =>

  def char(c: Char): Parser[Char] =
    string(c.toString) map (_.charAt(0))

  def succeed[A](a: A): Parser[A] =
    string("") map (_ => a)

  implicit def string(s: String): Parser[String]

  implicit def operators[A](p: Parser[A]): ParserOps[A] = ParserOps[A](p)

  implicit def asStringParser[A](a: A)(implicit f: A => Parser[String]): ParserOps[String] = ParserOps(f(a))

  def run[A](p: Parser[A])(input: String): Either[ParseError, A]

  def orString(s1: String, s2: String): Parser[String]

  def or[A](s1: Parser[A], s2: => Parser[A]): Parser[A]

  //don't really get how it should work. Does it have to return only matched fragments?
  //if not, then it always returns the whole messge.
  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]]

  // i dont like the way to use listOfN with various N.
  def zeroOrMore[A](c: Parser[A]): Parser[Int] =
    slice(many(c)) map (_.size)

  def oneOrMore[A](c: Parser[A]): Parser[Int]

  def zeroAFolOneB[A](c1: Parser[A], c2: Parser[A]): Parser[(Int, Int)] =
    zeroOrMore(c1) ** oneOrMore(c2)

  def and[A](value: Parser[A], value1: => Parser[A]): Parser[A]

  def together[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
    pa.flatMap(s => pb.map((s, _)))

  // counts given strings
  def count[A](s: Parser[A]): Parser[Int]

  def many[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _) or succeed(List())

  def many1[A](p: Parser[A]): Parser[List[A]] =
    map2(p, many(p))(_ :: _)

  def map[A, B](a: Parser[A])(f: A => B): Parser[B] =
    a flatMap(s => succeed(f(s)))

  def slice[A](p: Parser[A]): Parser[String]

  def map2[A, B, C](p: Parser[A], p2: => Parser[B])(g: (A, B) => C): Parser[C] =
    for {
      a <- p
      b <- p2
    } yield g(a, b)

  def listOfN[A](n: Int, p: Parser[A]): Parser[List[A]] =
    if (n == 0) succeed(List())
    else map2(p, listOfN(n - 1, p))(_ :: _)

  def flatMap[A, B](p: Parser[A])(f: A => Parser[B]): Parser[B]

  implicit def regex(r: Regex): Parser[String]


  case class ParserOps[A](p: Parser[A]) {
    def |[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def or[B >: A](p2: => Parser[B]): Parser[B] = self.or(p, p2)

    def &[B >: A](p2: => Parser[B]): Parser[B] = self.and(p, p2)

    def many: Parser[List[A]] = self.many(p)

    def map[B](f: A => B) = self.map(p)(f)

    def **[B](p2: => Parser[B]): Parser[(A, B)] = self.together(p, p2)

    def flatMap[B](f: A => Parser[B]): Parser[B] = self.flatMap(p)(f)
  }

  object Laws {

    import functionalex.part2.Prop._
    import functionalex.part1.Right

    def equal[A](p1: Parser[A], p2: Parser[A])(in: Gen[String]): Prop =
      forAll(in)(s => run(p1)(s) == run(p2)(s))

    def mapLaw[A](p: Parser[A])(in: Gen[String]): Prop =
      equal(p, p.map(identity))(in)

    /**
     * Parser, which succeeds on Any String ('a') should succeed on Any string (s)
     **/
    def succeedLaw[A](in: Gen[(String, String)]): Prop =
      forAll(in) { case (a, s) => run(succeed(a))(s) == Right(a)}

  }

}

