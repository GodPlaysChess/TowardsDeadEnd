package functionalex.part2

/**
 * Just a little experiment, to revive logic
 */
trait Parser[+A] {
  type ParseError = String

  /**
   * Parses the input.
   * Essence of parsing, making an desired object
   * from the input string or ends up with an error.
   * Stops and returns object, when successfully 'found' it in string.
   */
  def run(input: String): Either[ParseError, A]

  /**
   * Parses the whole fragment, returning sequence of objects. Can be zero
   */
  def all: Parser[Seq[A]]

  /**
   * Parses the whole fragment, returning one or more object.
   * Shall it return Error if found nothing? Probably not.
   */
  def + : Parser[Seq[A]]

  def map[B](f: A => B): Parser[B]

  def flatMap[B](f: A => Parser[B]): Parser[B]


}

object Parsers {

  /**
   * Put the value into the parser (alias to pure or unit)
   * In other words - creates the parser, which parses the value A.
   */
  def point[A](a: A): Parser[A] = ???

}

