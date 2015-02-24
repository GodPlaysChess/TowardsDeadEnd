package functionalex.part1

sealed trait Either[+E, +A] {
  self =>

  def map[B](f: A => B): Either[E, B] = self match {
    case Right(v) => Right(f(v))
    case Left(e) => Left(e)       // can I write  case _ => _    or  self
  }

  // brute
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = self match {
    case Right(v) => f(v)
    case Left(e) => Left(e)
  }

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B] = self match {
    case Right(x) => Right(x) // can I just write self ?
    case _ => b
  }


  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    flatMap(a => b.map(f(a, _)))   // where a is coming from?
}

case class Left[+E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]
