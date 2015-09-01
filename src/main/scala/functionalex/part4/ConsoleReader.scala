package functionalex.part4

import scalaz.Monad

case class ConsoleReader[A](run: String => A) {
  def map[B](f: A => B): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)))

  def flatMap[B](f: A => ConsoleReader[B]): ConsoleReader[B] =
    ConsoleReader(r => f(run(r)).run(r))

}

object ConsoleReader {
  implicit val monad = new Monad[ConsoleReader] {
    override def bind[A, B](fa: ConsoleReader[A])(f: (A) => ConsoleReader[B]) = fa flatMap f

    override def point[A](a: => A) = ConsoleReader(_ => a)
  }
}
