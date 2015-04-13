package functionalex.part4

import functionalex.part3.{Monad, Monoid}

sealed trait IO[A] { self =>
  def run(): A

  def map[B](f: A => B): IO[B] =
    IOMonad.map(self)(f)

  def flatMap[B](f: A => IO[B]): IO[B] =
    IOMonad.flatMap(self)(f)

  def ++[B](than: IO[B]) = new IO {
    override def run(): Unit = {
      self.run()
      than.run()
    }
  }
}

object IOMonad extends Monad[IO] {

  override def flatMap[A, B](ma: IO[A])(f: (A) => IO[B]): IO[B] = new IO[B] {
    override def run(): B = f(ma.run()).run()
  }

  override def unit[A](a: => A): IO[A] = new IO[A] {
    override def run(): A = a
  }

  def apply[A](a: => A) = unit(a)


}

object IO extends Monoid[IO] {

  override def op(a1: IO, a2: IO): IO = new IO {
    override def run(): Unit = { a1.run(); a2.run() }
  }

  override def zero: IO = new IO {
    override def run(): Unit = ()
  }

  def empty = zero
}

object IOActions {
  def ReadLine: IO[String] = IOMonad(scala.io.StdIn.readLine())

  def PrintLine(msg: String): IO[Unit] = IOMonad(println(msg))

  def echo: IO[Unit] = ReadLine flatMap PrintLine
}


