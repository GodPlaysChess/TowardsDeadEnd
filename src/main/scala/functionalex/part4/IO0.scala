package functionalex.part4

import functionalex.part3.{Monad, Monoid}

import scala.annotation.tailrec

sealed trait IO[A] { self =>
  def map[B](f: A => B): IO[B] =
    flatMap(x => Return(f(x)))

  def flatMap[B](f: A => IO[B]): IO[B] =
    FlatMap(this, f)

  @tailrec final def run(io: IO[A]): A = io match {
    case Return(a) => a
    case Suspend(r) => r()
    case FlatMap(x, f) => x match {
      case Return(a) => run(f(a))
      case Suspend(r) => run(f(r()))
      case FlatMap(y, g) => run(y flatMap (a => g(a) flatMap f))
    }
  }
}

case class Return[A](a: A) extends IO[A]
case class Suspend[A](resume: () => A) extends IO[A]
case class FlatMap[A,B](sub: IO[A], k: A => IO[B]) extends IO[B]



sealed trait IO0[A] { self =>
  def run(): A

  def map[B](f: A => B): IO0[B] =
    IOMonad.map(self)(f)

  def flatMap[B](f: A => IO0[B]): IO0[B] =
    IOMonad.flatMap(self)(f)

  def ++[B](than: IO0[B]) = new IO0[B] {
    override def run(): B = {
      self.run()
      than.run()
    }
  }
}

object IOMonad extends Monad[IO0] {

  override def flatMap[A, B](ma: IO0[A])(f: (A) => IO0[B]): IO0[B] = new IO0[B] {
    override def run(): B = f(ma.run()).run()
  }

  override def unit[A](a: => A): IO0[A] = new IO0[A] {
    override def run(): A = a
  }

  def apply[A](a: => A) = unit(a)


}

object IOActions {
  def ReadLine: IO0[String] = IOMonad(scala.io.StdIn.readLine())

  def PrintLine(msg: String): IO0[Unit] = IOMonad(println(msg))

  def echo: IO0[Unit] = ReadLine flatMap PrintLine
}


