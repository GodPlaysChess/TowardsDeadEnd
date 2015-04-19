package functionalex.part4

import functionalex.part2.Par
import functionalex.part2.Par.Par
import functionalex.part3.Monad
import functionalex.part4.Free.~>

sealed trait Console[A] {
  def toPar: Par[A]

  def toThunk: () => A
}

case object ReadLine extends Console[Option[String]] {
  override def toPar: Par[Option[String]] = Par.lazyUnit(run)

  override def toThunk: () => Option[String] = () => run

  def run: Option[String] =
    try Some(scala.io.StdIn.readLine())
    catch {
      case e: Exception => None
    }

}

case class PrintLine(line: String) extends Console[Unit] {
  override def toPar: Par[Unit] = Par.lazyUnit(println(line))

  override def toThunk: () => Unit = () => println(line)

}

object Console {
  type ConsoleIO[A] = Free[Console, A]

  implicit val function0Monad = new Monad[Function0] {
    def unit[A](a: => A) = () => a
    override def flatMap[A,B](a: Function0[A])(f: A => Function0[B]) =
      () => f(a())()
  }

  implicit val parMonad = new Monad[Par] {
    def unit[A](a: => A) = Par.unit(a)
    override def flatMap[A,B](a: Par[A])(f: A => Par[B]) =
      Par.fork { Par.flatMap(a)(f) }
  }

  def readLn: ConsoleIO[Option[String]] =
    Suspend1[Console, Option[String]](ReadLine)

  def printLn(line: String): ConsoleIO[Unit] =
    Suspend1[Console, Unit](PrintLine(line))

  def runConsoleFunction0[A](a: Free[Console, A]): () => A =
    Free.runFree[Console, Function0, A](a)(consoleToFunction0)

  def runConsolePar[A](a: Free[Console, A]): Par[A] =
    Free.runFree[Console, Par, A](a)(consoleToPar)

  val consoleToFunction0 =
    new (Console ~> Function0) {
      override def apply[A](a: Console[A]): () => A = a.toThunk
    }

  val consoleToPar =
    new (Console ~> Par) {
      override def apply[A](a: Console[A]): Par[A] = a.toPar
    }


  def translate[F[_],G[_],A](f: Free[F,A])(fg: F ~> G): Free[G,A] = {
    type FreeG[A] = Free[G, A]
    val t = new (F ~> FreeG) {
      def apply[A](a: F[A]): Free[G,A] = Suspend1[G, A](fg(a))
    }
    Free.runFree[F, FreeG, A](f)(t)(Free.freeMonad[G])
  }

  def runConsole[A](a: Free[Console,A]): A =
    Free.runTrampoline { translate(a)(new (Console ~> Function0) {
      def apply[A](c: Console[A]) = c.toThunk
    })}


}
