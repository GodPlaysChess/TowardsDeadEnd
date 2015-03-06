package functionalex.part2

import java.util.concurrent.{TimeUnit, Future, ExecutorService}


case class Par[A](a: A) {

  def map[B](f: A => B): Par[B] = ???

  def flatMap[B](f: A => Par[B]): Par[B] = ???

}

object Par {
  type Par[A] = ExecutorService => Future[A]

  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = for {
    aa <- a
    bb <- b
  } yield f(aa, bb)

  def fork[A](a: => Par[A]): Par[A] = ???

  //Par(a())

  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isDone: Boolean = true
  }

}
