package functionalex.part3

/**
 * Created by Gleb on 4/4/2015.
 */
case class Reader[R, A](run: R => A)

object Reader {
  def readerMonad[R] = new Monad[({type f[x] = Reader[R, x]})#f] {
    override def unit[A](a: => A): Reader[R, A] = Reader(_ => a)

    override def flatMap[A, B](ma: Reader[R, A])(f: (A) => Reader[R, B]): Reader[R, B] =
      Reader(r => f(ma.run(r)).run(r))
  }

}
