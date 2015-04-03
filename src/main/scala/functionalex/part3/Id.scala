package functionalex.part3

/**
 * Created by Gleb on 4/3/2015.
 */
case class Id[A](value: A) extends Monad[Id] {

  override def unit[A](a: => A): Id[A] = Id(a)

  override def flatMap[A, B](ma: Id[A])(f: (A) => Id[B]): Id[B] =
    f(ma.value)
}
