package functionalex.part3

trait Applicative[F[_]] extends Functor[F]{
  // those two is the primitives which must be set
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(af: F[A]): F[B]

  // everything else is defined through that primitives
  def map2[A, B, C](af: F[A], bf: F[B])(f: (A, B) => C): F[C] =
    apply[B, C](map(af)(f.curried))(bf)

  def _map2[A, B, C](af: F[A], bf: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit[A => B => C](f.curried))(af))(bf)

  override def map[A, B](af: F[A])(f: A => B): F[B] =
    apply[A,B](unit(f))(af)

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B])) {
      (el, acc) => map2(f(el), acc)(_ :: _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  //    map2(fab, af)(_(_))

}

object Applicative {
  def validationApplicative[E] = new Applicative[({type f[x] = Validation[E, x]})#f] {
    override def apply[A, B](fab: Validation[E, (A) => B])(af: Validation[E, A]): Validation[E, B] = af match {
      case Failure(head, _) => fab match {
        case Failure(head1, tail) => Failure(head, head1 +: tail)
        case Success(a) => Failure(head, Vector.empty)
      }
      case Success(a) => fab match {
        case Failure(head, tail) => Failure(head, tail)
        case Success(f) => Success(f(a))
      }
    }

    override def unit[A](a: => A): Validation[E, A] = Success(a)
  }
}