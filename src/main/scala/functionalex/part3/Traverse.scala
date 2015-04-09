package functionalex.part3

trait Traverse[F[_]] extends Functor[F] with Foldable[F] { self =>
  def traverse[M[_]:Applicative,A,B](fa: F[A])(f: A => M[B])/*(implicit M: Applicative[M])*/: M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_]:Applicative,A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(identity)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  override def foldMap[A,B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  implicit def monoidApplicative[M](M: Monoid[M]) = new Applicative[({ type f[x] = Const[M, x] })#f] {
    override def unit[A](a: => A): M = M.zero

    override def apply[A, B](m1: M)(m2: M): M = M.op(m1, m2)
  }


  type Const[A, B] = A
  type Id[A] = A

  val idMonad = new Monad[Id] {
    override def flatMap[A, B](a: A)(f: A => B): B = f(a)

    override def unit[A](a: => A): Id[A] = a
  }
}

object ListTraverse extends Traverse[List] {
  override def sequence[M[_]: Applicative, A](fa: List[M[A]])/*(implicit M: Applicative[M])*/: M[List[A]] =
    fa.foldRight(M.unit(List.empty[A])) {
      (el, acc) => M.map2(el, acc)(_ :: _)
    }
}

object OptionTraverse extends Traverse[Option] {
  override def sequence[M[_]: Applicative, A](fa: Option[M[A]])/*(implicit M: Applicative[M])*/: M[Option[A]] =
    fa match {
      case None => M.unit(None)
      case Some(m) => M.map(m)(Some(_))
    }
}

//object TreeTraverse extends Traverse[Tree] {
//  override def sequence[M[_]: Applicative, A](fa: Tree[M[A]])(implicit M: Applicative[M]): M[Tree[A]] =
//    fa.foldRight(M.unit(Leaf)) {
//      (el, acc) => M.map2(el, acc)(_ :: _)
//    }
//}
