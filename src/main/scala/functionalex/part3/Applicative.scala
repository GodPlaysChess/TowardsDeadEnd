package functionalex.part3

trait Applicative[F[_]] extends Functor[F]{
  self =>
  // those two is the primitives which must be set
  def unit[A](a: => A): F[A]

  def apply[A, B](fab: F[A => B])(af: F[A]): F[B]

  // everything else is defined through that primitives
  def map2[A, B, C](af: F[A], bf: F[B])(f: (A, B) => C): F[C] =
    apply[B, C](map(af)(f.curried))(bf)

  def _map2[A, B, C](af: F[A], bf: F[B])(f: (A, B) => C): F[C] =
    apply(apply(unit[A => B => C](f.curried))(af))(bf)

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D) = {
    val fcd: F[(C) => D] = map2(fa, fb)((a,b) => f.curried(a)(b))
    apply(fcd)(fc)
  }


  override def map[A, B](af: F[A])(f: A => B): F[B] =
    apply[A,B](unit(f))(af)

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B])) {
      (el, acc) => map2(f(el), acc)(_ :: _)
    }

  def sequenceM[K, V](ma: Map[K, F[V]]): F[Map[K, V]] =
    ma.foldRight(unit(Map.empty[K, V])) {
      (el, acc) => map2(acc, el._2)((m, v) => m + (el._1 -> v))
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def **[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = (F[x], G[x])})#f] {
    override def apply[A, B](fab: (F[A => B], G[A => B]))(af: (F[A], G[A])): (F[B], G[B]) =
     self.apply(fab._1)(af._1) -> G.apply(fab._2)(af._2)

    override def unit[A](a: => A): (F[A], G[A]) = self.unit(a) -> G.unit(a)
  }

  def compose[G[_]](G: Applicative[G]) = new Applicative[({type f[x] = F[G[x]]})#f] {
    override def apply[A, B](fab: F[G[(A) => B]])(af: F[G[A]]): F[G[B]] =
      map2(fab, af)(_(_))

    override def map2[A, B, C](fga: F[G[A]], fgb: F[G[B]])(f: (A,B) => C) =
      self.map2(fga, fgb)(G.map2(_,_)(f))

    override def unit[A](a: => A): F[G[A]] = self.unit(G.unit(a))
  }




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