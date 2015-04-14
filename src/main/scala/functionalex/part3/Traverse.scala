package functionalex.part3

import functionalex.part1.State

trait Traverse[F[_]] extends Functor[F] with Foldable[F] {
  self =>

  def traverse[M[_] : Applicative, A, B](fa: F[A])(f: A => M[B]): M[F[B]] =
    sequence(map(fa)(f))

  def sequence[M[_] : Applicative, A](fma: F[M[A]]): M[F[A]] =
    traverse(fma)(ma => ma)

  override def map[A, B](fa: F[A])(f: A => B): F[B] =
    traverse[Id, A, B](fa)(f)(idMonad)

  override def foldMap[A, B](as: F[A])(f: A => B)(mb: Monoid[B]): B =
    traverse[({type f[x] = Const[B, x]})#f, A, Nothing](as)(f)(monoidApplicative(mb))

  def traverseS[S, A, B](fa: F[A])(f: A => State[S, B]): State[S, F[B]] =
    traverse[({type f[x] = State[S, x]})#f, A, B](fa)(f)(Monad.stateMonad)

  def mapAccum[S, A, B](fa: F[A], s: S)(f: (A, S) => (B, S)): (F[B], S) =
    traverseS(fa)((a: A) => for {
      s1 <- State.get[S]
      (b, s2) = f(a, s1)
      _ <- State.set(s2)
    } yield b).run(s)

  override def toList[A](fa: F[A]): List[A] =
    mapAccum(fa, List[A]())((a, s) => ((), a :: s))._2.reverse

  def reverse[A](fa: F[A]): F[A] =
    mapAccum(fa, toList(fa).reverse)((_, as) => (as.head, as.tail))._1

  def foldLeftTraverse[A, B](z: B)(fa: F[A])(f: (B, A) => B) =
    mapAccum(fa, z)((a, b) => ((), f(b, a)))._2

  def zip[A, B](fa: F[A], fb: F[B]): F[(A, B)] =
    mapAccum(fa, toList(fb)) {
      case (_, Nil) => sys.error("Incompatible shapes")
      case (a, b :: bs) => (a -> b, bs)
    }._1

  def fuse[M[_], N[_], A, B](fa: F[A])(g: A => M[B], h: A => N[B])(implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    traverse[({type f[x] = (M[x], N[x])})#f, A, B](fa)(a => (g(a), h(a)))(M ** N)

  def fuse1[M[_], N[_], A, B](fa: F[A])(g: A => M[B], h: A => N[B])(implicit M: Applicative[M], N: Applicative[N]): (M[F[B]], N[F[B]]) =
    sequence(map(fa)(g))(M) -> sequence(map(fa)(h))(N) //with two traverses

  def compose[G[_]: Traverse]/*(implicit G: Traverse[G])*/: Traverse[({type f[x] = F[G[x]]})#f] =
    new Traverse[({type f[x] = F[G[x]]})#f] {
      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
        self.traverse(fa)((ga: G[A]) => implicitly[Traverse[G]].traverse(ga)(f))
    }

  def composeM[G[_]](F: Monad[F], G: Monad[G], T: Traverse[G]) = new Monad[({type f[x] = F[G[x]]})#f] {
    override def flatMap[A, B](ma: F[G[A]])(f: (A) => F[G[B]]): F[G[B]] =
      F.flatMap(ma)(na => F.map(T.traverse(na)(f)(F))(G.join))

    override def unit[A](a: => A): F[G[A]] =
      F.unit(G.unit(a))
  }

//  def compose[G[_]](implicit G: Applicative[G]) = new Traverse[({type f[x] = F[G[x]]})#f] {
//    val G = implicitly[G[_]] ?
//    override def traverse[M[_] : Applicative, A, B](fa: F[G[A]])(f: (A) => M[B]): M[F[G[B]]] =
//      self.traverse(fa)((ga: G[A]) => G.traverse[G, A](ga)(f))
//
//    new Traverse[({type f[x] = F[G[x]]})#f] {
//      override def traverse[M[_]:Applicative,A,B](fa: F[G[A]])(f: A => M[B]) =
//        self.traverse(fa)((ga: G[A]) => G.traverse(ga)(f))
//    }





  implicit def monoidApplicative[M](M: Monoid[M]) =
    new Applicative[({type f[x] = Const[M, x]})#f] {
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
  override def traverse[M[_], A, B](as: List[A])(f: A => M[B])(implicit M: Applicative[M]): M[List[B]] =
    as.foldRight(M.unit(List[B]()))((a, fbs) => M.map2(f(a), fbs)(_ :: _))
}

object OptionTraverse extends Traverse[Option] {
  override def traverse[M[_], A, B](oa: Option[A])(f: A => M[B])(implicit M: Applicative[M]): M[Option[B]] =
    oa match {
      case None => M.unit(None)
      case Some(m) => M.map(f(m))(Some(_))
    }
}

case class TreeN[A](head: A, tail: List[TreeN[A]])

object TreeTraverse extends Traverse[TreeN] {
  override def traverse[M[_], A, B](ta: TreeN[A])(f: A => M[B])(implicit M: Applicative[M]): M[TreeN[B]] =
    M.map2(f(ta.head), ListTraverse.sequence(ta.tail.map(traverse(_)(f))))(TreeN(_, _))
}
