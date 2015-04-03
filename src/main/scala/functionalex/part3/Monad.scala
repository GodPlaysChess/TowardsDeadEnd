package functionalex.part3

import functionalex.part2.Par.Par
import functionalex.part2.{Gen, Par}

/**
 * Created by Gleb on 4/2/2015.
 */
trait Monad[F[_]] {
  def unit[A](a: => A): F[A]

  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B]

  def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] =
    flatMap(fa)(a => map(fb)(b => f(a, b)))

  def sequence[A](lma: List[F[A]]): F[List[A]] =
    traverse(lma)(identity)

  def traverse[A, B](la: List[A])(f: A => F[B]): F[List[B]] =
    la.foldRight(unit(List.empty[B])) {
      (el, acc) => map2(f(el), acc)(_ :: _)
    }

  def replicateM[A](n: Int, ma: F[A]): F[List[A]] =
    sequence(List.fill(n)(ma))

  def product[A, B](ma: F[A], mb: F[B]): F[(A, B)] = map2(ma, mb)((_, _))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)


  // need some empty to implement filter
  //  def filter[A](na: F[A])(p: A => Boolean): F[A] =
  //    flatMap(na)(a => if (p(a)) unit(a) else unit())

  /**
   * Monadic filter. What does it actually mean?
   * For example:
   * Given, some List(1,2,3,4) and a function:(Int => Option(Boolean)) which
   * might filter odd numbers (f: Some(_ % 2 == 0))
   * returns Some(even ints)
   *
   * (f: Gen(Int) )
   *
   **/
  def filterM[A](ms: List[A])(f: A => F[Boolean]): F[List[A]] = {
    val x: F[List[List[A]]] = traverse(ms)(a => map(f(a))(x => if (x) List(a) else List.empty))
    val y: F[List[A]] = map(x)(_.flatten)
    y
  }

  def lift[A, B](m: F[A])(f: A => B): F[A] => F[B] =
    m => map(m)(f)

  def flatMapViaCompose[A, B](ma: F[A])(f: A => F[B]): F[B] =
    compose((_: Unit) => ma, f)(())

  def associativityLaw[A, B, C, D](f: A => F[B], g: B => F[C], h: C => F[D]) =
    compose(compose(f, g), h) == compose(f, compose(g, h))

  def leftIdentity[A](f: A => F[A]) =
    compose(f, unit) == f

  def rightIdentity[A](f: A => F[A]) =
    compose(unit, f) == f

  def _leftIdentity[A](x: F[A]) =
    flatMap(x)(unit) == x

  def _rightIdentity[A](y: A)(f: A => F[A]) =
    flatMap(unit(y))(f) == f(y)

  def compose111[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  /**
   * Equivalence: |<=>|
   *
   * leftIdentity[A](f: A => F[A])      <=>
   * compose(f, unit) == (A => F[A])    <=>  | apply to (a) both sides
   * flatMap(F[A])(unit) == F[A]        <=>
   * _leftIdentity[A](x: F[A])
   *
   */

  /**
   * Option identity:
   * flatMap(None)(unit) == None
   * flatMap(Some(_))(unit) = unit(_) = Some(_)
   *
   * compose(f, unit) = a => None flatMap unit = a => None = f
   * compose(f, unit) = a => Some(_) flatMap unit = a => Some(_) = f
   *
   */

}


object Monad {

  def genMonad = new Monad[Gen] {
    override def unit[A](a: => A): Gen[A] = Gen.unit(a)

    override def flatMap[A, B](ma: Gen[A])(f: (A) => Gen[B]): Gen[B] =
      ma flatMap f
  }

  def parMonad = new Monad[Par] {
    override def unit[A](a: => A): Par[A] = Par.unit(a)

    override def flatMap[A, B](ma: Par[A])(f: (A) => Par[B]): Par[B] =
      Par.flatMap(ma)(f)
  }

  def optionMonad = new Monad[Option] {
    override def unit[A](a: => A): Option[A] = Option(a)

    override def flatMap[A, B](ma: Option[A])(f: (A) => Option[B]): Option[B] =
      ma flatMap f
  }

  def streamMonad = new Monad[Stream] {
    override def unit[A](a: => A): Stream[A] =
      Stream(a)

    override def flatMap[A, B](ma: Stream[A])(f: (A) => Stream[B]): Stream[B] =
      ma flatMap f
  }

  def listMonad = new Monad[List] {
    override def unit[A](a: => A): List[A] =
      List(a)

    override def flatMap[A, B](ma: List[A])(f: (A) => List[B]): List[B] =
      ma flatMap f
  }

  //  def stateMonad[S, A] = new Monad[St] {
  //    type St = State[S, A]
  //
  //
  //    override def unit[A](a: => A): State[A] = ???
  //
  //    override def flatMap[A, B](ma: State[A])(f: (A) => State[B]): State[B] = ???
  //  }

}
