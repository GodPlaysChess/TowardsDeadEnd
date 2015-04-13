package functionalex.part3


import functionalex.part1.{State, Either, Right}
import functionalex.part2.Par.Par
import functionalex.part2.{Gen, Par}

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](ma: F[A])(f: A => F[B]): F[B] =
    join(map(ma)(f))

  override def map[A, B](ma: F[A])(f: A => B): F[B] =
    flatMap(ma)(a => unit(f(a)))

  def join[A](mma: F[F[A]]): F[A] =
    flatMap(mma)(m => m)

  def _compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => join(map(f(a))(g))

  def compose[A, B, C](f: A => F[B], g: B => F[C]): A => F[C] =
    a => flatMap(f(a))(g)

  override def apply[A, B](fab: F[A => B])(af: F[A]): F[B] =
    join(map(fab)(f => map(af)(a => f(a))))



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

//  def leftIdentity[A](f: A => F[A]) =
//    compose(f, unit) == f
//
//  def rightIdentity[A](f: A => F[A]) =
//    compose(unit, f) == f
//
//  def _leftIdentity[A](x: F[A]) =
//    flatMap(x)(unit) == x
//
//  def _rightIdentity[A](y: A)(f: A => F[A]) =
//    flatMap(unit(y))(f) == f(y)

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

  def eitherMonad[E] = new Monad[({type f[x] = Either[E, x]})#f] {
    override def unit[A](a: => A): Either[E, A] = Right(a)

    override def flatMap[A, B](ma: Either[E, A])(f: A => Either[E, B]): Either[E, B] =
      ma.flatMap(f)
  }

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

  def stateMonad[S] = new Monad[({type f[x] = State[S, x]})#f] {
    override def unit[A](a: => A): State[S, A] = State(s => (a, s))

    override def flatMap[A, B](st: State[S, A])(f: A => State[S, B]): State[S, B] =
      st flatMap f

  }

}

object IntstateMonad extends Monad[({type IntState[A] = State[Int, A]})#IntState] {
  override def unit[A](a: => A): State[Int, A] = ???

  override def flatMap[A, B](ma: State[Int, A])(f: (A) => State[Int, B]): State[Int, B] = ???
}
