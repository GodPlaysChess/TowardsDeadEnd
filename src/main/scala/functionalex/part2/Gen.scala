package functionalex.part2

import functionalex.part1.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State[RNG, Int](rng => rng.nonNegativeInt(rng)).map(i => i % (stopExclusive - start) + start))

  def boolean(): Gen[Boolean] =
    Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 1))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((_, _))

  def option: Gen[Option[A]] =
    map(Some(_))

  def string(length: Int): Gen[String] =
    Gen.listOfN(length, char).map(_.mkString)

  /**
   * generates ASCII character including special
   */
  def char: Gen[Char] =
    choose(32, 127).map(_.toChar)


}

object Gen {

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  def fromOption[A](go: Gen[Option[A]]): Option[Gen[A]] = ???


}
