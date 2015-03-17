package functionalex.part2

import functionalex.part1.{RNG, State}

case class Gen[A](sample: State[RNG, A]) {

  def flatMap[B](f: A => Gen[B]): Gen[B] =
    Gen(sample.flatMap(a => f(a).sample))

  def listOfN(size: Gen[Int]): Gen[List[A]] =
    size.flatMap(n => Gen.listOfN(n, this))

  //Gen(State.sequence(List.fill(n)(g.sample)))

  def map[B](f: A => B): Gen[B] =
    Gen(sample.map(f))

  def map2[B, C](gb: Gen[B])(f: (A, B) => C): Gen[C] =
    Gen(sample.map2(gb.sample)(f))

  def map3[B, C, D](gb: Gen[B], gc: Gen[C])(f: (A, B, C) => D): Gen[D] =
    Gen(sample.map3(gb.sample, gc.sample)(f))

  def option: Gen[Option[A]] =
    map(Some(_))

  def unsized: SGen[A] = SGen(_ => this)

}

object Gen {

  def choose(start: Int, stopExclusive: Int): Gen[Int] =
    Gen(State[RNG, Int](rng => rng.nonNegativeInt(rng)).map(i => i % (stopExclusive - start) + start))

  def choose2(start: Int, stopExclusive: Int): Gen[(Int, Int)] =
    choose(start, stopExclusive).map2(choose(start, stopExclusive))((_, _))

  def boolean(): Gen[Boolean] =
    Gen(State[RNG, Int](_.nextInt).map(_ % 2 == 1))

  def string(length: Int): Gen[String] =
    Gen.listOfN(length, char).map(_.mkString)

  def listOfN[A](n: Int, g: Gen[A]): Gen[List[A]] =
    Gen(State.sequence(List.fill(n)(g.sample)))

  /**
   * generates ASCII character including special
   */
  def char: Gen[Char] =
    choose(32, 127).map(_.toChar)

  def unit[A](a: => A): Gen[A] =
    Gen(State.unit(a))

  def fromOption[A](go: Gen[Option[A]]): Option[Gen[A]] = ???

  //definitely bad implementation cause I am returning old one, but not the combined one
  def unionBadExample[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean().flatMap(x => if (x) g1 else g2)

  //that's the idea, but current implementation is extremely ugly
  def _union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    Gen(State[RNG, A](rng =>
      if (boolean().sample.run(rng)._1) g1.sample.run(rng)
      else g2.sample.run(rng)))

  // the best way. Looks lik a choice
  def union[A](g1: Gen[A], g2: Gen[A]): Gen[A] =
    boolean().map3(g1, g2)((p, v1, v2) => if (p) v1 else v2)

  def weighted[A](g1: (Gen[A], Double), g2: (Gen[A], Double)): Gen[A] = {
    val prob1: Double = g1._2 / (g1._2 + g2._2)
    choose(1, 100).map3(g1._1, g2._1) {
      (i, v1, v2) => if (i < prob1 * 100) v1 else v2
    }
  }


}
