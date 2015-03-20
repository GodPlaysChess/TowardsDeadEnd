package functionalex.part2

import functionalex.part1.{Stream, Option, Some, None}

/**
 * Created by Gleb on 3/17/2015.
 */
case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(n => forSize(n).flatMap(f(_).forSize(n)))

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def map2[B, C](gb: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen(n => forSize(n).map2(gb.forSize(n))(f))
}

/**
 * Stands for exhaustive generator.
 * Generates all possible values of type A.
 * @param f must be a function which generates some Traversable
 * @tparam A
 *
 */
//TODO make this smart
case class EGen[A](f: Traversable[A]) {

  /**
   * It is easy to generate values for all 'integer' types.
   * Basically if one has min value - and max value then we can increase by 1
   *
   * It can be even mor general if we have some "min", "max" and "step", then we can produce
   * all sequence (by using the same unfold, for example)
   *
   * so all we need it to define (start value S, f: (S => S1) next value, and finish value F)
   * And it looks almost exactly like Stream.unfold (taking in account the finite nature of our domain)
   *
   * Now we can add some implicit EGens
   **/

  def allValues: Traversable[A] =
    f


}

object EGen {
  def values[A](all: Traversable[A]) =
    EGen(all)

  def make[A](min: A, max: A)(f: (A => A)): EGen[A] = {
    val g: (A => Option[(A, A)]) = { v =>
      if (v == max) None
      else Some((f(v), f(v)))
    }
    EGen(Stream.unfold[A, A](min)(g).toList)
  }

}

