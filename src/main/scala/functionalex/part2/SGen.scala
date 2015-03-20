package functionalex.part2

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

