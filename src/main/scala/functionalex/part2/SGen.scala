package functionalex.part2

/**
 * Created by Gleb on 3/17/2015.
 */
case class SGen[A](forSize: Int => Gen[A]) {

  def flatMap[B](f: A => SGen[B]): SGen[B] =
    SGen(forSize(_).flatMap(x => f(x).forSize(0))) // why 0?

  def map[B](f: A => B): SGen[B] =
    SGen(forSize(_).map(f))

  def map2[B, C](gb: SGen[B])(f: (A, B) => C): SGen[C] =
    SGen(forSize(_).map2(gb.forSize(0))(f))

}

object SGen {
  def listOf[A](g: Gen[A]): SGen[List[A]] =
    SGen(n => Gen.listOfN(n, g))
}
