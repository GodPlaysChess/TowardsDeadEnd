package functionalex.part1

object HandlingErrors {

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (
      m => mean(xs map (
        v => (m - v) * (m - v))
      ))


  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.sum / xs.size)
  }

  def map2[A, B, C](oa: Option[A], ob: Option[B])(f: (A, B) => C): Option[C] =
    for {
      a <- oa
      b <- ob
    } yield f(a, b)

  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    val acc = List[A]()
    a.foldLeft(Some(acc): Option[List[A]])((acc, op) => map2(acc, op)((li, v) => li :+ v))
  }

  def main(args: Array[String]) {
    println(sequence(List(Some(5), Some(6), Some(1))))
    println(sequence(List(Some(5), None, Some(1))))
  }


}
