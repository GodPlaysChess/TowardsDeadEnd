package functionalex.part1

object HandlingErrors {

  /*==== Optional ====*/

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

  def sequence1[A](a: List[Option[A]]): Option[List[A]] = {
    a.foldLeft(Some(List[A]()): Option[List[A]])(map2(_, _)(_ :+ _))
  }

  // map + sequence traverses the list twice. Implement it, that it traverses the list only once
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    a.foldLeft(Some(List[B]()): Option[List[B]])((x, y) => map2(x, f(y))(_ :+ _)) // or  _.::(_) append or prepend?
  }

  def sequence[A](a: List[Option[A]]): Option[List[A]] =
    traverse(a)(identity)




  /*==== Either ====*/

  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty) Left("mean of empty List!")
    else Right(xs.sum / xs.length)

  def sequenceE[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverseE(es)(identity)

  def traverseE[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] = {
    as.foldLeft(Right(List[B]()): Either[E, List[B]])((z, elem) => z.map2(f(elem))(_ :+ _))
  }


  def main(args: Array[String]) {
//    println(sequence(List(Some(5), Some(6), Some(1))))
//    println(sequence(List(Some(5), None, Some(1))))
    println(traverseE(List(1, 2, 3, 4, 5))(x => if (x < 4) Right("Hel") else Left(s"The number {$x} is too big ")))

  }


}
