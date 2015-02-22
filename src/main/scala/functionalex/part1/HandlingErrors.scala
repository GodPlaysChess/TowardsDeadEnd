package functionalex.part1

/**
 * Created by Gleb on 2/22/2015.
 */
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


}
