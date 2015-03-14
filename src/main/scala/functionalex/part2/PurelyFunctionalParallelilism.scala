package functionalex.part2

object PurelyFunctionalParallelilism {
 /* // answer why this is sequential (hint: inline sumL and sumR)
  def sumSequential(ints: IndexedSeq[Int]): Int =
    if (ints.size <= 1)
      ints.headOption getOrElse 0
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      val sumL: Par[Int] = Par.unit(sumSequential(l))
      val sumR: Par[Int] = Par.unit(sumSequential(r))
      Par.run(sumL) + Par.run(sumR)
    }

  // true parallel
  def sum(ints: IndexedSeq[Int]): Par[Int] =
    if (ints.size <= 1)
      Par.unit(ints.headOption getOrElse 0)
    else {
      val (l, r) = ints.splitAt(ints.length / 2)
      Par.map2(Par.fork(sum(l)), Par.fork(sum(r)))(_ + _)
    }

*/
}
