import functionalex.part3.Monoids

object Hi {
  def main(args: Array[String]) {
    val seq1 = IndexedSeq(1, 2, 3, 4)
    val seq2 = IndexedSeq(1, 2, 4, 3)
    val seq3 = IndexedSeq(4, 3, 2, 1)
    println(Monoids.isOrdered(seq1))
    println(Monoids.isOrdered(seq2))
    println(Monoids.isOrdered(seq3))

  }

}

