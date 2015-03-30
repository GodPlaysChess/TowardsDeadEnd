import functionalex.part3.Monoids

object Hi {
  def main(args: Array[String]) {
    val seq1 = IndexedSeq(1, 2, 3, 4)
    val seq2 = IndexedSeq(1, 2, 4, 3)
    val seq3 = IndexedSeq(4, 3, 2, 1)
    println(Monoids.isOrdered(seq1))
    println(Monoids.isOrdered(seq2))
    println(Monoids.isOrdered(seq3))

    val ex1 = "word"
    val ex2 = "two words"
    val ex3 = "three words here"
    val ex4 = "string contains four words"
    val ex5 = "string contains exactly five words"
    val ex6 = "this string contains exactly six words"
    println(Monoids._countWords(ex1))
    println(Monoids._countWords(ex2))
    println(Monoids._countWords(ex3))
    println(Monoids._countWords(ex4))
    println(Monoids._countWords(ex5))
    println(Monoids._countWords(ex6))
  }

}

