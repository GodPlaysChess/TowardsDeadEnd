import functionalex.part3.{Reader, Monad, Monoids}

object Hi {
  def main(args: Array[String]) {
    val seq1 = IndexedSeq(1, 2, 3, 4)
    val seq2 = IndexedSeq(1, 2, 4, 3)
    val seq3 = IndexedSeq(4, 3, 2, 1)
    println(Monoids.isOrdered(seq1))
    println(Monoids.isOrdered(seq2))
    println(Monoids.isOrdered(seq3))

    val ex1 = "word"
    val ex2 = "t w"
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

    val map1 = Map("A" -> 1)
    val map2 = Map("A" -> 2, "B" -> 1)
    println(Monoids.mapMergeMonoid(Monoids.intAddition).op(map1, map2))
    println(Monoids.bag(Vector("a", "rose", "is", "a", "rose")))

    /* List replicate */
    val x = List(0, 1)
    println(s"Replicating $x")
    val opMonoid = Monad.listMonad.replicateM(2, x)
    println(opMonoid)

    /* Option replicate */
    val y = Option(Option(0, 1))
    println(s"Replicating $y \n")
    println(Monad.optionMonad.replicateM(2, y))

    /* Monadic filter */
    val ints = List(1, 2, 3, 4)
    println(Monad.optionMonad.filterM(ints)(i => Some(i % 2 == 0)))
    println(Monad.listMonad.filterM(ints)(i => List(i % 2 == 0)))

    /* Reader */
    val mr = Reader.readerMonad[String]
    val mr1 = mr.replicateM(3, mr.unit("Hello"))
    println(mr1.run("world"))


  }

}

