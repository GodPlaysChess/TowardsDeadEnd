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
    val readerMonad = Reader.readerMonad[String]
    val r2: Reader[String, List[String]] = readerMonad.replicateM(3, readerMonad.unit("Hello"))
    val r1: Reader[String, Int] = readerMonad.map(r2)(_.length)
    println(r2.run("world"))
    println(r1.run("world"))
    println(readerMonad.map(readerMonad.unit("hello"))(_.length).run("w"))
    val lenReader = Reader[String, Int](_.length)
    println(lenReader.run("word"))
    println(readerMonad.map(lenReader)(_ * 2).run("word"))
    println(readerMonad.flatMap(lenReader)(x => Reader(y => y * x)).run("word"))




  }

}

