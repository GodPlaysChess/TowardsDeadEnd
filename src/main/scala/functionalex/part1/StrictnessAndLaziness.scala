package functionalex.part1

object StrictnessAndLaziness {

  def fibs: Stream[Int] = {
    def addRec(o1: Int, o2: Int): Stream[Int] =
      Stream.cons(o1, addRec(o2, o1 + o2))

    addRec(0, 1)
  }

  // seed => Option(value, seed)
  // seed | 0 , 1 | (tuple). value the last from tuple 
  def fibs1: Stream[Int] =
    Stream.unfold((0, 1))(s => Some((s._2, (s._2, s._1 + s._2))))


  def main(args: Array[String]) {
    val as = Stream(1, 2, 3, 4)
    println(as.take(2).toList)
    println(as)
    println(as.toList)
    println("should be List(2,3,4):  " + as.drop(1).toList)
    println(as.takeWhile1(_ < 3).toList)
    println("Headoptions")
    println(as.headOption)
    println(as.headOption1)
    println(Stream.empty.headOption)
    println(Stream.empty.headOption1)

    println("Appends")
    lazy val x = 5
    println(Stream.append(x, as))
    println(Stream.append(x, as).toList)
    val xs = Stream(5, 6, 7, 8)
    println(Stream.appendStream(as, xs).toList)
    println(as.flatMap(a => Stream(a, a)).toList)

    println("recursuve streams")
    lazy val ones: Stream[Int] = Stream.cons(1, ones) // did not work out??
    println(ones.take(5).toList)
    println(Stream.from1(5).take(5))

    println("Fibonacci")
    println(fibs.take(10).toList)
    println(fibs1.take(10).toList)

    println(Stream.unfold(1)(x => Some(x + 1, x + 1)).take(5).toList)


    println("maps")
    println(as.map(_ + 1).toList)
    println(as.map1(_ + 1).toList)
    println(as.map1(_ + 1).take1(8).toList)

    println("ZIPS")
    val bs = Stream(9, 8, 7, 6, 5)
    println(as.zipWith(bs)(_ + _).take(3).toList)
    println(as.zipWith(bs)(_ + _).take(5).toList)

    println(as.zipWith1(bs)(_ + _).take(3).toList)
    println(as.zipWith1(bs)(_ + _).take(5).toList)

    println("Zip all\n")
    println(as.zipAll(bs).take(3).toList)
    println(as.zipAll(bs).take(5).toList)
    println(bs.zipAll(as).take(3).toList)
    println(bs.zipAll(as).take(6).toList)

  }
}
