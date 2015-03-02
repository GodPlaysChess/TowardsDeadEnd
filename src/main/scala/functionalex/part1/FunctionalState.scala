package functionalex.part1

object FunctionalState {

  // better to do map on tuple
  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, r) = rng.nextInt
    if (a == Int.MinValue) (Int.MaxValue, r)
    else (Math.abs(a), r)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (a, r) = nonNegativeInt(rng)
    (a.toDouble / Int.MaxValue, r)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (i, r1) = rng.nextInt
    val (d, r2) = double(r1)
    ((i, d), r2)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val (d, r1) = double(rng)
    val (i, r2) = r1.nextInt
    ((d, i), r2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (d1, r1) = double(rng)
    val (d2, r2) = double(r1)
    val (d3, r3) = double(r2)
    ((d1, d2, d3), r3)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def innerInts(c: Int, l: List[Int], rng: RNG): (List[Int], RNG) = {
      if (c == 0) (l, rng)
      else {
        val (i, r) = rng.nextInt
        innerInts(c - 1, i :: l, r)
      }
    }
    innerInts(count, List.empty, rng)
  }


  def tupleMap[A, B, C](t: (A, B))(f: A => C): (C, B) =
    (f(t._1), t._2)


}
