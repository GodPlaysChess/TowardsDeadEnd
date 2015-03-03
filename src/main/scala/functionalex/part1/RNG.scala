package functionalex.part1


trait RNG {
  type Rand[+A] = RNG => (A, RNG)

  def nextInt: (Int, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng1) = ra(rng)
      val (b, rng2) = rb(rng1)
      (f(a, b), rng2)
    }

  def both[A, B](ra: Rand[A], rb: Rand[B]): Rand[(A, B)] =
    map2(ra, rb)((_, _))

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    rng => {
      fs.foldLeft(List.empty[A], rng)((state, rand) => {
        val (a, rng1) = rand(state._2)
        (a +: state._1, rng1)
      })
    }
  }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (a, r) = rng.nextInt
    if (a == Int.MinValue) (Int.MaxValue, r)
    else (Math.abs(a), r)
  }

  def nonNegativeEven: Rand[Int] =
    map(nonNegativeInt)(i => i - i % 2)

  def double: Rand[Double] = {
    map(nonNegativeInt)(_.toDouble / Int.MaxValue)
  }

  def intDouble(rng: RNG): Rand[(Int, Double)] =
    both(int, double)

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

  def ints(count: Int): (List[Int], RNG) = {
    def innerInts(c: Int, l: List[Int], rng: RNG): (List[Int], RNG) = {
      if (c == 0) (l, rng)
      else {
        val (i, r) = rng.nextInt
        innerInts(c - 1, i :: l, r)
      }
    }
    innerInts(count, List.empty, this)
  }

  def ints1(count: Int): (List[Int], RNG) =
    sequence(List.fill(count)(int))(this)

}

case class SimpleRNG(seed: Long) extends RNG {

  override def nextInt: (Int, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = SimpleRNG(newSeed)
    val n = (newSeed >>> 16).toInt
    (n, nextRNG)
  }
}
