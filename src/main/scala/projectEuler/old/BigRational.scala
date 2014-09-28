package projectEuler.old

class BigRational(n: BigInt, d: BigInt) {
  require(!d.equals(BigInt(0)))

  private val g = gcd(n.abs, d.abs)
  val numer: BigInt = n / g
  val denom: BigInt = d / g

  def this(n: BigInt) = this(n, 1)

  override def toString = numer + "/" + denom

  def +(num: BigRational): BigRational =
    new BigRational(num.numer * denom + numer * num.denom, num.denom * denom)

  def +(num: Int): BigRational =
    new BigRational(num * denom + numer, denom)

  def inverse = new BigRational(denom, numer)

  def *(num: BigRational): BigRational =
    new BigRational(num.numer * numer, num.denom * denom)

  def lessThan(that: BigRational): Boolean =
    this.numer * that.denom < that.numer * this.denom

  def max(that: BigRational) =
    if (this.lessThan(that)) that else this

  def gcd(a: BigInt, b: BigInt): BigInt =
    if (b == BigInt(0)) a else gcd(b, a % b)

  override def hashCode(): Int = {
    val state = Seq(numer, denom)
    state.map(_.hashCode()).foldLeft(0)((a, b) => 31 * a + b)
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[BigRational]

  override def equals(other: Any): Boolean = other match {
    case that: BigRational =>
      (that canEqual this) &&
        numer == that.numer &&
        denom == that.denom
    case _ => false
  }
}
