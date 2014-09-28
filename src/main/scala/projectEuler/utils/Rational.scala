package projectEuler.utils

class Rational(n: Long, d: Long) {
  require(d != 0)

  private val g = gcd(n.abs, d.abs)
  val numer: Long = n / g
  val denom: Long = d / g

  def this(n: Long) = this(n, 1)

  override def toString = numer + "/" + denom


  def +(num: Rational): Rational =
    new Rational(num.numer * denom + numer * num.denom, num.denom * denom)

  def +(num: Int): Rational =
    new Rational(num * denom + numer, denom)

  def inverse = new Rational(denom, numer)

  def *(num: Rational): Rational =
    new Rational(num.numer * numer, num.denom * denom)

  def lessThan(that: Rational) =
    this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) =
    if (this.lessThan(that)) that else this

  def gcd(a: Long, b: Long): Long =
    if (b == 0) a else gcd(b, a % b)

  implicit def intToRational(x: Int) = new Rational(x)


}
