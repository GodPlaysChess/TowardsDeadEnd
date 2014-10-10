package crypto.week05

/**
 * Created by Gleb Parakhosnkiy on 10/8/14.
 *
 * 1712063078 is the answer, but must take ORD in account. So it is the second DLog, in other words Dlog = thisValu - Ord
 */
object NumberTheory {
  def main(args: Array[String]) {
    val p = BigInt("13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171")
    val g = BigInt("11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568")
    val h = BigInt("3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333")
    val l = BigInt(1712063078)

    val p1 = BigInt(17)
    val g1 = BigInt(9)
    val h1 = BigInt(4)
    //    for (h <- 1 until 11) println("Dlog2(" + h + ") = " + dLog(h, 2, 11))
    //    for (h <- 1 until 11) println("Dlog2(" + h + ") = " + BigInt(h).modInverse(11))
    println(dLog(h1, g1, p1))
        println(g1.modPow(14, p1) - h1)
    //    println(dLog(13, 3, 17))
    //    println(dLog(2, 2, 11))
    //    println(ord(2, 11))
    //    println(ord(2, 35))
    //        println(ord(g, p))
  }


  def dLog(h: BigInt, g: BigInt, p: BigInt): BigInt = {
    val B = (p - 1 / 2).toInt
    //math.pow(2, 20).toInt
    val gB = g.modPow(B, p)
    val leftMap = (0 until B).map(x => (modMultiply(h, g.modPow(x, p).modInverse(p), p), x))
    val table = collection.mutable.Map[BigInt, Int]()
    leftMap.foreach(kv => table.put(kv._1, kv._2))

    (0 until B).foreach(x => {
      if (table.contains(gB.modPow(x, p))) {
        val badLog: BigInt = x * B + table(gB.modPow(x, p))
        return badLog

      }
    })

    0
  }

  def ord(g: BigInt, p: BigInt): BigInt = {
    var i = BigInt(1)
    var gx = g
    while (gx % p != BigInt(1)) {
      i += 1
      gx *= g
      gx %= p
      println(i)
    }
    return i
  }

  def modMultiply(num1: BigInt, num2: BigInt, p: BigInt): BigInt = (num1 * num2) % p

  def pow(base: BigInt, to: BigInt, p: BigInt): BigInt = {
    if (to == BigInt(1)) base
    if (to % 2 == BigInt(0)) pow(base * base, to / 2, p)
    else base * pow(base, to - 1, p)
  }

}
