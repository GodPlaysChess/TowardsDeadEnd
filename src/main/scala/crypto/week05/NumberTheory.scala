package crypto.week05

/**
 * Created by Gleb Parakhosnkiy on 10/8/14.
 */
object NumberTheory {
  def main(args: Array[String]) {
    for (a <- 1 to 10000) {
      if ((3 * a + 2) % 19 == 7){
        println("a = " + a)
        return

      }
    }
  }

}
