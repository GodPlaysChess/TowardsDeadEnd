package crypto.week02

import crypto.week01.Decrypter

object Hw02 {

  def main(args: Array[String]) {
    val dec = new Decrypter
    println(dec.normal("81"))

    // Question 1
    val cbcKey = "140b41b22a29beb4061bda66b6747e14"
    val cbcCypherText = "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee\2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"

  }

}
