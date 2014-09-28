package crypto.week02

import crypto.week01.Decrypter

class AesDecrypter {
  val decrypter = new Decrypter
  //IV 16 bytes random
  // AES key
  // cypherText

  // return plaintext from CBC key
  /**
   * CBC:
   * E(k, IV xor m[0])
   * E(k, c0 xor m[1])
   * etc...
   *
   *
   *
   * m0 =  D(k, c[0]) xor IV
   * m1 =  D(k, c[1]) xor c[0]
   * etc...
   * m[0] || m[n]
   * m[n] || pad at the end n n n ... n
   */

  def decrypt(cypher: String, key: String): String = {
    val (iv, message) = cypher.splitAt(32)
    val messages = message.grouped(32)
//    val decryptedMessages: String = messages.foldLeft(iv)(
//      (cypher, acc) => acc + (JavaAes.decrypt(cypher)  acc.substring(acc.length - 32, acc.length))
//    )
    /* remove bytes from the last block dePad*/
    // deHex
    //return
    "no"
  }

  "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"
}
