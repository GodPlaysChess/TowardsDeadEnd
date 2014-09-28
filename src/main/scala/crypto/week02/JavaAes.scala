package crypto.week02

import java.security.Key
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

import sun.misc.BASE64Encoder

object JavaAes {
  def encrypt(plainText: String): String = {
    val key: Key = generateKey
    val chiper: Cipher = Cipher.getInstance(algorithm)
    chiper.init(Cipher.ENCRYPT_MODE, key)
    val encVal: Array[Byte] = chiper.doFinal(plainText.getBytes)
    val encryptedValue: String = new BASE64Encoder().encode(encVal)
    encryptedValue
  }

  def decrypt(encryptedText: String): String = {
    val key: Key = generateKey
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val ivSpec = new IvParameterSpec(iv)
    cipher.init(Cipher.DECRYPT_MODE, key, ivSpec)
    val decodedValue: Array[Byte] = encryptedText.getBytes //new BASE64Decoder().decodeBuffer(encryptedText)
    val decValue: Array[Byte] = cipher.doFinal(decodedValue)
    val decryptedValue: String = new String(decValue)
    decryptedValue
  }

  private def generateKey: Key = {
   new SecretKeySpec(keyValue, algorithm)
  }

  def main(args: Array[String]) {
    val plainText: String = "Password"
    val encryptedText: String = encrypt(plainText)
    val decryptedText: String = decrypt("4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81")
    System.out.println("Plain Text : " + plainText)
    System.out.println("Encrypted Text : " + encryptedText)
    System.out.println("Decrypted Text : " + decryptedText)
  }

  val algorithm: String = "AES"
  val hexKey: String = "140b41b22a29beb4061bda66b6747e14"
  val iv: Array[Byte] = "4ca00ff4c898d61e1edbf1800618fb28".sliding(2,2).map(java.lang.Byte.decode(_).toByte).toArray
  iv.foreach(print(_))
//  val iv = new BASE64Decoder().decodeBuffer("4ca00ff4c898d61e1edbf1800618fb28")
//  println(iv.size + "  " + iv)
//  val keyValue = new BASE64Decoder().decodeBuffer(hexKey)
//  println(keyValue.size + "  " + keyValue)
  var keyValue: Array[Byte] = hexKey.sliding(2,2).map(Integer.parseInt(_, 16).toByte).toArray
  keyValue.foreach(print(_))
//  println(keyValue + " key")


}