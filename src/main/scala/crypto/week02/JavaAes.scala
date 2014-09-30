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
    val decodedValue: Array[Byte] = hexStringToByteArray(encryptedText)
    val decValue: Array[Byte] = cipher.doFinal(decodedValue)
    val decryptedValue: String = new String(decValue)
    decryptedValue
  }

  private def generateKey: Key = {
   new SecretKeySpec(keyValue, algorithm)
  }

  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length()
    val data = new Array[Byte](len / 2)
    (0 until len).by(2).foreach(i =>  data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).asInstanceOf[Byte])
    data
  }

  def main(args: Array[String]) {
    val decryptedText: String = decrypt("4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81")
    System.out.println("Decrypted Text : " + decryptedText)
  }

  val algorithm: String = "AES"
  val hexKey: String = "140b41b22a29beb4061bda66b6747e14"
  val iv: Array[Byte] = hexStringToByteArray("4ca00ff4c898d61e1edbf1800618fb28")
  var keyValue: Array[Byte] = hexKey.sliding(2,2).map(Integer.parseInt(_, 16).toByte).toArray

}