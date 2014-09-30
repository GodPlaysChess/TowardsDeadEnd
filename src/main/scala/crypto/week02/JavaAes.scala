package crypto.week02

import java.security.Key
import javax.crypto.Cipher
import javax.crypto.spec.{IvParameterSpec, SecretKeySpec}

object JavaAes {
//  def encrypt(plainText: String): String = {
//    val key: Key = generateKey
//    val chiper: Cipher = Cipher.getInstance(algorithm)
//    chiper.init(Cipher.ENCRYPT_MODE, key)
//    val encVal: Array[Byte] = chiper.doFinal(plainText.getBytes)
//    val encryptedValue: String = new BASE64Encoder().encode(encVal)
//    encryptedValue
//  }

  def decrypt(encryptedText: String, iv: Array[Byte], keyValue: Array[Byte]): String = {
    val key: Key = generateKey(keyValue)
    val cipher: Cipher = Cipher.getInstance("AES/CBC/PKCS5Padding")
    val ivSpec = new IvParameterSpec(iv)
    cipher.init(Cipher.DECRYPT_MODE, key, ivSpec)
    val decodedValue: Array[Byte] = hexStringToByteArray(encryptedText)
    val decValue: Array[Byte] = cipher.doFinal(decodedValue)
    val decryptedValue: String = new String(decValue)
    decryptedValue
  }

  def decCtr(encryptedText: String, iv: Array[Byte], keyValue: Array[Byte]): String = {
    val key: Key = generateKey(keyValue)
    val cipher: Cipher = Cipher.getInstance("AES/CTR/NoPadding")
    val ivSpec = new IvParameterSpec(iv)
    cipher.init(Cipher.DECRYPT_MODE, key, ivSpec)
    val decodedValue: Array[Byte] = hexStringToByteArray(encryptedText)
    val decValue: Array[Byte] = cipher.doFinal(decodedValue)
    val decryptedValue: String = new String(decValue)
    decryptedValue
  }

  private def generateKey(keyValue: Array[Byte]): Key = {
   new SecretKeySpec(keyValue, algorithm)
  }

  def hexStringToByteArray(s: String): Array[Byte] = {
    val len = s.length()
    val data = new Array[Byte](len / 2)
    (0 until len).by(2).foreach(i =>  data(i / 2) = ((Character.digit(s.charAt(i), 16) << 4) + Character.digit(s.charAt(i + 1), 16)).asInstanceOf[Byte])
    data
  }

  def main(args: Array[String]) {
    val m1 = "4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee2e4b7465d5290d0c0e6c6822236e1daafb94ffe0c5da05d9476be028ad7c1d81"
    val iv1: Array[Byte] = hexStringToByteArray("4ca00ff4c898d61e1edbf1800618fb28")
    val key1: Array[Byte] = hexStringToByteArray("140b41b22a29beb4061bda66b6747e14")
    val m2 = "5b68629feb8606f9a6667670b75b38a5b4832d0f26e1ab7da33249de7d4afc48e713ac646ace36e872ad5fb8a512428a6e21364b0c374df45503473c5242a253"
    val iv2 = hexStringToByteArray("5b68629feb8606f9a6667670b75b38a5")

    val m3 = "69dda8455c7dd4254bf353b773304eec0ec7702330098ce7f7520d1cbbb20fc388d1b0adb5054dbd7370849dbf0b88d393f252e764f1f5f7ad97ef79d59ce29f5f51eeca32eabedd9afa9329"
    val iv3 = hexStringToByteArray("69dda8455c7dd4254bf353b773304eec")
    val key3: Array[Byte] = hexStringToByteArray("36f18357be4dbd77f050515c73fcf9f2")

    val m4 = "770b80259ec33beb2561358a9f2dc617e46218c0a53cbeca695ae45faa8952aa0e311bde9d4e01726d3184c34451"
    val iv4 = hexStringToByteArray("770b80259ec33beb2561358a9f2dc617")


    val decryptedText: String = decrypt(m1, iv1, key1)
    val decryptedText2: String = decrypt(m2, iv2, key1)
    System.out.println("Decrypted Text : " + decryptedText.drop(16))
    System.out.println("Decrypted Text2 : " + decryptedText2.drop(16))
    System.out.println("Decrypted Text3 : " + decCtr(m3, iv3, key3).drop(16))
    System.out.println("Decrypted Text4 : " + decCtr(m4, iv4, key3).drop(16))
  }

  val algorithm: String = "AES"

  def getKeyValue(key: String): Array[Byte] = key.sliding(2,2).map(Integer.parseInt(_, 16).toByte).toArray

}