package protocol {

import annotation.implicitNotFound

import java.io.{DataOutputStream, ByteArrayOutputStream}

@implicitNotFound(msg = "Could not find a Writes for ${T}")
trait Writes[T] {

  def writes(value: T): Array[Byte]
}

class DataOutputStreamWrites[T](writeValue: (DataOutputStream, T) => Unit) extends Writes[T] {

  def writes(value: T): Array[Byte] = {
    val bos = new ByteArrayOutputStream
    val dos = new DataOutputStream(bos)
    writeValue(dos, value)
    dos.flush()
    val byteArray = bos.toByteArray
    bos.close()
    byteArray
  }
}

object defaults {
  implicit object WritesString extends Writes[String] {
    def writes(value: String) = value.getBytes("UTF-8")
  }
  implicit object WritesLong extends DataOutputStreamWrites[Long](_.writeLong(_))
  implicit object WritesInt extends DataOutputStreamWrites[Int](_.writeInt(_))
  implicit object WritesShort extends DataOutputStreamWrites[Short](_.writeShort(_))
}
}

package crypto {

import protocol.Writes

import javax.crypto.spec.SecretKeySpec
import javax.crypto.Cipher

trait Encryption {
  def encrypt(dataBytes: Array[Byte], secret: String): Array[Byte]
  def decrypt(codeBytes: Array[Byte], secret: String): Array[Byte]

  def encrypt[T:Writes](data: T, secret: String): Array[Byte] = encrypt(implicitly[Writes[T]].writes(data), secret)
  def decrypt[T:Writes](data: T, secret: String): Array[Byte] = decrypt(implicitly[Writes[T]].writes(data), secret)
}

class JavaCryptoEncryption(algorithmName: String) extends Encryption {

  def encrypt(bytes: Array[Byte], secret: String): Array[Byte] = {
    val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
    val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
    encipher.init(Cipher.ENCRYPT_MODE, secretKey)
    encipher.doFinal(bytes)
  }

  def decrypt(bytes: Array[Byte], secret: String): Array[Byte] = {
    val secretKey = new SecretKeySpec(secret.getBytes("UTF-8"), algorithmName)
    val encipher = Cipher.getInstance(algorithmName + "/ECB/PKCS5Padding")
    encipher.init(Cipher.DECRYPT_MODE, secretKey)
    encipher.doFinal(bytes)
  }
}

object DES extends JavaCryptoEncryption("DES")
object AES extends JavaCryptoEncryption("AES")
}

object Main extends App {

  import org.apache.commons.codec.binary.Base64

  import crypto._
  import protocol.defaults._

  def encodeBase64(bytes: Array[Byte]) = Base64.encodeBase64String(bytes)

  println(encodeBase64(DES.encrypt("hoge", "01234567")))
  //=> vyudTtnBJfs=

  println(encodeBase64(AES.encrypt("4ca00ff4c898d61e1edbf1800618fb2828a226d160dad07883d04e008a7897ee", "140b41b22a29beb4061bda66b6747e14")))

  println(encodeBase64(AES.encrypt("hoge", "0123456789012345")))
  //=> QWSouZUMVYMfS86xFyBgtQ==

  println(encodeBase64(DES.encrypt(123L, "01234567")))
  //=> Cqw2ipxTtvIIu122s3wG1w==

  println(encodeBase64(DES.encrypt(123, "01234567")))
  //=> BV+LSCSYmUU=
}