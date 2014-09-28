package crypto

import crypto.week01.{Decrypter, weekOne}
import org.junit.Test

class CryptoTest {

  @Test
  def split(): Unit = {
    val message1 = "hello gleb"
    val message2 = "goood bye"
    val message3 = "n"
    val message4 = "23asdd32"
    println(weekOne.sortByMostOccuredChars(message2))

    assert(weekOne.splitBy2(message1) == List("he", "ll", "o ", "gl", "eb"), "actual splitting " + weekOne.splitBy2(message1))
    assert(weekOne.splitBy2(message2) == List("go", "oo", "d ", "by", "e"), "actual splitting " + weekOne.splitBy2(message2))
    assert(weekOne.splitBy2(message3) == List("n"), "actual splitting " + weekOne.splitBy2(message3))
    assert(weekOne.splitBy2(message4) == List("23", "as", "dd", "32"), "actual splitting " + weekOne.splitBy2(message4))
  }

  @Test
  def makeAllPossibleStrings() = {
    assert(weekOne.generateAllPossiblePlainMessage(List(Set("ab"), Set("cd"))) == List("ac", "bc", "ad", "bd"), weekOne.generateAllPossiblePlainMessage(List(Set("ab"), Set("cd"))))
    assert(weekOne.generateAllPossiblePlainMessage(List(Set("ab", "xy"), Set("cd"))) == List("ac", "bc", "xc", "yc" ,"ad", "bd", "xd", "yd"), weekOne.generateAllPossiblePlainMessage(List(Set("ab"), Set("cd"))))
  }

  @Test
  def generateAllPossiblePairs() = {
    assert(weekOne.generateAllPossiblePairs(List(Set("ab"), Set("cd"))) == List(("ac", "bd"), ("ad", "bc")), weekOne.generateAllPossiblePairs(List(Set("ab"), Set("cd"))))
  }

  @Test
  def hex() = {
    val dec = new Decrypter
    assert(dec.hex("abcdef") == "616263646566")
    assert(dec.hex("abcdefdfdAASDFASD") == "6162636465666466644141534446415344")
    assert(dec.hex("cdefdfdAASDFASDQWEIOUF89") == "636465666466644141534446415344515745494f55463839")
    assert(dec.hex("!wd3fdfdAASDFASDQWEI2OUF89") == "2177643366646664414153444641534451574549324f55463839")
    assert(dec.hex("aqw[pfldfm,vdilf;fes") == "6171775b70666c64666d2c7664696c663b666573")

  }

  @Test
  def toNormal() = {
    val dec = new Decrypter
    assert(dec.normal("616263646566") == "abcdef", dec.normal("616263646566"))
    assert(dec.normal("6162636465666466644141534446415344") == "abcdefdfdAASDFASD")
    assert(dec.normal("636465666466644141534446415344515745494f55463839") == "cdefdfdAASDFASDQWEIOUF89")
    assert(dec.normal("2177643366646664414153444641534451574549324f55463839") == "!wd3fdfdAASDFASDQWEI2OUF89")
    assert(dec.normal("6171775b70666c64666d2c7664696c663b666573") == "aqw[pfldfm,vdilf;fes")
  }

  @Test
  def myXor() = {
    val dec = new Decrypter
    assert(dec.xor("616263646566", "dfd1fd1f") == "6162bcb59879", dec.xor("616263646566", "dfd1fd1f"))
    assert(dec.xor("cd", "a1") == "6c", dec.xor("cd", "a1"))
    assert(dec.xor("aa", "aa") == "00", dec.xor("aa", "aa"))
    assert(dec.xor("a121", "aadd34") == "aa7c15", dec.xor("a121", "aadd34"))
    assert(dec.xor("aadd34", "a121") == "aa7c15", dec.xor("a121", "aadd34"))
  }

}
