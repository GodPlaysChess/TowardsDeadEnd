package crypto.week01

class Decrypter {

  def hex(message: String): String = {
    message.toCharArray.map(_.toInt.toHexString).reduce(_ + _)
  }

  def normal(hexedMessage: String): String = {
    hexedMessage.sliding(2, 2).map(Integer.parseInt(_, 16)).map(_.toChar.toString).reduce(_ + _)
  }

  def xor(key: String, mess: String): String = {
    val result = new StringBuilder
    val countZeroes = key.length - mess.length
    val zeroes = String.valueOf(Array.fill[Char](math.abs(countZeroes)) {'0'})

    def myxor(key: String, mess: String, result: StringBuilder): StringBuilder = {
      if (key.length == 0) return result
      val (k1, k2) = key.splitAt(2)
      val (m1, m2) = mess.splitAt(2)
      var xored = (Integer.parseInt(k1, 16) ^ Integer.parseInt(m1, 16)).toHexString
      if (xored.length == 1) xored += "0"
      myxor(k2, m2, result.append(xored))
    }
    if (countZeroes < 0) myxor(zeroes + key, mess, result).toString()
    else myxor(key, zeroes + mess, result).toString()
  }

  def xorStolen(hexOne: String, hexAnother: String): String = {
    val iterator1 = hexOne.sliding(2, 2)
    val iterator2 = hexAnother.sliding(2, 2)
    val result = new StringBuilder

    if (hexAnother.length > hexOne.length) {
      while (iterator1.hasNext) {
        val i = Integer.toString(Integer.parseInt(iterator1.next(), 16) ^
          Integer.parseInt(iterator2.next(), 16), 16)

        if (i.length == 1) result.append("0")
        result.append(i)

      }

      while (iterator2.hasNext) result.append(iterator2.next())
    }
    else {
      while (iterator2.hasNext) {
        val i = Integer.toString(Integer.parseInt(iterator1.next(), 16) ^
          Integer.parseInt(iterator2.next(), 16), 16)

        if (i.length == 1) result.append("0")
        result.append(i)

      }

      while (iterator1.hasNext) result.append(iterator1.next())
    }

    result.toString()
  }


}
