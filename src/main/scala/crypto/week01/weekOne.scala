package crypto.week01

object weekOne {
  val dec = new Decrypter
  /* symbols which supposively used in messages */
  val alphabet = "abcdefghijklmnopqrstuvwxyz "
  val fullAlphabet = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ "                                                                             //??|
  var finalKey = "66 39 6e 89 c9 db d8 cc 98 74 35 2a cd 63 95 10 2e af ce 78 aa 7f ed 28 a0 7f 6b c9 8d 29 c5 0b 69 b0 33 9a 19 f8 aa 40 1a 9c 6d 70 8f 80 c0 66 c7 6f 00 00 12 31 00 00 00 00 2 00 5b 00 00 77 33 5d 00 00 00 00 00 43 3a 6b 26 00"
  val len = 30                                                                    //23    25 26             31 32 33

  def main(args: Array[String]) {

    val space = " "
    val hexedAlphabet = dec.hex(alphabet)
    val lettersXorSpace = alphabet.groupBy(letter => dec.xor(dec.hex(letter.toString), dec.hex(space)))

    /* given cyphered messages */
    val c1 = "315c4eeaa8b5f8aaf9174145bf43e1784b8fa00dc71d885a804e5ee9fa40b16349c146fb778cdf2d3aff021dfff5b403b510d0d0455468aeb98622b137dae857553ccd8883a7bc37520e06e515d22c954eba5025b8cc57ee59418ce7dc6bc41556bdb36bbca3e8774301fbcaa3b83b220809560987815f65286764703de0f3d524400a19b159610b11ef3e"
    val c2 = "234c02ecbbfbafa3ed18510abd11fa724fcda2018a1a8342cf064bbde548b12b07df44ba7191d9606ef4081ffde5ad46a5069d9f7f543bedb9c861bf29c7e205132eda9382b0bc2c5c4b45f919cf3a9f1cb74151f6d551f4480c82b2cb24cc5b028aa76eb7b4ab24171ab3cdadb8356f"
    val c3 = "32510ba9a7b2bba9b8005d43a304b5714cc0bb0c8a34884dd91304b8ad40b62b07df44ba6e9d8a2368e51d04e0e7b207b70b9b8261112bacb6c866a232dfe257527dc29398f5f3251a0d47e503c66e935de81230b59b7afb5f41afa8d661cb"
    val c4 = "32510ba9aab2a8a4fd06414fb517b5605cc0aa0dc91a8908c2064ba8ad5ea06a029056f47a8ad3306ef5021eafe1ac01a81197847a5c68a1b78769a37bc8f4575432c198ccb4ef63590256e305cd3a9544ee4160ead45aef520489e7da7d835402bca670bda8eb775200b8dabbba246b130f040d8ec6447e2c767f3d30ed81ea2e4c1404e1315a1010e7229be6636aaa"
    val c5 = "3f561ba9adb4b6ebec54424ba317b564418fac0dd35f8c08d31a1fe9e24fe56808c213f17c81d9607cee021dafe1e001b21ade877a5e68bea88d61b93ac5ee0d562e8e9582f5ef375f0a4ae20ed86e935de81230b59b73fb4302cd95d770c65b40aaa065f2a5e33a5a0bb5dcaba43722130f042f8ec85b7c2070"
    val c6 = "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd2061bbde24eb76a19d84aba34d8de287be84d07e7e9a30ee714979c7e1123a8bd9822a33ecaf512472e8e8f8db3f9635c1949e640c621854eba0d79eccf52ff111284b4cc61d11902aebc66f2b2e436434eacc0aba938220b084800c2ca4e693522643573b2c4ce35050b0cf774201f0fe52ac9f26d71b6cf61a711cc229f77ace7aa88a2f19983122b11be87a59c355d25f8e4"
    val c7 = "32510bfbacfbb9befd54415da243e1695ecabd58c519cd4bd90f1fa6ea5ba47b01c909ba7696cf606ef40c04afe1ac0aa8148dd066592ded9f8774b529c7ea125d298e8883f5e9305f4b44f915cb2bd05af51373fd9b4af511039fa2d96f83414aaaf261bda2e97b170fb5cce2a53e675c154c0d9681596934777e2275b381ce2e40582afe67650b13e72287ff2270abcf73bb028932836fbdecfecee0a3b894473c1bbeb6b4913a536ce4f9b13f1efff71ea313c8661dd9a4ce"
    val c8 = "315c4eeaa8b5f8bffd11155ea506b56041c6a00c8a08854dd21a4bbde54ce56801d943ba708b8a3574f40c00fff9e00fa1439fd0654327a3bfc860b92f89ee04132ecb9298f5fd2d5e4b45e40ecc3b9d59e9417df7c95bba410e9aa2ca24c5474da2f276baa3ac325918b2daada43d6712150441c2e04f6565517f317da9d3"
    val c9 = "271946f9bbb2aeadec111841a81abc300ecaa01bd8069d5cc91005e9fe4aad6e04d513e96d99de2569bc5e50eeeca709b50a8a987f4264edb6896fb537d0a716132ddc938fb0f836480e06ed0fcd6e9759f40462f9cf57f4564186a2c1778f1543efa270bda5e933421cbe88a4a52222190f471e9bd15f652b653b7071aec59a2705081ffe72651d08f822c9ed6d76e48b63ab15d0208573a7eef027"
    val c10 = "466d06ece998b7a2fb1d464fed2ced7641ddaa3cc31c9941cf110abbf409ed39598005b3399ccfafb61d0315fca0a314be138a9f32503bedac8067f03adbf3575c3b8edc9ba7f537530541ab0f9f3cd04ff50d66f1d559ba520e89a2cb2a83"
    /* this one needs to decrypt */
    val t = "32510ba9babebbbefd001547a810e67149caee11d945cd7fc81a05e9f85aac650e9052ba6a8cd8257bf14d13e6f0a803b54fde9e77472dbff89d71b57bddef121336cb85ccb8f3315f4b52e301d16e9f52f904"
    // t = "The .
    val cypheredMessages = List(c1, c2, c3, c4, c5, c6, c7, c8, c9, c10, t)


    //    val knownKey = guessFullKey(cypheredMessages.map(m => m.take(len * 2))) //.reduce(_ + _)
    //    println(knownKey mkString (" "))
    //    println(dec.normal(dec.xor(knownKey.reduce(_ + _), t)))

    println("key = " + (0xf0 ^ 'p').toHexString)
    println("key = " + (0xa8 ^ 'h').toHexString)
    println("key = " + (0xb5^ 'r').toHexString)
    println("key = " + (0x4f ^ ' ').toHexString)


//    println(dec.normal((0xdb ^ 0xb5).toHexString))

    val partKey = finalKey.filter(c => c != ' ')
//    println(dec.normal(dec.xor(partKey, c4.take(partKey.length)))) //c4 h e

    //    val knownKeyAsString = knownKey.reduce(_ + _)
    //    println(knownKeyAsString)
    //    println(dec.xor(c1, knownKeyAsString))
    cypheredMessages.foreach(message => println(dec.normal(dec.xor(partKey, message.take(partKey.length)))))


    /* all xored messages == cyphered xored messages for analysis */
    val xoredMessages = getXoredMessages(dec, cypheredMessages)
    /* m1^m2 = x1. This map represents x1 => frequency(x1) in xored messages */
    val cypheredAlphabet = sortByMostOccuredChars(xoredMessages.reduce(_ + _))

    /* values of alphabet xored with itself */
    val xoredCharsMap = getXoredAlphabetCharsMap(hexedAlphabet)

    /* starting decrypting point */
    //    val tVsM = dec.xor(t, c2) //cx is not matter at all

    /* take, say first 10 letters of this message and try bruteforce decrypt it */
    //    val bundle = splitBy2(tVsM.take(4 * 2)) // each letter is represented by 2 symbols
    /* printout(or write to file) all 10-letter words (should be no more than 20^10 combinations */
    //    val listOfPossiblePlainMessages = generateAllPossiblePlainMessage(bundle.map(code => xoredCharsMap(code)))


    /* proceed to the next 10 letter words. If perfomance is an issue - make smaller increment (6 letters, for instance) */
    //    listOfPossiblePlainMessages.foreach(println(_))                                      //now generating all possible 8-lettered words
    //crap, have already 17K 4lettered words reduced from 531441 words.. I am definitely doing something wrong
    /* having a message, determine a key, by message ^ cypheredMessage */

  }

  def getXoredAlphabetCharsMap(hexedAlphabet: String): scala.collection.mutable.Map[String, Set[String]] = {
    val xoredCharsMap = scala.collection.mutable.Map[String, Set[String]]()
    for (let1 <- hexedAlphabet.sliding(2, 2); let2 <- hexedAlphabet.sliding(2, 2)) {
      if (xoredCharsMap.contains(dec.xor(let1, let2))) {
        val set = xoredCharsMap.get(dec.xor(let1, let2))
        xoredCharsMap.put(dec.xor(let1, let2), set.get + (dec.normal(let1) + dec.normal(let2)))
      } else {
        xoredCharsMap.put(dec.xor(let1, let2), Set(dec.normal(let1) + dec.normal(let2)))
      }
    }
    xoredCharsMap
  }

  def getXoredMessages(dec: Decrypter, cypheredMessages: List[String]): List[String] = {
    for {
      cm1 <- cypheredMessages
      cm2 <- cypheredMessages
      if cm1 != cm2
    } yield {
      dec.xor(cm1, cm2)
    }
  }

  /**
   * Bruteforce generation of a single message. Basically must provide two messages as output. (deciphered c1, and deciphered c2)
   **/
  def generateAllPossiblePlainMessage(lists: List[Set[String]]): List[String] = {
    def appendNextCharToTheString(result: List[String], rest: List[Set[String]]): List[String] = {
      /* quit condition*/
      if (rest.isEmpty) return result

      /* initializing routine */
      val candidates = rest.head // this is set of two-char entry, such as "a-q", "e-f", etc..
      val passFurther = rest.tail
      /* extracting candidates to the list of characters (YES Yet erasing information about "Another" message) */
      val candidateString: String = candidates.reduce(_ + _)

      /* given a list of incomplete strings, I need to expand it to match exact the number of combinations */
      //f.e. List("a","b") needs to become List ("a", "a,", "b", "b") to match candidates "c-d"
      /* go over them and populate them into expanded result list */
      val populatedResult: IndexedSeq[String] = for {
        char <- candidateString
        prefix <- result // must create a string if empty
      } yield {
        prefix.concat(char.toString)
      }
      // which leads to List("ac", "ad", "bc", bd") for next iteration
      appendNextCharToTheString(populatedResult.toSet.toList, passFurther)
    }
    // starting with empty list and then go char by char
    appendNextCharToTheString(List[String](""), lists)
  }

  /**
   * Bruteforce generation of a single message. Basically must provide two messages as output. (deciphered c1, and deciphered c2)
   **/
  def generateAllPossiblePairs(lists: List[Set[String]]): List[(String, String)] = {
    def appendNextCharToTheString(result: List[(String, String)], rest: List[Set[String]]): List[(String, String)] = {
      /* quit condition*/
      if (rest.isEmpty) return result

      /* initializing routine */
      val candidates = rest.head // this is set of two-char entry, such as "a-q", "e-f", etc..
      val passFurther = rest.tail
      /* extracting candidates to the list of characters (YES Yet erasing information about "Another" message) */
      //      val candidateString: String = candidates.reduce(_+_)

      /* given a list of incomplete strings, I need to expand it to match exact the number of combinations */
      //f.e. List("a","b") needs to become List ("a", "a,", "b", "b") to match candidates "c-d"
      /* go over them and populate them into expanded result list */
      val populatedResult: Set[(String, String)] = for {
        pairOfChars <- candidates
        prefix <- result // must create a string if empty
      } yield {
        (prefix._1.concat(pairOfChars.head.toString),
          prefix._2.concat(pairOfChars.tail.toString))
        (prefix._2.concat(pairOfChars.head.toString),
          prefix._1.concat(pairOfChars.tail.toString))
      }
      // which leads to List("ac", "ad", "bc", bd") for next iteration
      appendNextCharToTheString(populatedResult.toList, passFurther)
    }
    // starting with empty list and then go char by char
    appendNextCharToTheString(List[(String, String)](("", "")), lists)
  }

  def sortByMostOccuredChars(message: String): List[(String, Int)] = {
    splitBy2(message).groupBy(m => m).mapValues(_.size).toList.sortBy(_._2)
  }

  /**
   * converts string to a list of strings, 2 characters length each.
   */
  def splitBy2(message: String): List[String] = {
    def splitBy2(message: List[Char], result: List[String]): List[String] = message match {
      case Nil => result
      case x :: y :: xs => splitBy2(xs, result :+ x.toString.concat(y.toString))
      case x :: xs => result :+ x.toString
    }
    splitBy2(message.toList, List())
  }

  def guessKeyChar(messages: List[String], position: Int): Map[List[String], String] = {
    val keyCharAscii = (0x00 to 0x7f).map(_.toHexString)
    //    val keyCharSpace: String = keyCharAscii.map(num => dec.normal(num.toHexString)).reduce(_ + _)
    //    val keyCharSpace = "`[]{}';.,/\"abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890!@#$%^&*()="
    val firstletters = keyCharAscii.groupBy(hexString => messages.map(
      message => dec.normal(dec.xor(hexString, message.substring(position * 2 - 2, position * 2))))
    )


    //    val firstletters = keyCharSpace.groupBy(char => messages.map(
    //      message => dec.normal(dec.xor(dec.hex(char.toString), message.substring(position * 2 - 2, position * 2))))
    //    )

    def containAlphabet(string: List[String]): Boolean = {
      string.count(x => !fullAlphabet.contains(x)) < 1
    }

    val reducedFirstLetters = firstletters.filter(map => containAlphabet(map._1))
    reducedFirstLetters.mapValues(seq => if (seq.head.length == 2) seq.head else "0" + seq.head)
  }

  def guessFullKey(messages: List[String]): List[String] = {
    var key = List[String]()

    def appendIfExists(map: Map[List[String], String]): List[String] = {
      if (map.size == 0) List("00")
      else map.values.toList
    }

    (1 to len).foreach(pos => key = key ++ appendIfExists(guessKeyChar(messages, pos)))
    key
  }

}