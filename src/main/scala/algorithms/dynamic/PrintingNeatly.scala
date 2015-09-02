package algorithms.dynamic

/**
 * 02-Sep-2015.
 */
class PrintingNeatly {
  //Sum of the cubes of characters left is min (Except the last line)

  /*
  Some thoughts about the algorithm:
  Best solution for N-word sequence is the Best solution for N-1 word sequence +
    1) if it is still fit on the last line - then put it there
    2) if it does not fit, then we rearrange the words somehow
      so we have to put them on the new line (n), and may be shift a word from (n - 2) -> (n - 1)

      AS an example consider          (length is 10)
      3 3 4
      5
      7 2

      When 7 is added and does not fit, we must shift 4 to the next line.
      Essentially it happens, because nth line becomes n-1th line, and suddenly begins to add a score
  * */


  val solution: scala.collection.mutable.Map[Int, Seq[Seq[String]]] = scala.collection.mutable.Map.empty


  def printingNeatly(words: IndexedSeq[String], length: Int): Seq[Seq[String]] = {
    ???
  }

  def sub(words: IndexedSeq[String], wordNum: Int, lineSize: Int): Unit = {
    if (wordNum <= words.size) {
      val word = words(wordNum)
      if (!solution.contains(wordNum - 1)) sub(words, wordNum - 1, lineSize) // fill the subproblem solution
      val lastLine = solution(wordNum - 1).last
      if (word.length < spaceLeft(lastLine, lineSize)) {
        // just append the word to the end of the last string
        solution.put(wordNum, solution(wordNum - 1) map (_ :+ word))
      } else {
        // if it does not fit, do regrouping based on point criterion
        val veryLastLine = Seq(word)
        val lastLine = solution(wordNum - 1).last
        val previousLine = solution(wordNum - 1).init.last

        val rearrangedSolution: Seq[Seq[String]] = ???
        solution.put(wordNum, rearrangedSolution)
      }
    }
  }

  def points(words: Seq[Seq[String]], lineSize: Int): Double = {
   words.reverse.tail.foldLeft(0d)((score, seq) ⇒ score + math.pow(seq.reduce(_.length + 1 + _.length), 3))
  }

  private def spaceLeft(words: Seq[String], lineSize: Int): Int = {
    lineSize - words.reduce(_.length + 1 + _.length)
  }


}
