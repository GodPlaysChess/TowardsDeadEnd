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
    2) if it does not fit, then we rearrange the words somehow.
  * */


  val solution: Seq[Seq[String]] = Seq.empty


  def printingNeatly(words: IndexedSeq[String], length: Int): Seq[Seq[String]] = {
    if (words.foldLeft(0)(_ + _.length) < length) Seq(words)  // if the rest fits on one line
    else {
      solution.last
    }
  }

  def sub(words: IndexedSeq[String], length: Int): Unit = {

  }

  def points(words: Seq[Seq[String]], lineSize: Int): Double = {
   words.reverse.tail.foldLeft(0d)((score, seq) ⇒ score + math.pow(seq.reduce(_.length + 1 + _.length), 3))
  }

  private def spaceLeft(words: Seq[String], lineSize: Int): Int = {
    lineSize - words.reduce(_.length + 1 + _.length)
  }


}
