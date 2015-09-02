package algorithms.dynamic

/**
 * 02-Sep-2015.
 * Working but dirty solution. Does not have time and desire to clean it up.
 * since algorithm still holds.
 * This algorithm has square complexity, but in my awesome implementation it takes n^3^.
 * #reorginize last two lines can take linear time, so overall time would be n^2^
 */
class PrintingNeatly {
  //Sum of the cubes of characters left is min (Except the last line)
  //

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


  val solution: scala.collection.mutable.Map[Int, Seq[Seq[Int]]] = scala.collection.mutable.Map.empty

  // Algorithm invokes n recurrences, each takes n^2 , so n^3 - overral time complexity
  def printingNeatly(words: IndexedSeq[Int], length: Int): Seq[Seq[Int]] = {
    sub(words, words.length - 1, length)
    solution(words.length - 1)
  }
                             // O(n^2)
  private def reorganizeLastTwoLines(s1: Seq[Int], s2: Seq[Int], lineSize: Int): (Seq[Int], Seq[Int]) = {
    val all = s2 ++ s1
    (1 to all.length - 1) map all.splitAt minBy (l ⇒ pointsOf2Sequences(l._1, l._2, lineSize))  // map takes O(n), then for every N -> we are calculating points (N)
  }

  def sub(words: IndexedSeq[Int], wordNum: Int, lineSize: Int): Unit = {
    if (wordNum == 0) solution.put(0, Seq(Seq(words.head)))                                                    // const
    else {
      val word = words(wordNum)
      if (!solution.contains(wordNum - 1)) sub(words, wordNum - 1, lineSize)
      val prevSolution: Seq[Seq[Int]] = solution(wordNum - 1)
      // fill the subproblem solution
      val lastLine = prevSolution.last                                                                         // const for Vec
      val linesAmount = prevSolution.size                                                                      // const
      if (word < spaceLeft(lastLine, lineSize)) {
        // just append the word to the end of the last Int
        solution.put(wordNum, prevSolution updated (linesAmount - 1, lastLine :+ word))
      } else {
        // if it does not fit, do regrouping based on point criterion
        val veryLastLine = Seq(word)
        if (linesAmount > 1) {
          val previousLine: Seq[Int] = prevSolution(linesAmount - 2)
          val (betterPrev, betterLast) = reorganizeLastTwoLines(lastLine, previousLine, lineSize)
          val rearrangedSolution = (prevSolution updated (linesAmount - 2, betterPrev) updated (linesAmount - 1, betterLast)) :+ veryLastLine
          solution.put(wordNum, rearrangedSolution)
        } else {
          solution.put(wordNum, lastLine +: Seq(veryLastLine))
        }

      }
    }
  }

  def pointsOf2Sequences(l1: Seq[Int], l2: Seq[Int], lineSize: Int): Double = {
    points(Seq(l1, l2, Seq.empty), lineSize)
  }
                                    // O(N)
  def points(words: Seq[Seq[Int]], lineSize: Int): Double = {
   words.reverse.tail.foldLeft(0d)((score, seq) ⇒ score + math.pow(spaceLeft(seq, lineSize), 3))
  }

  private def spaceLeft(words: Seq[Int], lineSize: Int): Int = {
    lineSize - (words reduce(_ + _ + 1))
  }


}

object PrintingNeatly {
  def main(args: Array[String]) {
    val input = Vector(2, 2, 4, 4, 6)
    val P = new PrintingNeatly()
    val solution = P.printingNeatly(input, 10)
    println(solution)
    println(P.points(solution, 10))
  }
}
