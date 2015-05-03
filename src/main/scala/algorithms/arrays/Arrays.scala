package algorithms.arrays

/**
 * Created by Gleb on 11/2/2014.
 */
object Arrays {

  /**
   * O(n) time / O(n) space complexity
   * returns one of consecutive subarrays, which has the max sum of its elements amongst all consecutive subarrays of input
   */
  def maxSubarray(input: Array[Int]): Array[Int] = {
    def maxS(low: Int, high: Int): (Int, Int, Int) = {
      if (high == low) (low, high, input(low))
      else {
        val mid: Int = (low + high) / 2
        val (leftlow, lefthigh, leftsum) = maxS(low, mid)
        val (rightlow, righthigh, rightsum) = maxS(mid + 1, high)
        val (crosslow, crosshigh, crosssum) = findMaxCrossingSubarray(input, low, mid, high)
        if (leftsum >= rightsum && leftsum >= crosssum) (leftlow, lefthigh, leftsum)
        else if (rightsum >= leftsum && rightsum >= crosssum) (rightlow, righthigh, rightsum)
        else (crosslow, crosshigh, crosssum)
      }
    }
    val (from, to, _) = maxS(0, input.length - 1)
    input.slice(from, to + 1)
  }

  private def findMaxCrossingSubarray(array: Array[Int], low: Int, mid: Int, high: Int): (Int, Int, Int) = {
    var leftSum = Int.MinValue
    var sum = 0
    var maxLeft = 0
    var maxRight = 0
    for (i <- mid to low by (-1)) {
      sum += array(i)
      if (sum > leftSum) {
        leftSum = sum
        maxLeft = i
      }
    }
    var rightSum = Int.MinValue
    sum = 0
    for (i <- mid + 1 to high) {
      sum += array(i)
      if (sum > rightSum) {
        rightSum = sum
        maxRight = i
      }
    }
    (maxLeft, maxRight, leftSum + rightSum)
  }

  def permutations[A](li: List[A]): List[List[A]] = {
    def step(li: List[A], th: List[List[A]]): List[List[A]] = li match {
      case Nil => th
      case l@(x :: xs) => l.flatMap(el => step(l.filterNot(_ == el), th.map(s => el :: s)))

    }
    step(li, List(List.empty))
  }

  def main(args: Array[String]) {
    println(permutations(List(1, 2, 3)))
  }


}
