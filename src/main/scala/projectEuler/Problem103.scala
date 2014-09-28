package projectEuler

import scala.collection.SortedSet

object Problem103 {
  def main(args: Array[String]) {
    println(findNextOptimumSet(SortedSet(1)))
  }

  def findNextOptimumSet(set: SortedSet[Int]): SortedSet[Int] = {
    var guess = set.max - set.min + 1
    while (set.contains(guess)) guess += 1
    println("guess: " + guess + " set: " + set)
    if (checkSpeciality(set, guess)) set + guess
    else set
    //do something

  }

  def checkSpeciality(set: SortedSet[Int], candidate: Int): Boolean =
    !set.contains(candidate) && set.map(x => candidate + x).intersect(set).isEmpty


}
