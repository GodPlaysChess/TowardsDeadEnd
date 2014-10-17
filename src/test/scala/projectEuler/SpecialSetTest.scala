package projectEuler

import org.junit.Test
import projectEuler.unsolved.Problem103._


import scala.collection.immutable.SortedSet

class SpecialSetTest {


  @Test def checkFailingSpecialityTest() {
    assert(checkSpeciality(SortedSet(1, 2), 2))
  }

  @Test def checkSpecialityTest() {
    assert(checkSpeciality(SortedSet(3, 4), 2))
  }

  @Test
  def optimumSet2() = assert(SortedSet(1, 2) == findNextOptimumSet(SortedSet(1)))

  @Test
  def optimumSet3() = assert(SortedSet(2, 3, 4) == findNextOptimumSet(SortedSet(1, 2)))

  @Test
  def optimumSet4() = assert(SortedSet(3, 5, 6, 7) == findNextOptimumSet(SortedSet(2, 3, 4)))

  @Test
  def optimumSet5() = assert(SortedSet(6, 9, 11, 12, 13) == findNextOptimumSet(SortedSet(3, 5, 6, 7)))

  @Test
  def optimumSet6() = assert(SortedSet(11, 18, 19, 20, 22, 25) == findNextOptimumSet(SortedSet(6, 9, 11, 12, 13)))

}
