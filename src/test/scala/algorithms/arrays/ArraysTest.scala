package algorithms.arrays

import org.junit.Test

/**
 * Created by Gleb on 11/2/2014.
 */
class ArraysTest {

  @Test
  def maxSubarray() = {
    val input = Array[Int](10, -29, 13, 23, 12, -22, 40, 2)
    assert(Arrays.maxSubarray(input).toList == input.slice(2, 8).toList)
  }


}
