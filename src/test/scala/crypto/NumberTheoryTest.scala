package crypto

import crypto.week05.NumberTheory._
import org.junit.Test

/**
 * Created by GlebP on 10-Oct-2014.
 */
class NumberTheoryTest {

  @Test
  def dLogtest(): Unit = {
    assert(dLog(4, 9, 17) == BigInt(6), "actual: " + dLog(17, 9, 4))
    assert(dLog(2, 3, 17) == BigInt(14))
    assert(dLog(6, 6, 11) == BigInt(1))
    assert(dLog(4, 13, 121) == BigInt(12), "actual: " + dLog(4, 13, 121))
  }


}
