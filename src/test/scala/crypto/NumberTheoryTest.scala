package crypto

import crypto.week05.NumberTheory
import org.junit.Test

/**
 * Created by GlebP on 10-Oct-2014.
 */
class NumberTheoryTest {

  @Test
  def dLog(): Unit = {
    assert(NumberTheory.dLog(17, 9, 4) == BigInt(6))
    assert(NumberTheory.dLog(17, 3, 2) == BigInt(14))
    assert(NumberTheory.dLog(11, 6, 6) == BigInt(1))
    assert(NumberTheory.dLog(119, 9, 4) == BigInt(14))
  }


}
