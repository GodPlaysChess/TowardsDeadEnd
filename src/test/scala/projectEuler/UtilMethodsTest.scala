package projectEuler

import org.junit.Test
import UtilMethods._



class UtilMethodsTest {

  @Test
  def primeDecompositon(): Unit = {
    assert(primeDecompositionFor(2) == List(2), "Actual value is " + primeDecompositionFor(2))
    assert(primeDecompositionFor(1) == List(), "Actual value is empty list")
    assert(primeDecompositionFor(12) ==List(2, 2, 3), "Actual value is " + primeDecompositionFor(12))
    assert(primeDecompositionFor(35) == List(5, 7), "Actual value is " + primeDecompositionFor(35))
    assert(primeDecompositionFor(720) == List(2, 2, 2, 2, 3, 3, 5), "Actual value is " + primeDecompositionFor(720))
  }

}
