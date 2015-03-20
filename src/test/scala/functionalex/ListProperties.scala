package functionalex

import org.scalacheck.{Gen, Properties}
import org.scalacheck.Prop.forAll

object ListProperties extends Properties("List") {
  property("maxElement") = forAll(Gen.nonEmptyListOf(Gen.choose(1, 100))) { list =>
      val max = list.max
      !list.exists(_ > max)
    }


}
