package functionalex.part3

/**
 * Created by Gleb on 3/30/2015.
 */
sealed trait WC {
  def count: Int = this match {
    case Stub(_) => 1
    case Part(_, w, _) => w + 1
  }
}

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC