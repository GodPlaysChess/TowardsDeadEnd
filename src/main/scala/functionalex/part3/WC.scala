package functionalex.part3

import java.util.Date

sealed trait WC {
  def count: Int = this match {
    case Stub(c) => if (c == "") 0 else 1
    case Part(l, w, r) => Stub(l).count + w + Stub(r).count
  }
}

case class Stub(chars: String) extends WC
case class Part(lStub: String, words: Int, rStub: String) extends WC

case class WebForm(name: String, birthDate: Date, phoneNumber: String)