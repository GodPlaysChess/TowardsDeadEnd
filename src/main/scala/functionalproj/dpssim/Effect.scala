package functionalproj.dpssim

/**
 * Created by Gleb on 3/11/2015.
 */
sealed trait Effect {
  type Sec = Double
  type Hp = Double

  val duration: Sec
  val dmg: Hp
  val timeleft: Sec = ??? // do you really need it here with a tick

  val dps = dmg / duration

  def tick = ??? // think where to put it         definetly not here

}

case class Corruption() extends Effect {
  override val duration: Sec = 15
  override val dmg: Hp = 90
}
