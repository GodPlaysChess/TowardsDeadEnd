package functionalproj.dpssim

/**
 * Created by Gleb on 3/11/2015.
 */
case class Enemy(hp: Double/*, effects: Seq[Effect] = Seq.empty*/) {
  def isDead: Boolean = hp < 0
}