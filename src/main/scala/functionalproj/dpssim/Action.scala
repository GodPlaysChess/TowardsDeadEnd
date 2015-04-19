package functionalproj.dpssim

sealed trait Action

sealed trait Hit extends Action

sealed trait Spell extends Action {
  type Hp = Double
  type Sec = Double
  type Mp = Double

  val manaCost: Mp
  val castTime: Sec = 0.5
  val dmg: Hp
  val effects: Seq[Effect] = Seq.empty

  val dps: Hp = dmg / castTime

}

case class Corrupt() extends Spell {
  override val manaCost: Mp = 55
  override val castTime: Sec = 2
  override val dmg: Hp = 0
  override val effects: Seq[Effect] = Seq(Corruption())
}

case class ShadowBolt() extends Spell {
  override val manaCost: Mp = 110
  override val castTime: Sec = 3
  override val dmg: Hp = 92
}

case class SearingPain() extends Spell{
  override val manaCost: Mp = 45
  override val dmg: Hp = 38
  override val castTime: Sec = 1.5
}

