package functionalproj.dpssim

import scalaz.Scalaz._
import scalaz._

object DpsSimStatesV2 {
  type Hp = Double

  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  implicit val spellSum: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      (f1._2.foldMap(_.castTime) < f2._2.foldMap(_.castTime)) ? f1 | f2
  }

  val hpLens: Lens[Enemy, Hp] = Lens.lensu[Enemy, Hp](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  def combineS1(spell: Spell): State[(NonEmptyList[(Enemy, List[Spell])]), Boolean] = for {
    _ <- modify[NonEmptyList[(Enemy, List[Spell])]](_.map { case tuple@(e, sp) =>
      !e.isDead ? (hpLens.mod(_ - spell.dmg, e) -> (spell :: sp)) | tuple
    })
    a <- get[NonEmptyList[(Enemy, List[Spell])]]
    allDead = a.all(_._1.isDead)
  } yield allDead

  def combineS(variations: NonEmptyList[Spell]): State[NonEmptyList[(Enemy, List[Spell])], Boolean] =
    foldStates(variations map combineS1)

  def foldStates(in: NonEmptyList[State[NonEmptyList[(Enemy, List[Spell])], Boolean]]): State[NonEmptyList[(Enemy, List[Spell])], Boolean] = {
    in.foldLeft1((s1, s2) => State(e => {
      val seq1 = s1(e)
      val seq2 = s2(e)
      seq1._1.append(seq2._1) -> (seq1._2 && seq2._2)
    }))
  }

  def allSequences(en: Enemy, spells: NonEmptyList[Spell]): NonEmptyList[(Enemy, List[Spell])] = {
    val StateX = StateT.stateMonad[NonEmptyList[(Enemy, List[Spell])]]
    StateX.untilM_(get, combineS(spells)).exec(NonEmptyList(en -> List.empty))
  }

  def findStrategy(enemy: Enemy, spells: NonEmptyList[Spell]): List[Spell] =
    Foldable1[NonEmptyList].fold1(allSequences(enemy, spells))._2

}
