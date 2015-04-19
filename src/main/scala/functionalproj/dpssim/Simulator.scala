package functionalproj.dpssim

import scalaz._
import Scalaz._


/**
 * Not clear:
 * 1) should I pass along the state "Alive check"?
 * 2) should I pass along the state "Timepool" ?
 * 3) Shoould I pass with the enemy "time spent" ? think
 * no, cause it can be calculated by folding the sequence of spells by casttime
 */

object Simulator {
  type StateEnemy[A] = State[Enemy, A]

  implicit object Conjunction extends Monoid[Boolean] {
    override def zero: Boolean = true

    override def append(f1: Boolean, f2: => Boolean): Boolean = f1 && f2
  }

  implicit object SpellFoldMonoid extends Monoid[Seq[Spell]] {
    override def zero: Seq[Spell] = ???

    override def append(f1: Seq[Spell], f2: => Seq[Spell]): Seq[Spell] = ???
  }

  def findStrategy(enemy: Enemy): Seq[Spell] =
    findBest(run(enemy))

  // | best for now means less time | Monad or foldable!
  def findBest(seq: Seq[Seq[Spell]]): Seq[Spell] =
    seq.minBy(s => s.foldLeft(0d)(_ + _.castTime))

  // should be state, but let's make it work first in functional but in non-monadic way
  def step(enemy: Enemy): Seq[(Enemy, Spell)] =
    Seq(
      applySpell(enemy, ShadowBolt()),
      applySpell(enemy, SearingPain())
    )

  // lens or state!
  def applySpell(enemy: Enemy, spell: Spell): (Enemy, Spell) =
    enemy.copy(enemy.hp - spell.dmg) -> spell

  /** returns sequence of sequences of spells which kills the enemy */
  // LENS here
  def run(enemy: Enemy): Seq[Seq[Spell]] = {
    def go(state: Seq[(Enemy, Seq[Spell])]): Seq[(Enemy, Seq[Spell])] = {
      if (state.forall(_._1.hp <= 0)) state
//      else go(state.flatMap { case (en, seq) if en.hp > 0 =>
//        step(en).map(ensp => ensp._1 -> (ensp._2 +: seq)) }
//      )
      else go(state.flatMap(enseq => {
        if (enseq._1.hp > 0) {
          step(enseq._1).map(ensp => ensp._1 -> (ensp._2 +: enseq._2))
        } else Seq(enseq)
      }))
    }
    go(Seq(enemy -> Seq.empty)).map(_._2)
  }


  ///////////////////////////////////////////////////////////////////////

  def appendSpellForEveryLivingEnemy(enseq: (Enemy, Seq[Spell])): Seq[(Enemy, Seq[Spell])] = {
    if (enseq._1.hp > 0) {
      step(enseq._1).map(ensp => ensp._1 -> (ensp._2 +: enseq._2))
    } else Seq(enseq)
  }

  def simulate(input: List[Spell]): State[Enemy, Boolean] = {
    val sts: List[State[Enemy, Boolean]] = input.map(applySpell)
    val b: StateEnemy[List[Boolean]] = sts.sequence[StateEnemy, Boolean]
    b.map(_.suml)
  }

  // neeed to apply spell using State.modify
  def applySpell(sp: Spell): State[Enemy, Boolean] = for {
    s <- get[Enemy]
    alive = if (s.hp - sp.dmg > 0) true else false
    enemy <- put(Enemy(s.hp - sp.dmg, s.effects ++ sp.effects)) // here we're chaining the state
  } yield alive // this value we're returning.

  ////////////////////////////////////////////////////////////////////////


  //  def tick: StateEnemy = for {
  //    m <- init
  //    t <- modify[Enemy](e => {
  //      val dmg = e.effects.map(_.dmg).sum
  //      val effects = e.effects
  //      e.copy(e.hp - dmg, effects.map(eff => Effect(eff.timeleft - 1)))
  //    }
  //    )
  //  }
  /*

  Effect => (damage, Effect) is a state itself
  *
  *
  *
  *
  * */
  // seq[obj] => seq [obj] with changed 1 values

  //  def tick: State[Enemy, Unit] = for {
  //    e <- get[Enemy]
  //    Enemy(hp, effects) = e
  //    n <- put(Enemy(hp - sp.dmg, effects ++ sp.effects))
  //  } yield put(Enemy(hp - e.dmg, effects ++ e.effects))


}
