package functionalproj.dpssim

import scala.language.reflectiveCalls
import scalaz.Scalaz._
import scalaz.{Monoid, State, _}


/**
 * Not clear:
 * 1) should I pass along the state "Alive check"?
 * 2) should I pass along the state "Timepool" ?
 * 3) Shoould I pass with the enemy "time spent" ? think
 * no, cause it can be calculated by folding the sequence of spells by casttime
 */

object Simulator {

  val hpLens: Lens[Enemy, Double] = Lens.lensu[Enemy, Double](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  implicit object Conjunction extends Monoid[Boolean] {
    override def zero: Boolean = true

    override def append(f1: Boolean, f2: => Boolean): Boolean = f1 && f2
  }

  ////////////////////////// Fancy functional way /////////////////////////////////////////////  TODO try to replace tuples with functions (A, B) <~> A => B
  type Live = Boolean
  type StateE = State[(Enemy, List[Spell]), Live]
  // or [(Enemy, (Boolean, List[Spell])] or (Enemy -> List[Spell], Boolean -> List[Spell])
  type StateEnemy[A] = State[Enemy, A]

  // apply spell results in a list of StateE s.
  // >>= works the way, that spell is applied only if Enemy.alive
  // traverse over List[StateE] => State[List[(Enemy, List[Spell])]
  // Then reduce State[List[A]] => State[A], by applying |(A, A) => A| OR |A => B, Monoid[B]|

  // if semgigroup defined through the monoid, probably we may combine them. // modularity, heh?      //ToDo look at OneAndFoldable

  // state: S.run(enemy) yields
  def simulate(input: List[Spell]): State[Enemy, Boolean] = {
    val sts: List[State[Enemy, Boolean]] = input.map(applySpell) // applies the sequence of spells to enemy, tracks down the after each application, whether enemy is alive
    val b: StateEnemy[List[Boolean]] = sts.sequence[StateEnemy, Boolean] // List[State[Enemy, Boolean] -> State[Enemy, List[Boolean]]
    b.map(_.suml) // reduces the list with conjunction monoid
  }

  // neeed to apply spell using State.modify
  def applySpell(sp: Spell): State[Enemy, Boolean] = for {
    s <- State.get[Enemy]
    alive = if (s.hp - sp.dmg > 0) true else false
    enemy <- State.put(Enemy(s.hp - sp.dmg /*, s.effects ++ sp.effects*/)) // here we're chaining the state
  } yield alive // this value we're returning.


  def castSpell(en: Enemy, sp: Spell): Enemy =
    hpLens.mod(_ - sp.dmg, en)

  def _applySpell(sp: Spell, en: Enemy): (Enemy, Boolean) = {
    val enemy = castSpell(en, sp)
    enemy -> (enemy.hp <= 0)
  }


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

  //  def tick: State[Enemy, Unit] = for {
  //    e <- get[Enemy]
  //    Enemy(hp, effects) = e
  //    n <- put(Enemy(hp - sp.dmg, effects ++ sp.effects))
  //  } yield put(Enemy(hp - e.dmg, effects ++ e.effects))


}
