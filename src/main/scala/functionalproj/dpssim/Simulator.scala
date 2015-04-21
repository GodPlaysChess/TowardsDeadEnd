package functionalproj.dpssim

import scalaz.Scalaz._
import scalaz.{Foldable, Monoid, State, _}


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

  implicit val minDoubleMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 min f2
  }

  /* Standard functional way */

  def findStrategy(enemy: Enemy): Seq[Spell] =
    findBest(runViaFor(enemy))

  // | best for now means less time | Monad or foldable!
  def findBest(seq: Seq[Seq[Spell]]): Seq[Spell] =
    seq.minBy(_.foldLeft(0d)(_ + _.castTime))

  // using foldMap and min Monoid:
  def _findBest(seq: Seq[Seq[Spell]]): Seq[Spell] = {
    Foldable[Seq].minimumBy(seq)(inner => Foldable[Seq].foldMap(inner)(_.castTime)).getOrElse(Seq.empty)
  }

  // should be state, but let's make it work first in functional but in non-monadic way
  def step(enemy: Enemy): Seq[(Enemy, Spell)] =
    Seq(
      applySpell(enemy, ShadowBolt()),
      applySpell(enemy, SearingPain())
    )

  // easy way
  def applySpell(enemy: Enemy, spell: Spell): (Enemy, Spell) =
    enemy.copy(enemy.hp - spell.dmg) -> spell

  // using lens
  def _applySpellLens(enemy: Enemy, spell: Spell): (Enemy, Spell) =
    castSpell(enemy, spell) -> spell

  /** returns sequence of sequences of spells which kills the enemy */
  // LENS here
  def run(enemy: Enemy): Seq[Seq[Spell]] = {
    def go(state: Seq[(Enemy, Seq[Spell])]): Seq[(Enemy, Seq[Spell])] = {
      if (state.forall(_._1.hp <= 0)) state
      else go(state.flatMap { case id@(en, seq) =>
        if (en.hp > 0) {
          step(en).map(ensp => ensp._1 -> (ensp._2 +: seq))
        } else Seq(id)
      })
    }
    go(Seq(enemy -> Seq.empty)).map(_._2)
  }

  def runViaFor(enemy: Enemy): Seq[Seq[Spell]] = {
    def go(state: Seq[(Enemy, Seq[Spell])]): Seq[(Enemy, Seq[Spell])] = {
      if (state.forall(_._1.hp <= 0)) state
      else go(for {
        (enemy, seq) <- state
        (en, spell) <- step(enemy)
      } yield {
          if (enemy.hp > 0) en -> (spell +: seq)
          else enemy -> seq
        }
      )
    }
    go(Seq(enemy -> Seq.empty)).map(_._2)
  }

  ////////////////////////// Fancy functional way /////////////////////////////////////////////

  def fancyFindStrategy(enemy: Enemy): List[Spell] = ???

  def simulate(input: List[Spell]): State[Enemy, Boolean] = {
    val sts: List[State[Enemy, Boolean]] = input.map(applySpell)
    val b: StateEnemy[List[Boolean]] = sts.sequence[StateEnemy, Boolean]
    b.map(_.suml)
  }

  // neeed to apply spell using State.modify
  def applySpell(sp: Spell): State[Enemy, Boolean] = for {
    s <- State.get[Enemy]
    alive = if (s.hp - sp.dmg > 0) true else false
    enemy <- State.put(Enemy(s.hp - sp.dmg, s.effects ++ sp.effects)) // here we're chaining the state
  } yield alive // this value we're returning.


  //set: (A, B) => A,
  //get: A => B
  val hpLens: Lens[Enemy, Double] = Lens.lensu[Enemy, Double] (
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  // the same as apply spell, but using Lens
  def castSpell(en: Enemy, sp: Spell): Enemy = {
    hpLens.mod(_ - sp.dmg, en)
  }

  def _applySpell(sp: Spell, en: Enemy): (Enemy, Boolean) = {
    val enemy = castSpell(en, sp)
    enemy -> (enemy.hp <= 0)
  }





  /**
   * Idea:
   * 1) Generate all sequences   ->  Gen must help
   * 2) Simulate behavior with this sequence.   ->    Simulate yields boolean.
   * 3) Yield the sequence if enemy dies   -> can I somehow use computed state?
   * 4) Collect all sequences
   * 5) Find the best sequence
   */




















  def appendSpellForEveryLivingEnemy(enseq: (Enemy, Seq[Spell])): Seq[(Enemy, Seq[Spell])] = {
    if (enseq._1.hp > 0) {
      step(enseq._1).map(ensp => ensp._1 -> (ensp._2 +: enseq._2))
    } else Seq(enseq)
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
