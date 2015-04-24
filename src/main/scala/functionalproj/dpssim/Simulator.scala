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

  implicit object Conjunction extends Monoid[Boolean] {
    override def zero: Boolean = true

    override def append(f1: Boolean, f2: => Boolean): Boolean = f1 && f2
  }

  val minDoubleMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 min f2
  }

  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  def findStrategy(enemy: Enemy): Seq[Spell] =
    findBest(runViaFor(enemy))

  // | best for now means less time | Monad or foldable!
  def findBest(seq: Seq[Seq[Spell]]): Seq[Spell] =
    seq.minBy(_.foldLeft(0d)(_ + _.castTime))

  // using foldMap and min Monoid:
  //  def _findBest(seq: Seq[Seq[Spell]]): Seq[Spell] = {
  //    Foldable[Seq].minimumBy(seq)(inner => Foldable[Seq].foldMap(inner)(_.castTime)).getOrElse(Seq.empty)
  //  }

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
    hpLens.mod(_ - spell.dmg, enemy) -> spell

  //set: (A, B) => A,
  //get: A => B
  val hpLens: Lens[Enemy, Double] = Lens.lensu[Enemy, Double](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

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

  ////////////////////////// Fancy functional way /////////////////////////////////////////////  TODO try to replace tuples with functions (A, B) <~> A => B
  // S => (A, S)  ==  Enemy => (Seq[Spell], Enemy)
  type Live = Boolean
  type StateE = State[(Enemy, List[Spell]), Live]
  // or [(Enemy, (Boolean, List[Spell])] or (Enemy -> List[Spell], Boolean -> List[Spell])
  type StateEnemy[A] = State[Enemy, A]

  // apply spell results in a list of StateE s.
  // >>= works the way, that spell is applied only if Enemy.alive
  // traverse over List[StateE] => State[List[(Enemy, List[Spell])]
  // Then reduce State[List[A]] => State[A], by applying |(A, A) => A| OR |A => B, Monoid[B]|

  //** Definetly Monad should be involved to define a flow. For example, enemy Monad.
  // Like Writer monad, where Enemy is a value, and Seq of applied spells is Log
  //
  def spellBind(es: (Enemy, Spell))(f: Enemy => (Enemy, List[Spell])): (Enemy, List[Spell]) = {
    val (e, l) = f(es._1)
    e -> (es._2 :: l)
  }

  val stepF: Enemy => List[(Enemy, Spell)] = e =>
    List(e -> ShadowBolt(), e -> SearingPain())

  // Actually I need something like  *combine*: (List[E, List[A]])(E => List[E, A]): List[E, List[A]]
  //

  def combine[E, A](li: List[(E, List[A])])(f: E => List[(E, A)]): List[(E, List[A])] = ???

  //    li.flatMap{ case (e, l) => f(e)}
  // List[E, A] |+| List[A] => List[E, List[A]]

  val spells = List(ShadowBolt(), SearingPain())

  def combine1[E, A](li: List[(E, List[A])])(f: List[A])(p: E => Boolean): List[(E, List[A])] = for {
    sp <- f
    esp <- li
  } yield {
      if (p(esp._1)) esp._1 -> (sp :: esp._2)
      else esp._1 -> esp._2
    }

  def giveMeEnemyAndICalculateYouAllRes(en: Enemy): List[(Enemy, List[Spell])] =
    combine1(List(en -> List.empty[Spell]))(spells)(_.isDead)

  def findStrategy2(enemy: Enemy): List[Spell] = {
//    val x: List[(Enemy, List[Spell])] = giveMeEnemyAndICalculateYouAllRes(enemy)
//    val xx: ListOps[(Enemy, List[Spell])] = Scalaz.TOFo(x)
//    val t = enSpellSemigroup.foldl1(x)
//    val y: (Enemy, List[Spell]) = Foldable1[List](ListFoldable).suml1(enSpellSemigroup)
//    val y: (Enemy, List[Spell]) = Foldable1[List].fold1(x)(enSpellSemigroup)
//    y._2
    ???
  }
  // Semigroup -> OptionMonoid
  // if semgigroup defined through the monoid, probably we may combine them. // modularity, heh?      //ToDo look at OneAndFoldable
  implicit val enSpellSemigroup: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    //using implicit sum Monoid
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      if (f1._2.foldMap(_.dmg) > f2._2.foldMap(_.dmg)) f1 else f2
  }







  /*
  SequenceComputationMonad[Comput[Enemy]]

  import Data.Monoid

    type Food = String
    type Price = Sum Int

      addDrink :: Food -> (Food,Price)
    addDrink "beans" = ("milk", Sum 25)
    addDrink "jerky" = ("whiskey", Sum 99)
    addDrink _ = ("beer", Sum 30)
    */

  def fancyFindStrategy(enemy: Enemy): List[Spell] = ???

  def step(stateE: StateE): List[StateE] = ???

  def simulateSeq(input: List[Spell]): State[(Enemy, List[Spell]), Live] =
    for {
      es <- get[(Enemy, List[Spell])]
      e = es._1; s = es._2
      enemy <- put(hpLens.mod(_ - input.foldMap(_.dmg), e) -> (s ++ input))
    } yield !e.isDead

  def simulateSeq1(input: List[Spell]): State[Enemy, List[Spell]] =
    for {
      e <- get[Enemy]
      en <- put(hpLens.mod(_ - input.foldMap(_.dmg), e))
    } yield input

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
