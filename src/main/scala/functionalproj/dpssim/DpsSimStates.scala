package functionalproj.dpssim

import scalaz._
import Scalaz._

// using states
object DpsSimStates {
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

  // actually IndexedState (more general one), can use a Functor for a S.( S1 => F[S2, A] )
  def damState(spell: Spell): State[Enemy, Boolean] =
    State[Enemy, Boolean](e => hpLens.mod(_ - spell.dmg, e) -> !e.isDead)

  type AllDead = Boolean
  type StateNel[E, A] = State[(NonEmptyList[(E, List[A])]), AllDead]

  def damState1(spell: Spell): State[Enemy, (Spell, AllDead)] =
    State[Enemy, (Spell, Boolean)](e => hpLens.mod(_ - spell.dmg, e) -> (spell -> !e.isDead))

  val spells = NonEmptyList(ShadowBolt() , SearingPain())

  // give me spell, and I apply it to your every enemy if he's still alive (and append it to list of applied spells)
  def combineS1(spell: Spell): State[(NonEmptyList[(Enemy, List[Spell])]), Boolean] = for {
    _ <- modify[NonEmptyList[(Enemy, List[Spell])]](_.map { case tuple@(e, sp) =>
      !e.isDead ? (hpLens.mod(_ - spell.dmg, e) -> (spell :: sp)) | tuple
    })
    a <- get[NonEmptyList[(Enemy, List[Spell])]]
    allDead = a.all(_._1.isDead)
  } yield allDead

  def combineS(variations: NonEmptyList[Spell]): State[NonEmptyList[(Enemy, List[Spell])], AllDead] = {
    val x: NonEmptyList[State[NonEmptyList[(Enemy, List[Spell])], Boolean]] = variations map combineS1 // if down below solution is not correct - then work on this one. (fold and sum with concatenate)\
    val x1: State[NonEmptyList[(Enemy, List[Spell])], AllDead] = foldStates(x)                     // yes, traverse does exactly this. It applies the states sequentially. It actually folds states, by appending the list of spells. But i need branching there
    val y: State[NonEmptyList[(Enemy, List[Spell])], NonEmptyList[Boolean]] = variations.traverseS(combineS1) // consider this is correct. should be V map combineS1 . sequence
    y.map(_.suml1(booleanInstance.conjunction))
    x1
  }

  def foldStates(in: NonEmptyList[State[NonEmptyList[(Enemy, List[Spell])], Boolean]]): State[NonEmptyList[(Enemy, List[Spell])], Boolean] = {
    in.foldLeft1((s1, s2) => State(e => s2(s1(e)._1)))
  }

  def map2[S, A](sa: State[S, A], sb: State[S, A])(g: (S, S) => S)(f: (A, A) => A): State[S, A] =
    sa.flatMap(a => sb.map(f(a, _)))

  def allSequences1(en: Enemy): NonEmptyList[(Enemy, List[Spell])] = {
    val StateX = StateT.stateMonad[NonEmptyList[(Enemy, List[Spell])]]
    val x = StateX.untilM_(gets(_.all(_._1.isDead)), combineS(spells)).exec(NonEmptyList(en -> List()))
//    val x = combineS(spells).run(NonEmptyList(en -> List()))
    println(x)
    x
  }

  def combine1[E, A](li: NonEmptyList[(E, List[A])], variations: NonEmptyList[A])(mod: A => State[E, Boolean]): NonEmptyList[(E, List[A])] = for {
    e0_sp <- li
    sp <- variations
    (e1, p) = mod(sp)(e0_sp._1)
  } yield
    p ? (e1 -> (sp :: e0_sp._2)) | e0_sp

  /** combines until certain condition is met */
  //TODO p is the same as in mod. Can unify it.
  def combineUntil[E, A](start: NonEmptyList[(E, List[A])], variations: NonEmptyList[A])(mod: A => State[E, Boolean])(p: E => Boolean): NonEmptyList[(E, List[A])] = {
    val c = combine1(start, variations)(mod)
    c.all(e => p(e._1)) ? c | combineUntil(c, variations)(mod)(p)
  }

  def allSequences(en: Enemy): NonEmptyList[(Enemy, List[Spell])] =
    combineUntil(NonEmptyList(en -> List.empty[Spell]), spells)(damState)(_.isDead)

  def findStrategy(enemy: Enemy): List[Spell] =
    Foldable1[NonEmptyList].fold1(allSequences1(enemy))._2
}
