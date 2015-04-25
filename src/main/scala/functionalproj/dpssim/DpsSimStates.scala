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

  val spells = NonEmptyList(ShadowBolt(), SearingPain())

  def combine1[E, A](li: NonEmptyList[(E, List[A])], variations: NonEmptyList[A])(mod: A => State[E, Boolean]): NonEmptyList[(E, List[A])] = for {
    e0_sp <- li
    sp <- variations
    (e1, p) = mod(sp)(e0_sp._1)
  } yield
    p ? (e1 -> (sp :: e0_sp._2)) | e0_sp

  /** combines until certain condition is met */
  def combineUntil[E, A](li: NonEmptyList[(E, List[A])], f: NonEmptyList[A])(mod: A => State[E, Boolean])(p: E => Boolean): NonEmptyList[(E, List[A])] = {
    val c = combine1(li, f)(mod)
    c.all(e => p(e._1)) ? c | combineUntil(c, f)(mod)(p)
  }

  def allSequences(en: Enemy): NonEmptyList[(Enemy, List[Spell])] =
    combineUntil(NonEmptyList(en -> List.empty[Spell]), spells)(damState)(_.isDead)

  def findStrategy(enemy: Enemy): List[Spell] =
    Foldable1[NonEmptyList].fold1(allSequences(enemy))._2
}
