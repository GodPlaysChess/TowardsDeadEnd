package functionalproj.dpssim

import scalaz._
import Scalaz._

// using states
object DpsSimStates {
  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  implicit val spellSum: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      (f1._2.foldMap(_.castTime) < f2._2.foldMap(_.castTime)) ? f1 | f2
  }

  val hpLens: Lens[Enemy, Double] = Lens.lensu[Enemy, Double](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  def damState(spell: Spell): State[Enemy, Unit] =
    State[Enemy, Unit](e => hpLens.mod(_ - spell.dmg, e) -> ())

  val spells = NonEmptyList(ShadowBolt(), SearingPain())

  def combine1[E, A](li: NonEmptyList[(E, List[A])], variations: NonEmptyList[A])(mod: A => State[E, Unit])(p: E => Boolean): NonEmptyList[(E, List[A])] = for {
    esp <- li
    sp <- variations
  } yield
    !p(esp._1) ? (mod(sp).exec(esp._1) -> (sp :: esp._2)) | esp

  /** combines until certain condition is met */
  def combineUntil[E, A](li: NonEmptyList[(E, List[A])], f: NonEmptyList[A])(mod: A => State[E, Unit])(p: E => Boolean): NonEmptyList[(E, List[A])] = {
    val c = combine1(li, f)(mod)(p)
    c.all(e => p(e._1)) ? c | combineUntil(c, f)(mod)(p)
  }

  def allSequences(en: Enemy): NonEmptyList[(Enemy, List[Spell])] =
    combineUntil(NonEmptyList(en -> List.empty[Spell]), spells)(damState)(_.isDead)

  def findStrategy(enemy: Enemy): List[Spell] =
    Foldable1[NonEmptyList].fold1(allSequences(enemy))._2
}
