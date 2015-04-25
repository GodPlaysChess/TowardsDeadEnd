package functionalproj.dpssim

import scalaz._
import Scalaz._

// Using lenses, monoids and Nels
object FunctionalDpsSim {
  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  implicit val enSpellSemigroup: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    //using implicit sum Monoid
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      if (f1._2.foldMap(_.castTime) < f2._2.foldMap(_.castTime)) f1 else f2
  }

  val hpLens = Lens.lensu[Enemy, Double](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  val damageMod: (Enemy, Spell) => Enemy = (e, s) =>
    hpLens.mod(_ - s.dmg, e)

  val spells = NonEmptyList(ShadowBolt(), SearingPain())
  
  private def damState(spell: Spell): State[Enemy, Unit] =
    State[Enemy, Unit](e => hpLens.mod(_ - spell.dmg, e) -> ())

  private def combine1[E, A](li: NonEmptyList[(E, List[A])])(f: NonEmptyList[A])(g: (E, A) => E)(p: E => Boolean): NonEmptyList[(E, List[A])] = for {
    esp <- li
    sp <- f
  } yield {
      if (!p(esp._1)) g(esp._1, sp) -> (sp :: esp._2)
      else esp
    }

  /** combines until certain condition is met */
  private def combineUntil[E, A](li: NonEmptyList[(E, List[A])])(f: NonEmptyList[A])(mod: (E, A) => E)(p: E => Boolean): NonEmptyList[(E, List[A])] = {
    val c = combine1(li)(f)(mod)(p)
    if (c.all(e => p(e._1))) c
    else combineUntil(c)(f)(mod)(p)
  }

  private def allSequences(en: Enemy): NonEmptyList[(Enemy, List[Spell])] =
    combineUntil(NonEmptyList(en -> List.empty[Spell]))(spells)(damageMod)(_.isDead)

  def findStrategy(enemy: Enemy): List[Spell] =
    Foldable1[NonEmptyList].fold1(allSequences(enemy))._2

}

