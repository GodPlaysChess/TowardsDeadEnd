package projectEuler.utils.rest

import functionalproj.dpssim.{SearingPain, ShadowBolt, Spell, Enemy}

import scalaz._
import Scalaz._

// Fancy funcitonal approach for findSequence
object TestingFoldables {
  val seq = NonEmptyList(1, 2, 4)
  val t1: (Enemy, List[Spell]) = Enemy(100) -> List(ShadowBolt())
  val t2: (Enemy, List[Spell]) = Enemy(100) -> List(SearingPain())
  val list: NonEmptyList[(Enemy, List[Spell])] = NonEmptyList(t1, t2)

  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  implicit val enSpellSemigroup: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    //using implicit sum Monoid
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      if (f1._2.foldMap(_.dmg) > f2._2.foldMap(_.dmg)) f1 else f2
  }

  val hpLens: Lens[Enemy, Double] = Lens.lensu[Enemy, Double](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  val damageMod: (Enemy, Spell) => Enemy = (e, s) =>
    hpLens.mod(_ - s.dmg, e)

  val spells = NonEmptyList(ShadowBolt(), SearingPain())

  // need to "forever combine"
  def combine1[E, A](li: NonEmptyList[(E, List[A])])(f: NonEmptyList[A])(g: (E, A) => E)(p: E => Boolean): NonEmptyList[(E, List[A])] = for {
    esp <- li
    sp <- f
  } yield {
      if (!p(esp._1)) g(esp._1, sp) -> (sp :: esp._2)
      else esp._1 -> esp._2
    }

  /** combines until certain condition is met */
  def combineUntil[E, A](li: NonEmptyList[(E, List[A])])(f: NonEmptyList[A])(mod: (E, A) => E)(p: E => Boolean): NonEmptyList[(E, List[A])] = {
    val c = combine1(li)(f)(mod)(p)
    c.foreach(println)
    if (c.all(e => p(e._1))) c
    else combineUntil(c)(f)(mod)(p)
  }

  def giveMeEnemyAndICalculateYouAllRes(en: Enemy): NonEmptyList[(Enemy, List[Spell])] =
    combineUntil(NonEmptyList(en -> List.empty[Spell]))(spells)(damageMod)(_.isDead)

  def findStrategy2(enemy: Enemy): List[Spell] =
    Foldable1[NonEmptyList].fold1(giveMeEnemyAndICalculateYouAllRes(enemy))._2

  def main(args: Array[String]) {
    println(Foldable1[NonEmptyList].fold1(seq))
    println(Foldable1[NonEmptyList].fold1(list))

    println("Strategy: " + findStrategy2(Enemy(50)).mkString(" -> "))
  }
}

