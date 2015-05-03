package functionalproj.dpssim

import scalaz.Scalaz._
import scalaz._

object DpsSimWriter {

  type Hp = Double

  implicit val sumMonoid = new Monoid[Double] {
    override def zero: Double = 0d

    override def append(f1: Double, f2: => Double): Double = f1 + f2
  }

  implicit val spellSum: Semigroup[(Enemy, List[Spell])] = new Semigroup[(Enemy, List[Spell])] {
    override def append(f1: (Enemy, List[Spell]), f2: => (Enemy, List[Spell])): (Enemy, List[Spell]) =
      (f1._2.foldMap(_.castTime) < f2._2.foldMap(_.castTime)) ? f1 | f2
  }

  val hpLens = Lens.lensu[Enemy, Hp](
    (e, newhp) => e.copy(hp = newhp),
    _.hp
  )

  def gcd(a: Int, b: Int): Writer[List[String], Int] =
    if (b == 0) for {
      _ <- List("Finished with " + a.shows).tell
    } yield a
    else
      List(a.shows + " mod " + b.shows + " = " + (a % b).shows).tell >>= { _ =>
        gcd(b, a % b)
      }

  def applySpell(e: Enemy, s: Spell): Writer[List[Spell], Enemy] =
    e.set(List(s))

  def logSpells(spells: NonEmptyList[Spell], e0: Enemy): Writer[List[Spell], Enemy] =
    spells.foldLeft(e0.set(List.empty[Spell]))((e1, s1) => for {
      e2 <- e1
      e3 <- applySpell(e2, s1)
    } yield e3
    )



  def main(args: Array[String]) {
    val e0 = Enemy(100)
    val log = logSpells(NonEmptyList(ShadowBolt(), SearingPain()), e0)
    print(log)
  }

  /*def simulateSpell(spell: Spell): State[(NonEmptyList[(Enemy, List[Spell])]), Boolean] = for {
    _ <- modify[NonEmptyList[(Enemy, List[Spell])]](_.map { case tuple@(e, sp) =>
      !e.isDead ? (hpLens.mod(_ - spell.dmg, e) -> (spell :: sp)) | tuple
    })
    a <- get[NonEmptyList[(Enemy, List[Spell])]]
    allDead = a.all(_._1.isDead)
  } yield allDead

  def step(variations: NonEmptyList[Spell]): State[NonEmptyList[(Enemy, List[Spell])], Boolean] =
    (variations map simulateSpell) |> combine

  def combine(in: NonEmptyList[State[NonEmptyList[(Enemy, List[Spell])], Boolean]]): State[NonEmptyList[(Enemy, List[Spell])], Boolean] = {
    in.foldLeft1((s1, s2) => State(e => {
      val seq1 = s1(e)
      val seq2 = s2(e)
      seq1._1.append(seq2._1) -> (seq1._2 && seq2._2)
    }))
  }

  def allSequences(en: Enemy, spells: NonEmptyList[Spell]): NonEmptyList[(Enemy, List[Spell])] = {
    val StateX = StateT.stateMonad[NonEmptyList[(Enemy, List[Spell])]]
    StateX.untilM_(get, step(spells)).exec(NonEmptyList(en -> List.empty))
  }
*/
//  def findStrategy(enemy: Enemy, spells: NonEmptyList[Spell]): List[Spell] =
//    logSpells(spells, enemy).run(spells.toList, enemy)

}
