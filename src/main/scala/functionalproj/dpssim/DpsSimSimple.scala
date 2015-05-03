package functionalproj.dpssim

object DpsSimSimple {

  def findStrategy(enemy: Enemy): Seq[Spell] =
    findBest(runViaFor(enemy))

  private def findBest(seq: Seq[Seq[Spell]]): Seq[Spell] =
    seq.minBy(_.foldLeft(0d)(_ + _.castTime))

  private def step(enemy: Enemy): Seq[(Enemy, Spell)] =
    Seq(
      applySpell(enemy, ShadowBolt()),
      applySpell(enemy, SearingPain())
    )

  private def applySpell(enemy: Enemy, spell: Spell): (Enemy, Spell) =
    enemy.copy(enemy.hp - spell.dmg) -> spell

  private def runViaFor(enemy: Enemy): Seq[Seq[Spell]] = {
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

}
