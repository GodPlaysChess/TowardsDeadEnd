package functionalex.part1

/**
 * Created by GlebP on 05-Mar-2015.
 * Happy birthday, Gleb!
 */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  def state: State[Machine, Unit] = State.unit(this)

  def act(input: Input): State[Machine, Unit] = input match {
    case Coin if this.locked => State(m => ((), Machine(locked = false, m.candies, m.coins + 1)))
    case Turn if !this.locked && this.candies > 0 => State(m => ((), Machine(locked = true, m.candies - 1, m.coins)))
    case _ => State.unit(this)
  }

  // I can either change act, that it keeps coins and candies, but it seems like
  // redundant duplication.
  // lets start with fold!
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
//    inputs.foldLeft()
}
