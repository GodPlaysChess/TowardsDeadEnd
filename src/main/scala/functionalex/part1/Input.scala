package functionalex.part1

/**
 * Created by GlebP on 05-Mar-2015.
 * Happy birthday, Gleb!
 */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {

  // State(Machine, Input) => Machine
  def state: State[Machine, Unit] = State.unit(this)

  def act(input: Input): State[Machine, Unit] = input match {
    case Coin if this.locked => State(m => ((), Machine(locked = false, m.candies, m.coins + 1)))
    case Turn if !this.locked && this.candies > 0 => State(m => ((), Machine(locked = true, m.candies - 1, m.coins)))
    case _ => State.unit(this)
  }

  def ac: Input => State[Machine, Unit] = {
    case Coin if this.locked => State(m => ((), Machine(locked = false, m.candies, m.coins + 1)))
    case Turn if !this.locked && this.candies > 0 => State(m => ((), Machine(locked = true, m.candies - 1, m.coins)))
    case _ => State.unit(this)
  }

  def go(input: Input): Machine = input match {
    case Coin if this.locked => Machine(locked = false, this.candies, this.coins + 1)
    case Turn if !this.locked && this.candies > 0 => Machine(locked = true, this.candies - 1, this.coins)
    case _ => this
  }


  // apply sequence of Inputs to the machine => and get machine in the end
  // I can either change act, that it keeps coins and candies, but it seems like
  // redundant duplication.
  // lets start with fold!
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] =
    State(s => (inputs.foldLeft(this)(_.go(_)), (0,0)))
}
