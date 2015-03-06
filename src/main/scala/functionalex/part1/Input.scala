package functionalex.part1

/**
 * Created by GlebP on 05-Mar-2015.
 * Happy birthday, Gleb!
 */
sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int) {
  def go(input: Input): Machine = input match {
    case Coin if locked => Machine(locked = false, candies, coins + 1)
    case Turn if !locked && this.candies > 0 => Machine(locked = true, candies - 1, coins)
    case _ => this
  }

  def unit: State[Machine, Unit] = State.unit(Machine)

  def act(input: Input): State[Machine, Unit] = input match {
    case Coin => State(m => (if (m.locked) Machine(locked = false, m.candies, m.coins + 1), Machine(locked = false, m.candies, m.coins + 1)))
    case Turn => State(m => (if (!m.locked && m.candies > 0) Machine(locked = true, m.candies - 1, m.coins), Machine(locked = true, m.candies - 1, m.coins)))
    case _ => unit
  }

  /**
   * my solution. Actually I like that better, cause split the logic of act on a certain input
   * and simulation of several inputs. But call a.f(a) is surely redundant.
   */
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {
    val a = inputs.foldLeft(unit)((m, i) => m.flatMap(_ => act(i)))
    for {
      _ <- a
      b <- State.get
    } yield (b.candies, b.coins)
  }

  // this solution does not use states and easier to understand ;)
  def simulate(inputs: List[Input]): (Int, Int) = {
    val m = inputs.foldLeft(this)(_ go _)
    (m.coins, m.candies)
  }
}

// this solution is stolen from
// https://github.com/fpinscala/fpinscala/blob/master/answers/src/main/scala/fpinscala/state/State.scala
import State._

object Candy {
  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
      case (_, Machine(_, 0, _)) => s
      case (Coin, Machine(false, _, _)) => s
      case (Turn, Machine(true, _, _)) => s
      case (Coin, Machine(true, candy, coin)) =>
        Machine(locked = false, candy, coin + 1)
      case (Turn, Machine(false, candy, coin)) =>
        Machine(locked = true, candy - 1, coin)
    })))
    s <- get
  } yield (s.coins, s.candies)
}
