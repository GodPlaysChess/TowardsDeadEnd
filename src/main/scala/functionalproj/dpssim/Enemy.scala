package functionalproj.dpssim

/**
 * Created by Gleb on 3/11/2015.
 */
case class Enemy(hp: Double, effects: Seq[Effect] = Seq.empty)

//  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = for {
//    _ <- sequence(inputs.map(i => modify((s: Machine) => (i, s) match {
//      case (_, Machine(_, 0, _)) => s
//      case (Coin, Machine(false, _, _)) => s
//      case (Turn, Machine(true, _, _)) => s
//      case (Coin, Machine(true, candy, coin)) =>
//        Machine(locked = false, candy, coin + 1)
//      case (Turn, Machine(false, candy, coin)) =>
//        Machine(locked = true, candy - 1, coin)
//    })))
//    s <- get
//  } yield (s.coins, s.candies)
