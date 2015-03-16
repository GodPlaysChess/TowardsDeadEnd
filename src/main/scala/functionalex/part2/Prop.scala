package functionalex.part2

import functionalex.part1.RNG
import functionalex.part1.Stream
import functionalex.part1.Some
import functionalex.part2.Prop.{FailedCase, SuccessCount, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def foraAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (n, rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }
}


case class Prop(run: (TestCases, RNG) => Result) {

  //dont' like a pattern matching here. Looks more like a work for a monad.
  //but how to extract error message without pattern matching?
  def &&(p: Prop) = Prop((tc, rng) => {
    val p1r = run(tc, rng)
    val p2r = p.run(tc, rng)
    p1r match {
      case Passed => p2r match {
        case Passed => Passed
        case Falsified(f2, s2) => Falsified(f2, s2 + tc)
      }
      case Falsified(f1, s1) => p2r match {
        case Passed => Falsified(f1, s1 + tc)
        case Falsified(f2, s2) => Falsified(f1 + f2, s2 + s1)
      }
    }
  }
  )

  def ||(p: Prop) = Prop((tc, rng) => {
    val p1r = run(tc, rng)
    val p2r = p.run(tc, rng)
    p1r match {
      case Passed => Passed
      case Falsified(f1, s1) => p2r match {
        case Passed => Passed
        case Falsified(f2, s2) => Falsified(f1 + f2, s2 + s1)
      }
    }
  })
}

  sealed trait Result {
    def isFalsified: Boolean
  }

  case object Passed extends Result {
    override def isFalsified = false
  }

  case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
    override def isFalsified = true
  }
