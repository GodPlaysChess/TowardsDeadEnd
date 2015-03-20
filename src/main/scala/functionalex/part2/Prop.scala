package functionalex.part2

import functionalex.part1.{RNG, SimpleRNG, Some, Stream}
import functionalex.part2.Prop.{FailedCase, MaxSize, SuccessCount, TestCases}

object Prop {
  type FailedCase = String
  type SuccessCount = Int
  type TestCases = Int
  type MaxSize = Int

  def randomStream[A](g: Gen[A])(rng: RNG): Stream[A] =
    Stream.unfold(rng)(rng => Some(g.sample.run(rng)))

  def buildMsg[A](s: A, e: Exception): String =
    s"test case: $s\n" +
      s"generated an exception: ${e.getMessage}\n" +
      s"stack trace:\n ${e.getStackTrace.mkString("\n")}"

  def forAll[A](g: SGen[A])(f: A => Boolean): Prop =
    forAll(g.forSize)(f)

  def forAll[A](g: Int => Gen[A])(f: A => Boolean): Prop = Prop {
    (max, n, rng) =>
      val casesPerSize = (n + (max - 1)) / max
      val props: Stream[Prop] =
        Stream.from(0).takeS((n min max) + 1).map(i => forAll[A](g(i))(f))
      val prop: Prop =
        props.toList.map(p => Prop { (max, _, rng) =>
          p.run(max, casesPerSize, rng)
        }).reduce(_ && _)
      prop.run(max, n, rng)
  }

  def forAll[A](as: Gen[A])(f: A => Boolean): Prop = Prop {
    (s, n, rng) => randomStream(as)(rng).zipWith(Stream.from(0))((_, _)).take(n).map {
      case (a, i) => try {
        if (f(a)) Passed else Falsified(a.toString, i)
      } catch {
        case e: Exception => Falsified(buildMsg(a, e), i)
      }
    }.find(_.isFalsified).getOrElse(Passed)
  }

  /**
   * for now I'll make separate forall for exhaustive genreation.
   * Of course, later, it should do exhaustive check implicitly for small
   * domains
   *
   *
   * */
  def forAllExhaustive[A](as: EGen[A])(f: A => Boolean): Result = {
    val failed = as.allValues.filter(f)
    if (failed.isEmpty) Proved
    else Falsified(failed.toList.toString(), as.allValues.size - failed.size)
  }



  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
      case Proved =>
        println(s"+ OK, proved property")
    }

  def check(p: => Boolean): Prop = Prop { (_, _, _) =>
    if (p) Passed else Falsified(" () ", 0)
  }

}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  //dont' like a pattern matching here. Looks more like a work for a monad.
  //but how to extract error message without pattern matching?
  def &&(p: Prop) = Prop((s, tc, rng) => {
    val p1r = run(s, tc, rng)
    val p2r = p.run(s, tc, rng)
    p1r match {
      case Passed | Proved => p2r match {
        case Passed | Proved => Passed
        case Falsified(f2, s2) => Falsified(f2, s2 + tc)
      }
      case Falsified(f1, s1) => p2r match {
        case Passed | Proved => Falsified(f1, s1 + tc)
        case Falsified(f2, s2) => Falsified(f1 + f2, s2 + s1)
      }
    }
  }
  )

  def ||(p: Prop) = Prop((s, tc, rng) => {
    val p1r = run(s, tc, rng)
    val p2r = p.run(s, tc, rng)
    p1r match {
      case Passed => Passed
      case Proved => Proved
      case Falsified(f1, s1) => p2r match {
        case Passed => Passed
        case Proved => Proved
        case Falsified(f2, s2) => Falsified(f1 + f2, s2 + s1)
      }
    }
  })
}

sealed trait Result {
  def isFalsified: Boolean
}

case object Proved extends Result {
  override def isFalsified = false
}

case object Passed extends Result {
  override def isFalsified = false
}

case class Falsified(failure: FailedCase, successes: SuccessCount) extends Result {
  override def isFalsified = true
}
