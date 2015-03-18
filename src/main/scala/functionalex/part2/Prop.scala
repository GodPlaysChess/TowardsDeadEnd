package functionalex.part2

import functionalex.part1.{SimpleRNG, RNG, Stream, Some}
import functionalex.part2.Prop.{MaxSize, FailedCase, SuccessCount, TestCases}

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

  def run(p: Prop,
          maxSize: Int = 100,
          testCases: Int = 100,
          rng: RNG = SimpleRNG(System.currentTimeMillis)): Unit =
    p.run(maxSize, testCases, rng) match {
      case Falsified(msg, n) =>
        println(s"! Falsified after $n passed tests:\n $msg")
      case Passed =>
        println(s"+ OK, passed $testCases tests.")
    }
}


case class Prop(run: (MaxSize, TestCases, RNG) => Result) {

  //dont' like a pattern matching here. Looks more like a work for a monad.
  //but how to extract error message without pattern matching?
  def &&(p: Prop) = Prop((s, tc, rng) => {
    val p1r = run(s, tc, rng)
    val p2r = p.run(s, tc, rng)
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

  def ||(p: Prop) = Prop((s, tc, rng) => {
    val p1r = run(s, tc, rng)
    val p2r = p.run(s, tc, rng)
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
