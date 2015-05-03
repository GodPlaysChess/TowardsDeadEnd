package functionalproj.learningscalaz

import scalaz.Scalaz._
import scalaz._

object LearningStates {

  def main(args: Array[String]) {
    val bimapExample: IndexedStateT[Id.Id, Int, Int, String] =
      sm.bimap(_ + 10)(if (_) "F" else "T")
    val xMapExample: IndexedStateT[Id.Id, String, String, Boolean] =
      is.xmap[String, String]("changed string to " + _)(_.length)
    val contramapExample = is.contramap[String](_.length)
    val imapExample = is.imap(List(_))

    println(sm.run(1))
    val iss = is.run(1)
    println("just when running 1: " + iss)
    println(" biMap: " + bimapExample.run(1))
    println(" xMap: " + iss + " -> " + xMapExample.run("one"))
    println(" contramapExample: " + iss + " -> " + contramapExample.run("contramap")) //isn't it the same as just map? we're modifying S1 argument
    println(" imapExample: " + iss + " -> " + imapExample.run(1))
    // Indexed Reader-Writer State
    val writer: IRWST[Id.Id, List[Int], Hp, Int, String, Boolean] = is.rwst[Hp, List[Int]]
    val t: Writer[String, Int] = 5.set("another writer")

    println("\n Writer  \n " + writer.run(List(9, 1, 2), 1))
    println("\n Writer  \n " + writer.run(List(1), 1))
    println("\n Writer  \n " + t.run)

  }

  def sm: State[Int, Boolean] = for {
    x <- get[Int]
    _ <- put(x + 1)
  } yield x > 2

  def is: IndexedState[Int, String, Boolean] = for {
    x <- get[Int]
    _ <- iPut(x + " changed")
  } yield x > 2

  implicit val hpMonoid = new Monoid[Hp] {
    override def zero: Hp = Hp(0d)

    override def append(f1: Hp, f2: => Hp): Hp = Hp(f1.hp + f2.hp)
  }

  case class Hp(hp: Double)

}
