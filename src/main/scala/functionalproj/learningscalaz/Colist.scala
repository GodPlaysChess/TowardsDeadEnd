package functionalproj.learningscalaz

import scalaz._
import Scalaz._

object Colist extends Comonad[List] {
  override def copoint[A](p: List[A]): A = p.head

  override def cobind[A, B](fa: List[A])(f: (List[A]) => B): List[B] = List(f(fa))

  override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
}

object Costream extends Comonad[Stream] {
  override def copoint[A](p: Stream[A]): A = p.head

  override def cobind[A, B](fa: Stream[A])(f: (Stream[A]) ⇒ B): Stream[B] = fa.tails.map(f).toStream //Stream(f(fa)) <- taking jsut Stream is wrong cause we're loosing information

  override def map[A, B](fa: Stream[A])(f: (A) ⇒ B): Stream[B] = fa map f
}

object Cooption extends Comonad[Option] {
  override def copoint[A](p: Option[A]): A = p.get

  override def cobind[A, B](fa: Option[A])(f: (Option[A]) => B): Option[B] = Some(f(fa))

  override def map[A, B](fa: Option[A])(f: (A) => B): Option[B] = fa map f
}

object Examples extends App {
  import Costream._
  val one = List(1, 2, 3)
  val two = List("three", "four", "five")
  println(Colist.cobind(two)(_.length))

  val nats: Stream[Int] = 0 #:: nats map (_ - 1)   // here Stream and Costream are working identically
  val nats1: Stream[Int] = map(0 #:: nats1)(_ + 1)  // because Costream just delegates map to Stream
  val nats2: Stream[(Int, Int)] = cobind(nats1)(firstTwo)

  println("Costream example ")
  println("Shall be 1, 2, 3, 4: " + nats1.take(4).toList)
  println("Shall be 1, 2, 3, 4: " + nats.take(4).toList)
  println("Tupled stream: " + nats2.take(4).toList)
  println(Costream.counzip(leftOrRight).take(4).toList)

  println("Cooption example ")
  println("Co-Option 2: " + Cooption.cobind(Some(2))(getOrDef))
  println("Co-Option 3: " + Cooption.cobind(Some(3))(getOrDef))

  def firstTwo[A](a: Stream[A]): (A, A) = a match {
    case f #:: s #:: _ => f → s
  }

  def getOrDef(a: Option[Int]): Int = a match {
    case None => 0
    case Some(x) => (x % 2 == 0) ? 1 | 0
  }

  def leftOrRight: \/[Stream[Int], Stream[Int]] = {
    -\/(nats)
  }


}
