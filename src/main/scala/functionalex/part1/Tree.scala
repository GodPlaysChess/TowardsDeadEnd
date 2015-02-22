package functionalex.part1

/**
 * Created by Gleb on 2/21/2015.
 */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]



