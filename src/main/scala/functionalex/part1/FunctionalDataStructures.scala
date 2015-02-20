package functionalex.part1

import scala.annotation.tailrec

object FunctionalDataStructures {

  def tail[A](list: List[A]): List[A] = list match {
    case Nil => Nil
    case _ :: xs => xs
  }

  def setHead[A](list: List[A], head: A): List[A] = head :: tail(list)

  def drop[A](l: List[A], n: Int): List[A] = l match {
    case _ if n == 0 => l
    case Nil => Nil
    case _ :: xs => drop(xs, n - 1)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case x :: xs if f(x) => dropWhile(xs, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case x :: Nil => x :: Nil
    case x :: xs => x +: init(xs)
  }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = as match {
    case Nil => z
    case x :: xs => f(x, foldRight(xs, z)(f))
  }


  def length[A](as: List[A]): Int = foldRight(as, 0)((_, z) => z + 1)

  @tailrec
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case x :: xs => foldLeft(xs, f(z, x))(f)
  }

  def reverse[A](l: List[A]): List[A] = {
    //    (List[A]() /: l)((x, xs) => xs :: x)
    foldLeft(l, List[A]())((x, xs) => xs :: x)
  }

  // foldRight with foldLeft
  def foldR[A, B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))
  }

  def foldL[A, B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))
  }

  // wiht use of folds
  def append[A](a1: List[A], a2:List[A]): List[A] = {
    ???
  }

  // write concatenate


  def main(args: Array[String]) {
    print(foldRight(List(1, 2, 3), "")(_ + _))
  }

}
