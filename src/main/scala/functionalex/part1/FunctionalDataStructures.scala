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
  def foldR[A, B](as: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(as), z)((b: B, a: A) => f(a, b))


  def foldL[A, B](as: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(as), z)((a: A, b: B) => f(b, a))

  def append[A](a1: List[A], a2: List[A]): List[A] =
    foldR(a1, a2)((acc, elem) => acc :: elem)

  def flatten[A](ll: List[List[A]]): List[A] =
    foldLeft(ll, List[A]())(append)

  def incBy1(li: List[Int]) = li.map(_ + 1)

  // other ex are too simple and skipped

  def map[A, B](as: List[A])(f: A => B): List[B] = as match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }

  def filter[A](as: List[A])(f: A => Boolean): List[A] = as match {
    case Nil => Nil
    case x :: xs if f(x) => x :: filter(xs)(f)
    case _ :: xs => filter(xs)(f)
  }

  def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = flatten(map(as)(f))

  // using flatmap
  def filter1[A](as: List[A])(f: A => Boolean): List[A] =
    flatMap(as)(a => if (f(a)) List(a) else List())

  def zipWith[A, B, C](a1: List[A], a2: List[B])(f: (A, B) => C): List[C] = a1 match {
    case Nil => Nil
    case x :: xs => a2 match {
      case Nil => Nil
      case y :: ys => f(x, y) :: zipWith(xs, ys)(f)
    }
  }

  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = {
    sup.tails.exists(startsWith(_, sub))
  }

  def startsWith[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case x :: xs => sup match {
      case y :: ys if y == x => startsWith(ys, xs)
      case _ => false
    }
  }

  /** ===== Tree ===== */

  def size[A](tr: Tree[A]): Int = tr match {
    case Branch(left, right) => 1 + size(left) + size(right)
    case _ => 1
  }

  def maximum(tr: Tree[Int]): Int = tr match {
    case Branch(left, right) => maximum(left).max(maximum(right))
    case Leaf(value) => value
  }

  def depth[A](tr: Tree[A]): Int = tr match {
    case Branch(left, right) => 1 + depth(left).max(depth(right))
    case _ => 1
  }

  def map[A, B](tr: Tree[A])(f: A => B): Tree[B] = tr match {
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    case Leaf(value) => Leaf(f(value))
  }

  def fold[A, B](tr: Tree[A], z: B)(f: (A, B) => B): B = tr match {
    case Branch(left, right) => fold(right, fold(left, z)(f))(f)
    case Leaf(value) => f(value, z)
  }

  def main(args: Array[String]) {
    val a = Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))
    println(size(a))
    println(maximum(a))
    println(depth(a))
    println(map(a)(_ + 1))
    println(fold(a, 0)(_ + _))

    println("\t Lists \n\n")
    println(foldRight(List(1, 2, 3), "")(_ + _))
    println(foldLeft(List(1, 2, 3), "")(_ + _))
    println(append(List(1, 2, 3), List(4, 5, 6)))
    println(flatten(List(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9))))
    println(flatMap(List(1, 2, 3))(i => List(i, i)))
    println(filter1(List(1, 2, 3, 4, 5))(_ % 2 == 0))
  }

}
