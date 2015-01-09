package projectEuler.solved

import projectEuler.utils.Problem

/**
 * Created by GlebP on 23-Dec-2014.
 */
object Problem102 extends Problem {
  val o: Point = new Point(0, 0)

  override def solve(): Unit = {
    var counter = 0
    for (line <- io.Source.fromFile("src/main/scala/projectEuler/unsolved/p102_triangles.txt").getLines()) {
      val points = line.split(',')
      val a = Point(points(0).toInt, points(1).toInt)
      val b = Point(points(2).toInt, points(3).toInt)
      val c = Point(points(4).toInt, points(5).toInt)
      if (containsOrigin(a, b, c)) counter += 1
    }
    println("result: " + counter)
  }

  def containsOrigin(a: Point, b: Point, c: Point): Boolean = {
    sameSide(o, a, b, c) && sameSide(o, b, a, c) && sameSide(o, c, a, b)
  }

  def sameSide(p1: Point, p2: Point, a: Point, b: Point): Boolean = {
    val cp1 = (b - a) x (p1 - a)
    val cp2 = (b - a) x (p2 - a)
    cp1.toLong * cp2.toLong >= 0
  }

  case class Point(x: Int, y: Int) {
    def -(that: Point): Point = Point(x - that.x, y - that.y)

    def +(that: Point): Point = Point(x + that.x, y + that.y)

    def *(that: Point): Int = x * that.x + y * that.y

    def x(that: Point): Int = x * that.y - y * that.x
  }

}
