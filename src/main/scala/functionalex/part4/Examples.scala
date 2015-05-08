package functionalex.part4

object Examples {
  // TODO:
  // equals on colored points
  // <- inheritance
  // CRUD -> DB monad actions
  // interface -> interpreters
  // implicit classes with type classes

  // factorial example w/o side effects
  /**
   * type IntTuple[+A]=(Int, A)
   * Functor[IntTuple].map((1, 2))(a => a + 1)) // (1, 3)   *
   * Functor[({type l[a] = (Int, a)})#l].map((1, 2))(a => a + 1)) // (1, 3)
   */

  def fac(n: Int): Long = (1 until n).product


  def main(args: Array[String]) {
    val io: IO0[Unit] = for {
      _ <- IOActions.PrintLine("Enter a number")
      m <- IOActions.ReadLine.map(_.toInt)
      _ <- IOActions.PrintLine(fac(m).toString)
    } yield ()
    io.run()
  }


}
