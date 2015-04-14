package functionalex.part4

object Examples {
  // TODO:
  // equals on colored points
  // <- inheritance
  // CRUD -> DB monad actions
  // interface -> interpreters
  // implicit classes with type classes

  // factorial example w/o side effects

  def fac(n: Int): Long = (1 until n).product



  def main (args: Array[String]) {
    val io: IO0[Unit] = for {
      _ <- IOActions.PrintLine("Enter a number")
      m <- IOActions.ReadLine.map(_.toInt)
      _ <- IOActions.PrintLine(fac(m).toString)
    } yield ()
    io.run()
  }


}
