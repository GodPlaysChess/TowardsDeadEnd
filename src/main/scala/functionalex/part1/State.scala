package functionalex.part1

case class State[S, +A](run: S => (A, S)) {
  self =>             // just to excersice such usage of "self" instead of "this"

  def unit: State[S, A] =
    State(s => self.run(s))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = self.run(s)
      g(a).run(s1)
    })

  def map[B](f: A => B): State[S, B] =
    State(s => {
      val (a, s1) = self.run(s)
      (f(a), s1)
    })


  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] = ???

  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] = ???
}
