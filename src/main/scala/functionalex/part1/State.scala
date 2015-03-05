package functionalex.part1

case class State[S, +A](run: S => (A, S)) {
  self =>
  // just to excersice such usage of "self" instead of "this"

  def unit: State[S, A] =
    State(s => self.run(s))

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = self.run(s)
      g(a).run(s1)
    })

  /**
   * the same as
   * State(s => {
   * val (a, s1) = self.run(s)
   * (f(a), s1)
   * })
   */
  def map[B](f: A => B): State[S, B] =
    flatMap(a => State(s => (f(a), s)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(f(a, _)))

  /**
   * def sequence[A](fs: List[State[S, A]]): State[S, List[A]] =
   * State(s =>
   * fs.foldLeft((List.empty[A], s))((acc, state) => {
   * val (a, s1) = state.run(s)
   * (a :: acc._1, s1)
   * }))
   */
  def sequence[A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft[State[S, List[A]]](State(s => (List.empty[A], s))) {
      (acc, state) => acc.map2(state)(_.::(_))
    }

}

object State {
  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))
}
