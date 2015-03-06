package functionalex.part1

import State._

case class State[S, +A](run: S => (A, S)) {

  def flatMap[B](g: A => State[S, B]): State[S, B] =
    State(s => {
      val (a, s1) = run(s)
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
    flatMap(a => unit(f(a)))

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    flatMap(a => sb.map(f(a, _)))





}

object State {
  type Rand[A] = State[RNG, A]

  def unit[S, A](a: A): State[S, A] =
    State(s => (a, s))

  /**
   * def sequence[A](fs: List[State[S, A]]): State[S, List[A]] =
   * State(s =>
   * fs.foldLeft((List.empty[A], s))((acc, state) => {
   * val (a, s1) = state.run(s)
   * (a :: acc._1, s1)
   * }))
   */
  def sequence[S, A](fs: List[State[S, A]]): State[S, List[A]] =
    fs.foldLeft[State[S, List[A]]](State(s => (List.empty[A], s))) {
      (acc, state) => acc.map2(state)(_.::(_))
    }

  def sequence1[S, A](sas: List[State[S, A]]): State[S, List[A]] =
    sas.foldRight(unit[S, List[A]](List()))(_.map2(_)(_ :: _))

  def get[S]: State[S, S] = State(s => (s, s))

  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def modify[S](f: S => S): State[S, Unit] = for {
    s <- get
    _ <- set(f(s))
  } yield ()
}
