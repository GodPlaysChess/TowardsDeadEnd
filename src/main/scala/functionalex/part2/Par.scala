package functionalex.part2

import java.util.concurrent.{Callable, TimeUnit, Future, ExecutorService}

object Par {
  type Par[A] = ExecutorService => Future[A]

  /**
   * computation that immediately results in a value `a`
   **/
  def unit[A](a: A): Par[A] = (_: ExecutorService) => UnitFuture(a)

  /**
   * wraps the expression `a` for concurrent evaluation
   */
  def lazyUnit[A](a: => A): Par[A] = fork(unit(a))

  def run[A](s: ExecutorService)(a: Par[A]): Future[A] = a(s)

  /** def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] = for {
    * aa <- a
    * bb <- b
    * } yield f(aa, bb
    *
    * Combines the results of a two parallel computations with a binary function
    */
  def map2[A, B, C](a: Par[A], b: Par[B])(f: (A, B) => C): Par[C] =
    (es: ExecutorService) => {
      val af = a(es)
      val bf = b(es)
      UnitFuture(f(af.get, bf.get))
    }

  def map3[A, B, C, D](a: Par[A], b: Par[B], c: Par[C])(f: (A, B, C) => D): Par[D] = {
    val (g1, g2) = f.curried
    map2(map2(a, b)(g1), c)(g2)
  }

  def map4[A, B, C, D, E](a: Par[A], b: Par[B], c: Par[C], d: Par[D])(f: (A, B, C, D) => E): Par[E] = {
    val (g1, g2, g3) = f.curried
    map2(map2(map2(a, b)(g1), c)(g2), d)(g3)
  }

  def map5[A, B, C, D, E, F](a: Par[A], b: Par[B], c: Par[C], d: Par[D], e: Par[E])(f: (A, B, C, D, E) => F): Par[F] = {
    val (g1, g2, g3, g4) = f.curried
    map2(map2(map2(map2(a, b)(g1), c)(g2), d)(g3), e)(g4)
  }

  def map[A, B](pa: Par[A])(f: A => B): Par[B] =
    map2(pa, unit(()))((a, _) => f(a))

  def paragraphs(ls :List[String])(implicit es: ExecutorService): Int = {
    val t: Par[Int] = map(parMap(ls)(_.split(' ').size))(_.sum)
    t.apply(es).get
  }

  // makes execution of `a` in a separate thread
  def fork[A](a: => Par[A]): Par[A] = es =>
    es.submit(new Callable[A] {
      override def call(): A = a(es).get
    })

  def asyncF[A, B](f: A => B): A => Par[B] =
    a => lazyUnit(f(a))

  def sortPar(parList: Par[List[Int]]): Par[List[Int]] =
    map(parList)(_.sorted)

  def parMap[A, B](ps: List[A])(f: A => B): Par[List[B]] = fork {
    val fbs: List[Par[B]] = ps.map(asyncF(f))
    sequence(fbs)
  }

  def sequence[A](ps: List[Par[A]]): Par[List[A]] =
    ps.foldRight(unit(List.empty[A]))((par, li) => map2(par, li)(_ :: _))

  // this is kinda uses flatMap
  // A => EmptyList or List(A) , then concat
  // map actually DOES stuff, and sequence COMBINES stuff, so I need to change the "sequence" to make "exclude" some values from combination
  def parFilter[A](as: List[A])(f: A => Boolean): Par[List[A]] = {
    val y: Par[List[List[A]]] = parMap(as)(x => if (f(x)) List(x) else List.empty[A]) // this goes parallel
    map(y)(ll => ll.foldRight(List.empty[A])((l1, l2) => if (l1.isEmpty) l2 else l1.head :: l2)) //is thi sequential?
  }


  private case class UnitFuture[A](get: A) extends Future[A] {
    override def cancel(mayInterruptIfRunning: Boolean): Boolean = false

    override def isCancelled: Boolean = false

    /**
     * In order to respect timeouts,
     * we’d need a new Future implementation that
     * records the amount of time spent evaluating
     * af, and then subtracts that time from the
     * available time allocated for evaluating bf.
     */
    override def get(timeout: Long, unit: TimeUnit): A = get

    override def isDone: Boolean = true
  }

}