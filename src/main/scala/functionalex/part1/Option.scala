package functionalex.part1

/**
 * It is simple to do everything with pattern matching,
 * but the exercise was to use it only for <b>map</b>
 * and <b>getOrElse</b> 
 *
 * But does it actually make sense not to use pattern matching, if it would be used anyway.
 */
sealed trait Option[+A] {
  self =>

  def map[B](f: A => B): Option[B] = self match {
    case Some(x) => Some(f(x))
    case None => None
  }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f) getOrElse None

  def flatMap1[B](f: A => Option[B]): Option[B] = self match {
    case Some(x) => f(x)
    case None => None
  }


  def getOrElse[B >: A](default: => B): B = self match {
    case Some(x) => x
    case None => default
  }

  def orElse[B >: A](ob: => Option[B]): Option[B] = self match {
    case None => ob
    case _ => self
  }


  def filter(f: A => Boolean): Option[A] =
    flatMap(x => if (f(x)) self else None)

}

case object None extends Option[Nothing]

case class Some[+A](get: A) extends Option[A]



