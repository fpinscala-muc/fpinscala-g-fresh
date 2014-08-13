package fpinscala.errorhandling


import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter

sealed trait Either[+E,+A] {

  def map[B](f: A => B): Either[E, B]

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B]

  def orElse[EE >: E, B >: A](b: => Either[EE, B]): Either[EE, B]

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C]
}

case class Left[+E](get: E) extends Either[E, Nothing] {

  override def map[B](f: Nothing => B): Either[E, B] = this

  override def flatMap[EE >: E, B](f: Nothing => Either[EE, B]): Either[EE, B] = this

  override def orElse[EE >: E, B >: Nothing](b: => Either[EE, B]): Either[EE, B] = b

  override def map2[EE >: E, B, C](b: Either[EE, B])(f: (Nothing, B) => C): Either[EE, C] = this
}

case class Right[+A](get: A) extends Either[Nothing, A] {

  override def map[B](f: A => B): Either[Nothing, B] = Right(f(get))

  override def flatMap[EE >: Nothing, B](f: A => Either[EE, B]): Either[EE, B] = f(get)

  override def orElse[EE >: Nothing, B >: A](b: => Either[EE, B]): Either[EE, B] = this

  override def map2[EE >: Nothing, B, C](other: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    other map (b => f(get, b))
}

object Either {
  def mean(xs: IndexedSeq[Double]): Either[String, Double] =
    if (xs.isEmpty)
      Left("mean of empty list!")
    else
      Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Exception, Int] =
    try Right(x / y)
    catch { case e: Exception => Left(e) }

  def Try[A](a: => A): Either[Exception, A] =
    try Right(a)
    catch { case e: Exception => Left(e) }

  def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
    traverse(es)(a => a)

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case head :: tail => f(head) flatMap (b => traverse(tail)(f) map (b :: _))
      case Nil => Right(Nil)
    }
}