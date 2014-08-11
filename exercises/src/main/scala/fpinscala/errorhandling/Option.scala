package fpinscala.errorhandling

import scala.{Option => _, Either => _, _} // hide std library `Option` and `Either`, since we are writing our own in this chapter
import Math.pow

///*
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B]

  def getOrElse[B>:A](default: => B): B

  def flatMap[B](f: A => Option[B]): Option[B]

  def orElse[B>:A](ob: => Option[B]): Option[B]

  def filter(f: A => Boolean): Option[A]
}

case class Some[+A](get: A) extends Option[A] {

  override def map[B](f: A => B): Option[B] = Some(f(get))

  override def getOrElse[B>:A](default: => B): B = get

  override def flatMap[B](f: A => Option[B]): Option[B] = f(get)

  override def orElse[B>:A](ob: => Option[B]): Option[B] = this

  override def filter(f: A => Boolean): Option[A] = if (f(get)) Some(get) else None
}

case object None extends Option[Nothing] {

  override def map[B](f: Nothing => B): Option[B] = None

  override def getOrElse[B](default: => B): B = default

  override def flatMap[B](f: Nothing => Option[B]): Option[B] = None

  override def orElse[B](ob: => Option[B]): Option[B] = ob

  override def filter(f: Nothing => Boolean): Option[Nothing] = None
}
// */

/*
 * A more compact implementation, at the cost of pattern matching overhead:
 *
 * It actually doesn't look that compact to me after all.
 * Perhaps it does when using the 'academic' implementation of some methods.
 * However, these will still do all the pattern matching and in addition, do some strange things,
 * like creating Option(Option(value)) and stuff.
 *
sealed trait Option[+A] {

  def map[B](f: A => B): Option[B] =
  	this match {
      case Some(value) => Some(f(value))
      case None => None
    }

  def getOrElse[B>:A](default: => B): B =
    this match {
      case Some(value) => value
      case None => default
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    this match {
      case Some(value) => f(value)
      case None => None
    }

  // More academic version
//  def flatMap[B](f: A => Option[B]): Option[B] =
//    map(f) getOrElse None

  def orElse[B>:A](ob: => Option[B]): Option[B] =
    this match {
      case Some(value) => this
      case None => ob
    }

  // More academic version
//  def orElse[B>:A](ob: => Option[B]): Option[B] =
//    this.map(Some(_)) getOrElse ob

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(value) if (f(value)) => this
      case _ => None
    }

  // More academic version
//  def filter(f: A => Boolean): Option[A] =
//    this.flatMap(value => if (f(value)) this else None)
}

case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

// */

object Option {
  def failingFn(i: Int): Int = {
    val y: Int = throw new Exception("fail!") // `val y: Int = ...` declares `y` as having type `Int`, and sets it equal to the right hand side of the `=`.
    try {
      val x = 42 + 5
      x + y
    }
    catch { case e: Exception => 43 } // A `catch` block is just a pattern matching block like the ones we've seen. `case e: Exception` is a pattern that matches any `Exception`, and it binds this value to the identifier `e`. The match returns the value 43.
  }

  def failingFn2(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int) // A thrown Exception can be given any type; here we're annotating it with the type `Int`
    }
    catch { case e: Exception => 43 }
  }

  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None
    else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs) flatMap (m => mean(xs map (x => pow(x - m, 2))))

  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a flatMap(av => b.map(bv => f(av, bv)))

//  def sequence[A](list: List[Option[A]]): Option[List[A]] =
//    list match {
//      case x :: xs => x flatMap (a => sequence(xs) map (a :: _))
//      case Nil => Some(Nil)
//    }

  def sequence[A](list: List[Option[A]]): Option[List[A]] =
    traverse(list)(a => a)

  def traverse[A,B](list: List[A])(f: A => Option[B]): Option[List[B]] =
    list match {
      case x :: xs => f(x) flatMap (b => traverse(xs)(f) map (b :: _))
      case Nil => Some(Nil)
  }
}