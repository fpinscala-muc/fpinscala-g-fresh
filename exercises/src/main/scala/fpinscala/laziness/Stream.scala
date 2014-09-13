package fpinscala.laziness

import scala.annotation.tailrec

import Stream._
trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  // Exercise 5.1
  def toList: List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }
    go(this, List()) reverse
  }

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 0) => t().drop(n - 1)
      case _ => this
    }

  // Exercise 5.2
  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
      case _ => empty
    }

  // Exercise 5.13
  def takeViaUnfold(n: Int): Stream[A] =
    unfold(this, n) {
      case (Cons(h, t), n) if n > 0 => Some(h(), (t(), n - 1))
      case _ => None
  	}

  // Exercise 5.4
  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  // Exercise 5.5
  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  // Exercise 5.13
  def takeWhileViaUnfold(p: A => Boolean): Stream[A] =
    unfold(this) {
      case Cons(h, t) if p(h()) => Some(h(), t())
      case _ => None
    }

  // Exercise 5.6
  def headOption: Option[A] =
    foldRight(Option.empty[A])((a, _) => Option(a))

  // Exercise 5.13
  def mapViaUnfold[B](f: A => B): Stream[B] =
  	unfold(this) {
      case Cons(h, t) => Some(f(h()), t())
      case Empty => None
    }

  def mapViaUnfold2[B](f: A => B): Stream[B] =
    unfold(this)(s => for (h <- s.headOption) yield (f(h), s.drop(1)))

  // Exercise 5.7
  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  // Exercise 5.13
  def zipWith[B, C](other: Stream[B])(f: (A, B) => C): Stream[C] =
    unfold(this, other) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some(f(h1(), h2()), (t1(), t2()))
      case _ => None
    }

  def zipAll[B](other: Stream[B]): Stream[(Option[A], Option[B])] =
    unfold(this, other) {
      case (Cons(h1, t1), Cons(h2, t2)) => Some((Some(h1()), Some(h2())), (t1(), t2()))
      case (Cons(h1, t1), Empty) => Some((Some(h1()), None), (t1(), Empty))
      case (Empty, Cons(h2, t2)) => Some((None, Some(h2())), (Empty, t2()))
      case _ => None
    }

  def hasSubsequence[A](other: Stream[A]): Boolean =
    tails.exists(_.startsWith(other))

  // Exercise 5.14
  def startsWith[A](other: Stream[A]): Boolean =
    zipAll(other).takeWhile {
      case (a, b) => ! b.isEmpty
    } forAll {
      case (a, b) => a == b
    }

  // Exercise 5.15
  def tails: Stream[Stream[A]] =
    unfold(this) {
      case s @ Cons(_, t) => Some(s, t())
      case Empty => None
    } append Stream(empty)

  // Exercise 5.16
  def scanRight[B](z: => B)(f: (A, => B) => B): Stream[B] =
    tails.map(_.foldRight(z)(f))	// I guess this is not quite right yet
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  // Exercise 5.11
  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  // Exercise 5.12
  val ones: Stream[Int] =
  	constant(1)

  def constant[A](a: A): Stream[A] =
  	unfold(a)(a => Some(a, a))

  def from(n: Int): Stream[Int] =
    unfold(n)(n => Some(n, n + 1))

  def fibs(): Stream[Int] =
    unfold((0, 1)) { case (f1, f2) => Some(f1, (f2, f1 + f2))}
}