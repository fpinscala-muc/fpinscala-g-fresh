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

  def toList(): List[A] = {
    @annotation.tailrec
    def go(stream: Stream[A], acc: List[A]): List[A] = stream match {
      case Cons(h, t) => go(t(), h() :: acc)
      case Empty => acc
    }
    go(this, List()) reverse
  }

  def take(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 0) => cons(h(), t().take(n - 1))
      case _ => empty
    }

  def takeViaUnfold(n: Int): Stream[A] = sys.error("todo")

  def drop(n: Int): Stream[A] =
    this match {
      case Cons(h, t) if (n > 0) => t().drop(n - 1)
      case _ => this
    }

  def takeWhile(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (p(a)) cons(a, b) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption(): Option[A] =
    foldRight(Option.empty[A])((a, _) => Option(a))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((a, b) => cons(f(a), b))

  def filter(f: A => Boolean): Stream[A] =
    foldRight(empty[A])((a, b) => if (f(a)) cons(a, b) else b)

  def append[B>:A](s: => Stream[B]): Stream[B] =
    foldRight(s)((a, b) => cons(a, b))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((a, b) => f(a) append b)

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = sys.error("todo")

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] = sys.error("todo")

  def mapViaUnfold[B](f: A => B): Stream[B] = sys.error("todo")

  def zipWith[B,C](s2: Stream[B])(f: (A,B) => C): Stream[C] = sys.error("todo")

  def startsWith[B](s: Stream[B]): Boolean = sys.error("todo")
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

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant[A](a: A): Stream[A] = sys.error("todo")

  def from(n: Int): Stream[Int] = sys.error("todo")

  val fibs: Stream[Int] = sys.error("todo")

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = sys.error("todo")

  val fibsViaUnfold: Stream[Int] = sys.error("todo")

  def fromViaUnfold(n: Int): Stream[Int] = sys.error("todo")

  def constantViaUnfold[A](a: A): Stream[A] = sys.error("todo")

  val onesViaUnfold: Stream[Int] = sys.error("todo")
}