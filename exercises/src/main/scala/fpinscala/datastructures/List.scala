package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: List[A]) extends List[A] // Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`, which may be `Nil` or another `Cons`.

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] =
    foldRight(a1, a2)((element, tail) => Cons(element, tail))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  def product3(ns: List[Double]): Double = {
    def go(l: List[Double]): Double = l match {
      case Cons(x, xs) => if (x != 0.0) x * go(xs) else 0.0
      case Nil => 1.0
    }
    go(ns)
  }

  def tail[A](l: List[A]): List[A] =
    l match {
      case Cons(x, xs) => xs
      case Nil => throw new UnsupportedOperationException("Cannot get tail of empty list")
    }

  def setHead[A](l: List[A], h: A): List[A] =
    l match {
      case Cons(x, xs) => Cons(h, xs)
      case Nil => Cons(h, Nil)
    }

  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] =
    l match {
      case Cons(x, xs) if (n > 0) => drop(xs, n - 1)
      case _ => l
    }

  @annotation.tailrec
  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] =
    l match {
      case Cons(x, xs) if (f(x)) => dropWhile(xs)(f)
      case _ => l
    }

  def init[A](l: List[A]): List[A] =
    l match {
      case Cons(x, Nil) => Nil
      case Cons(x, xs) => Cons(x, init(xs))
      case Nil => throw new UnsupportedOperationException("Cannot get init of empty list")
    }

  def length[A](l: List[A]): Int =
    foldRight(l, 0)((_, counter) => counter + 1)

  @annotation.tailrec
  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B =
    l match {
      case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      case Nil => z
    }

  def sumViaFold(ints: List[Int]): Int =
    foldLeft(ints, 0)((sum, element) => element + sum)

  def productViaFold(ints: List[Int]): Int =
    foldLeft(ints, 1)((product, element) => element * product)

  def lengthViaFold[A](l: List[A]): Int =
    foldLeft(l, 0)((counter, _) => counter + 1)

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, Nil:List[A])((result, element) => Cons(element, result))

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(reverse(l), z)((a, b) => f(b, a))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def concat[A](l: List[List[A]]): List[A] =
   	foldLeft(l, Nil:List[A])((result, list) => append(result, list)) // this is not linear in the length of all lists

  def incrementByOne(ints: List[Int]): List[Int] =
  	foldRight(ints, Nil:List[Int])((int, result) => Cons(int + 1, result))

  def mapToString(l: List[Double]): List[String] =
    foldRight(l, Nil:List[String])((element, result) => Cons(element.toString, result))

  def map[A,B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((element, result) => Cons(f(element), result))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A])((element, result) => if (f(element)) Cons(element, result) else result)

  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    foldRight(l, Nil:List[B])((element, result) => append(f(element), result))

  def filterViaFlatMap[A,B](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(element => if (f(element)) List(element) else Nil)

  def sumPairwise(a: List[Int], b: List[Int]): List[Int] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(x + y, sumPairwise(xs, ys))
    }

  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A, B) => C): List[C] =
    (a, b) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (Cons(x, xs), Cons(y, ys)) => Cons(f(x, y), zipWith(xs, ys)(f))
    }

  def startsWith[A](l: List[A], prefix: List[A]): Boolean =
    (l, prefix) match {
      case (_, Nil) => true
      case (Cons(x, xs), Cons(y, ys)) if (x == y) => startsWith(xs, ys)
      case _ => false
    }

  @tailrec
  def hasSubsequence[A](l: List[A], sub: List[A]): Boolean =
    l match {
      case Cons(x, xs) if (startsWith(l, sub)) => true
      case Cons(x, xs) => hasSubsequence(xs, sub)
      case Nil => false
    }
}