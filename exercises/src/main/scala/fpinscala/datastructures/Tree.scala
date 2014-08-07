package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def size(tree: Tree[Any]): Int =
    tree match {
      case Branch(left, right) => size(left) + 1 + size(right)
      case Leaf(_) => 1
    }

  def maximum(tree: Tree[Int]): Int =
    tree match {
      case Branch(left, right) =>  maximum(left) max maximum(right)
      case Leaf(value) => value
    }

  def depth(tree: Tree[Any]): Int = {
    def traverse(t: Tree[Any], acc: Int): Int = t match {
      case Branch(left, right) => traverse(left, acc + 1) max traverse(right, acc + 1)
      case Leaf(_) => acc
    }
    traverse(tree, 0)
  }

  def map[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    tree match {
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
      case Leaf(value) => Leaf(f(value))
    }

  def fold[A,B,C](tree: Tree[A])(f: A => B)(g: (B, B) => B): B =
    tree match {
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
      case Leaf(value) => f(value)
  	}

  def sizeViaFold(tree: Tree[Any]): Int =
    fold(tree)(v => 1)(_ + 1 + _)

  def maximumViaFold(tree: Tree[Int]): Int =
    fold(tree)(v => v)(_ max _)

  def depthViaFold(tree: Tree[Any]): Int =
    fold(tree)(v => 0)((l, r) => 1 + (l max r))

  def mapViaFold[A,B](tree: Tree[A])(f: A => B): Tree[B] =
    fold(tree)(v => Leaf(f(v)): Tree[B])(Branch(_, _))
}