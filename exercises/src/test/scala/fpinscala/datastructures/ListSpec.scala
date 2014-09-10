
package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import fpinscala.datastructures.List._

@RunWith(classOf[JUnitRunner])
class ListSpec extends FlatSpec {

  // Preface: make sure the constructor functions used in the following tests work out as expected
  behavior of "apply"

  it should "return an empty list when invoked without arguments" in {
    assert(List() === Nil)
  }

  it should "return a list containing the given arguments" in {
    assert(List(1) === Cons(1, Nil))
    assert(List(1, 1, 2) === Cons(1, Cons(1, Cons(2, Nil))))
  }

  // Exercise 3.1
  "the given expression" should "result in the value 3" in {
    val result = List(1, 2, 3, 4, 5) match {
      case Cons(x, Cons(2, Cons(4, _))) => x
      case Nil => 42
      case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
      case Cons(h, t) => h + sum(t)
      case _ => 101
    }
    assert(result === 3)
  }

  // Exercise 3.2
  behavior of "tail"

  it should "return a list which is the given list without the first element" in {
    assert(tail(List(1, 2)) === List(2))
    assert(tail(List(1, 2, 3)) === List(2, 3))
    assert(tail(List(3, 2, 1)) === List(2, 1))
  }

  it should "return an empty list when invoked on a list with one element" in {
    assert(tail(List(1)) === Nil)
  }

  it should "throw UnsupportedOperationException when invoked on an empty list" in {
    intercept[UnsupportedOperationException] (tail(List()))
  }

  // Exercise 3.3
  behavior of "setHead"

  it should "return a list which is the given list with the first element replaced by the given value" in {
    assert(setHead(List(1, 2, 3), 4) === List(4, 2, 3))
    assert(setHead(List(1), 2) === List(2))
  }

  it should "return a one element list with the given value when invoked on an empty list" in {
    assert(setHead(Nil, 1) == List(1))
  }

  // Exercise 3.4
  behavior of "drop"

  it should "return a list which is the given list with the first n elements removed" in {
    assert(drop(List(1, 1), 1) === List(1))
    assert(drop(List(1, 2, 3), 1) === List(2, 3))
    assert(drop(List(1, 2, 3), 2) === List(3))
  }

  it should "return an empty list when invoked on a list with n or less elements" in {
    assert(drop(Nil, 1) === Nil)
    assert(drop(Nil, 3) === Nil)
    assert(drop(List(1), 1) === Nil)
    assert(drop(List(1), 3) === Nil)
    assert(drop(List(1, 2, 3), 4) === Nil)
  }

  // Exercise 3.5
  behavior of "dropWhile"

  it should "work" in {
    assert(dropWhile(List(1))(_ > 0) === Nil)
    assert(dropWhile(List(0, 0))(_ > 0) === List(0, 0))
    assert(dropWhile(List(1, 0, 1, 1))(_ > 0) === List(0, 1, 1))
  }

  it should "return an empty list when invoked on an empty list" in {
    assert(dropWhile(List[Int]())(_ => true) === Nil)
  }

  // Exercise 3.6
  behavior of "init"

  it should "return a list which is the given list without the last element" in {
    assert(init(List(1, 2)) === List(1))
    assert(init(List(3, 4, 5)) === List(3, 4))
  }

  it should "return an empty list when invoked on a list with only one element" in {
    assert(init(List(1)) === Nil)
  }

  it should "throw UnsupportedOperationException when invoked on an empty list" in {
    intercept[UnsupportedOperationException] (init(List()))
  }

  // Exercise 3.7

  // Answer: Short circuit logic cannot be implemented with the current version of foldRight/foldLeft.
  //         A lazily evaluated version of foldRIght/foldLeft would have this property.

  // Exercise 3.8
  behavior of "foldRight"

  it should "fold the list from end to start" in {
    assert(foldLeft(List(1, 2, 3), "")((element, result) => result + element) === "321")
  }

  it should "return an identical list when handed the list constructor functions" in {
    val result = foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _))
    assert(result === List(1, 2, 3))
  }

  // Exercise 3.9
  behavior of "length"

  it should "return the number of elements in the list" in {
    assert(length(List()) === 0)
    assert(length(List(1)) === 1)
    assert(length(List(1, 2, 3)) === 3)
  }

  // Exercise 3.10
  behavior of "foldLeft"

  it should "fold the list from start to end" in {
    assert(foldLeft(List(1, 2, 3), "")((result, element) => result + element) === "123")
  }

  // Exercise 3.11
  behavior of "sum, product, length via fold"

  it should "work just like the original versions" in {
    assert(sumViaFold(List(1, 2, 3, 4)) === 10)
    assert(sumViaFold(Nil) === 0)
    assert(productViaFold(List(1, 2, 3, 4)) === 24)
    assert(productViaFold(Nil) === 1)
    assert(lengthViaFold(Nil) === 0)
    assert(lengthViaFold(List(42)) === 1)
    assert(lengthViaFold(List(1, 2, 3, 4)) === 4)
  }

  // Exercise 3.12
  behavior of "reverse"

  it should "return the list reversed" in {
    assert(reverse(List(1, 2, 3)) === List(3, 2, 1))
    assert(reverse(Nil) === Nil)
  }

  // Exercise 3.13
  behavior of "foldLeftViaFoldRight"

  it should "work just like foldLeft" in {
    def mkString[A](s: String, element: A): String = s + element
    assert(foldLeftViaFoldRight(List(1, 2, 3), "")(mkString) === "123")
    assert(foldLeftViaFoldRight(List(1), "")(mkString) === "1")
    assert(foldLeftViaFoldRight(List(), "")(mkString) === "")
  }

  behavior of "foldRightViaFoldLeft"

  it should "work just like foldRight" in {
    def mkString[A](element: A, s: String): String = s + element
    assert(foldRightViaFoldLeft(List(1, 2, 3), "")(mkString) === "321")
    assert(foldRightViaFoldLeft(List(1), "")(mkString) === "1")
    assert(foldRightViaFoldLeft(List(), "")(mkString) === "")
  }

  // Exercise 3.14
  behavior of "append via foldRight"

  it should "return a list which is the first argument with the second argument appended" in {
    assert(append(List(), List()) === List())
    assert(append(List(), List(1)) === List(1))
    assert(append(List(1), List()) === List(1))
    assert(append(List(), List(1, 2)) === List(1, 2))
    assert(append(List(1, 2), List()) === List(1, 2))
    assert(append(List(1), List(2)) === List(1, 2))
    assert(append(List(1, 2, 3), List(1, 2, 3)) === List(1, 2, 3, 1, 2, 3))
  }

  // Exercise 3.15
  behavior of "concat"

  it should "flatten the list of lists" in {
    assert(concat(List()) === List())
    assert(concat(List(List())) === List())
    assert(concat(List(List(), List())) === List())
    assert(concat(List(List(1), List())) === List(1))
    assert(concat(List(List(), List(1))) === List(1))
    assert(concat(List(List(1), List(1))) === List(1, 1))
    assert(concat(List(List(1, 2), List())) === List(1, 2))
    assert(concat(List(List(), List(1, 2), List())) === List(1, 2))
    assert(concat(List(List(), List(1, 2))) === List(1, 2))
  }

  // Exercise 3.16
  behavior of "incrementByOne"

  it should "return a list which contains all the elements of the list incremented by one" in {
    assert(incrementByOne(Nil) === Nil)
    assert(incrementByOne(List(1)) === List(2))
    assert(incrementByOne(List(2)) === List(3))
    assert(incrementByOne(List(1, 2, 3, 4, 5)) === List(2, 3, 4, 5, 6))
  }

  // Exercise 3.17
  behavior of "mapToString"

  it should "return a list containing all elements of the list converted to strings" in {
    assert(mapToString(List(0.5, 3.1415, 42)) === List("0.5", "3.1415", "42.0"))
  }

  // Exercise 3.18
  behavior of "map"

  it should "return an empty list if invoked on an empty list" in {
    assert(map(List[Int]())(_ * 2) === Nil)
  }

  it should "apply the function to the list elements" in {
    assert(map(List(1, 2, 3))(_ * 2) === List(2, 4, 6))
  }

  // Exercise 3.19
  behavior of "filter"

  it should "return a list including all elements from the list satisfying the predicate" in {
    assert(filter(List(1, 2, 3, 4, 5, 6))(_ % 2 == 0) === List(2, 4, 6))
    assert(filter(List(1, 2, 3))(_ < 10) === List(1, 2, 3))
    assert(filter(List(1, 2, 3))(_ > 10) === Nil)
    assert(filter(Nil)(_ => true) === Nil)
  }

  // Exercise 3.20
  behavior of "flatMap"

  it should "return a flat list containing the results of the map operation" in {
    assert(flatMap(List())(e => List(e, e)) === List())
    assert(flatMap(List(1))(e => List(e, e)) === List(1, 1))
    assert(flatMap(List(1, 2))(e => List(e, e)) === List(1, 1, 2, 2))
    assert(flatMap(List(1, 2, 3))(e => List(e, e)) === List(1, 1, 2, 2, 3, 3))
  }

  // Exercise 3.21
  behavior of "flatMapViaFilter"

  it should "return a flat list containing the results of the map operation" in {
    assert(flatMap(List())(e => List(e, e)) === List())
    assert(flatMap(List(1))(e => List(e, e)) === List(1, 1))
    assert(flatMap(List(1, 2))(e => List(e, e)) === List(1, 1, 2, 2))
    assert(flatMap(List(1, 2, 3))(e => List(e, e)) === List(1, 1, 2, 2, 3, 3))
  }

  // Exercise 3.22
  behavior of "sumPairwise"

  it should "sum corresponding elements from the two lists" in {
    assert(sumPairwise(List(1, 2, 3), List(4, 5, 6)) === List(5, 7, 9))
  }

  it should "not produce a value if either of the lists is exhausted" in {
    assert(sumPairwise(Nil, Nil) === Nil)
    assert(sumPairwise(List(1), Nil) === Nil)
    assert(sumPairwise(Nil, List(1, 2)) === Nil)
    assert(sumPairwise(List(1, 2), List(3)) === List(4))
    assert(sumPairwise(List(1, 1, 1), List(2, 3)) === List(3, 4))
  }

  // Exercise 3.23
  behavior of "zipWith"

  it should "pair corresponding elements using the function" in {
    assert(zipWith(List(1, 2, 3), List(1, 2, 3))(_ + _) === List(2, 4, 6))
    assert(zipWith(List(1, 2, 3), List(1, 2, 3))(_ * _) === List(1, 4, 9))
    assert(zipWith(List(1, 2, 3), List(1, 2))(_ - _) === List(0, 0))
    assert(zipWith(Nil: List[Int], Nil)(_ + _) === Nil)
    assert(zipWith(List(1, 2), Nil)(_ + _) === Nil)
  }

  // Exercise 3.24
  behavior of "hasSubsequence"

  it should "return true if the list contains the sub-sequence" in {
    assert(hasSubsequence(List(1, 2, 3), List(1)) === true)
    assert(hasSubsequence(List(1, 2, 3), List(2)) === true)
    assert(hasSubsequence(List(1, 2, 3), List(3)) === true)
    assert(hasSubsequence(List(1, 2, 3), List(1, 2)) === true)
    assert(hasSubsequence(List(1, 2, 3), List(2, 3)) === true)
    assert(hasSubsequence(List(1, 2, 3), List(1, 2, 3)) === true)
  }

  it should "return false if the list doesn't contain the sub-sequence" in {
    assert(hasSubsequence(Nil, List(1)) === false)
    assert(hasSubsequence(Nil, List(1, 2, 3)) === false)
    assert(hasSubsequence(List(1), List(1, 2, 3)) === false)
    assert(hasSubsequence(List(1, 2), List(1, 2, 3)) === false)
  }

  it should "return true if the sub-sequence is an empty list" in {
//    assert(hasSubsequence(Nil, Nil) === true) // not sure about this one
    assert(hasSubsequence(List(1), Nil) === true)
    assert(hasSubsequence(List(1, 2, 3), Nil) === true)
  }
}
