
package fpinscala.laziness

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import fpinscala.laziness.Stream._

@RunWith(classOf[JUnitRunner])
class StreamSpec extends FlatSpec with Matchers {

  // Exercise 5.1

  "toList, applied to a non-empty stream," should "return a list containing the stream's elements" in {
    Stream(1).toList should equal (List(1))
    Stream(1, 2, 3).toList should equal (List(1, 2, 3))
  }

  "toList, applied to an empty stream," should "return an empty list" in {
    Stream().toList should equal (List())
  }

  // Exercise 5.2

  "take(n), applied to a non-empty stream," should "return a stream containing the first n elements" in {
    Stream(1, 2, 3).take(0).toList should equal (List())
    Stream(1, 2, 3).take(1).toList should equal (List(1))
    Stream(1, 2, 3).take(2).toList should equal (List(1, 2))
    Stream(1, 2, 3).take(3).toList should equal (List(1, 2, 3))
  }

  "take(n), applied to a stream with less than n elements," should "return the full stream" in {
    Stream(1).take(3).toList should equal (List(1))
  }

  "take, applied to an empty stream," should "return an empty stream" in {
    Stream.empty.take(3).toList should equal (List())
  }

  "drop(n), applied to a stream of more than n elements," should "return the stream without the first n elements" in {
    Stream(1, 2, 3).drop(1).toList should equal (List(2, 3))
    Stream(1, 2, 3).drop(2).toList should equal (List(3))
  }

  "drop(n), applied to a stream of n or less elements," should "return an empty stream" in {
    Stream(1, 2, 3).drop(3).toList should equal (List())
    Stream.empty.drop(1).toList should equal (List())
  }

  // Exercise 5.3

  def even(int: Int): Boolean = int % 2 == 0

  behavior of "takeWhile, applied to a non-empty stream,"

  it should "return a stream which is the longest prefix satisfying the given predicate" in {
    Stream(2).takeWhile(even).toList should equal(List(2))
    Stream(2, 4, 5).takeWhile(even).toList should equal(List(2, 4))
    Stream(3, 4, 5, 6).takeWhile(even).toList should equal(List())
  }

  behavior of "takeWhile, applied to an empty stream,"

  it should "return an empty steam" in {
    Stream().takeWhile(even).toList should equal (List())
  }

  // Exercise 5.4

  behavior of "forAll, applied to a non-empty stream,"

  it should "return true if all elements satisfy the given predicate" in {
    Stream(2).forAll(even) should equal(true)
    Stream(0, 2, 4).forAll(even) should equal(true)
  }

  it should "return false if any element doesn't satisfy the given predicate" in {
    Stream(1).forAll(even) should equal(false)
    Stream(1, 3).forAll(even) should equal(false)
    Stream(0, 1, 2, 3).forAll(even) should equal(false)
  }

  behavior of "forAll, applied to an empty stream,"

  it should "return true" in {
    Stream.empty[Int].forAll(even) should equal (true)
  }

  // Exercise 5.5


  // Exercise 5.6

  "headOption, applied to a non-empty stream," should "return Some of the first element" in {
    Stream(1, 2, 3).headOption should equal(Some(1))
  }

  "headOption, applied to an empty stream," should "return None" in {
    Stream().headOption should equal(None)
  }

  // Exercise 5.7

  "map, applied to a non-empty stream," should "return apply the given function to the stream elements" in {
	Stream(1, 2, 3).map(_ * 2).toList should equal(List(2, 4, 6))
  }

  "map, applied to an empty stream," should "return an empty stream" in {
    Stream.empty[Int].map(_ * 2).toList should equal(List())
  }

  "filter" should "return a stream containing only the elements satisfying the given predicate" in {
	Stream(1, 2, 3, 4).filter(even).toList should equal(List(2, 4))
  }

  "filter, applied to an empty stream," should "return an empty stream" in {
    Stream().filter(even).toList should equal(List())
  }

  "append" should "return the concatenation of this and the given stream" in {
    Stream().append(Stream()).toList should equal(List())
    Stream().append(Stream(1, 2)).toList should equal(List(1, 2))
    Stream(1, 2).append(Stream()).toList should equal(List(1, 2))
    Stream(1, 2, 3).append(Stream(4, 5)).toList should equal(List(1, 2, 3, 4, 5))
  }

  def double[A](a: A): Stream[A] = Stream(a, a)

  "flatMap, applied to a non-empty stream," should "return a flat stream containing the results of the map operation" in {
    Stream(1, 2, 3).flatMap(double).toList should equal(List(1, 1, 2, 2, 3, 3))
  }

  "flatMap, applied to an empty stream," should "return an empty stream" in {
    Stream().flatMap(double).toList should equal(List())
  }

  // Exercise 5.8

  "constant(value)" should "return an infinite stream of the given value" in {
	Stream.constant(0).take(0).toList should equal(List())
	Stream.constant(1).take(3).toList should equal(List(1, 1, 1))
	Stream.constant("*").take(5).toList should equal(List("*", "*", "*", "*", "*"))
  }

  // Exercise 5.9

  "from(n)" should "return a stream of Ints starting from n" in {
    Stream.from(0).take(3).toList should equal(List(0, 1, 2))
    Stream.from(3).take(6).toList should equal(List(3, 4, 5, 6, 7, 8))
  }

  // Exercise 5.10

  "fibs" should "return a stream of fibonacci numbers" in {
	fibs.take(6).toList should equal(List(0, 1, 1, 2, 3, 5))
  }

  // Exercise 5.11, 5.12  TODO: figure out how to re-use the same set of tests for both take and takeViaUnold

  "takeViaUnfold(n), applied to a non-empty stream," should "return a stream containing the first n elements" in {
    Stream(1, 2, 3).takeViaUnfold(0).toList should equal (List())
    Stream(1, 2, 3).takeViaUnfold(1).toList should equal (List(1))
    Stream(1, 2, 3).takeViaUnfold(2).toList should equal (List(1, 2))
    Stream(1, 2, 3).takeViaUnfold(3).toList should equal (List(1, 2, 3))
  }

  "takeViaUnfold(n), applied to a stream with less than n elements," should "return the full stream" in {
    Stream(1).takeViaUnfold(3).toList should equal (List(1))
  }

  "takeViaUnfold, applied to an empty stream," should "return an empty stream" in {
    Stream.empty.takeViaUnfold(3).toList should equal (List())
  }

  behavior of "zipAll"

  it should "combine this and the given stream to a stream of pairs of options" in {
    (Stream(1, 2) zipAll Stream("a", "b")).toList should equal (List((Some(1), Some("a")), (Some(2), Some("b"))))
  }

  it should "generate None when one of the streams is exhausted" in {
	(Stream(1, 2) zipAll Stream(3)).toList should equal (List((Some(1), Some(3)), (Some(2), None)))
    (Stream(1) zipAll Stream(2, 3)).toList should equal (List((Some(1), Some(2)), (None, Some(3))))
	(Stream(1, 2) zipAll Empty).toList should equal (List((Some(1), None), (Some(2), None)))
    (Empty zipAll Stream(1, 2)).toList should equal (List((None, Some(1)), (None, Some(2))))
  }

  it should "return an empty stream if both streams are empty" in {
    (Empty zipAll Empty).toList should equal (List())
  }

  // Exercise 5.15

  def toList[A](s: Stream[Stream[A]]) = s.map(_.toList).toList

  "tails, applied to a non-empty stream," should "return a stream of all suffixes of the given stream" in {
	toList(Stream(1, 2, 3).tails) should equal(List(List(1, 2, 3), List(2, 3), List(3), List()))
  }

  "tails, applied to an empty stream," should "return a stream containing only an empty stream" in {
    toList(Stream().tails) should equal(List(List()))
  }

  // Exercise 5.16

  "scanRight, applied to a non-empty stream," should "return a stream of the right folds of all suffixes" in {
    Stream(1, 2, 3).scanRight(0)(_ + _).toList should equal(List(6, 5, 3, 0))
  }

  "scanRight, applied to an empty stream," should "return a stream with only the given zero value" in {
    Stream[Int]().scanRight(0)(_ + _).toList should equal(List(0))
  }
}