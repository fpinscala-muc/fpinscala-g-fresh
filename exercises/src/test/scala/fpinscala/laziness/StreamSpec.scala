package fpinscala.laziness

import org.scalatest.Matchers
import org.scalatest.FlatSpec
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class StreamSpec extends FlatSpec with Matchers {

  // Chapter 5, exercise 1

  "toList, applied to a non-empty stream," should "return a list containing the stream's elements" in {
    Stream(1).toList should equal (List(1))
    Stream(1, 2, 3).toList should equal (List(1, 2, 3))
  }

  "toList, applied to an empty stream," should "return an empty list" in {
    Stream().toList should equal (List())
  }

  // Chapter 5, exercise 2

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

  // Chapter 5, exercise 3

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

  // Chapter 5, exercise 4

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

  it should "true" in {
    Stream.empty[Int].forAll(even) should equal (true)
  }

  // Chapter 5, exercise 5


  // Chapter 5, exercise 6

  "headOption, applied to a non-empty stream," should "return Some of the first element" in {
    Stream(1, 2, 3).headOption should equal(Some(1))
  }

  "headOption, applied to an empty stream," should "return None" in {
    Stream().headOption should equal(None)
  }

  // Chapter 5, exercise 7

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

  "flatMap, applied to a non-empty stream," should "return a flat list containing the results of the map operation" in {
    Stream(1, 2, 3).flatMap(double).toList should equal(List(1, 1, 2, 2, 3, 3))
  }

  "flatMap, applied to an empty stream," should "return an empty stream" in {
    Stream().flatMap(double).toList should equal(List())
  }
}