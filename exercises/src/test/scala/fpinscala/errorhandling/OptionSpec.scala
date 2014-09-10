
package fpinscala.errorhandling

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FlatSpec {

  // Chapter 4, exercise 1

  def toString(obj: Any): String = obj.toString

  "map(f), when applied to Some(value)," should "return Some(f(value))" in {
    assert((Some(0) map toString) === Some("0"))
  }

  "map(f), when applied to None," should "return None" in {
    assert((None map toString) === None)
  }

  behavior of "getOrElse"

  "getOrElse(default), when applied to Some(value)," should "return the option value" in {
    assert((Some(0) getOrElse 1) === 0)
  }

  "getOrElse(default), when applied to None," should "return the given default" in {
    assert((None getOrElse 1) === 1)
  }

  def f(int: Int): Option[String] = if (even(int)) Some(int.toString) else None

  "flatMap(f), when applied to Some(value)," should "return f(value)" in {
    assert((Some(0) flatMap f) === Some("0"))
    assert((Some(1) flatMap f) === None)
  }

  "flatMap(f), when applied to None," should "return None" in {
    assert((None flatMap f) === None)
  }

  "orElse(alternative), when applied to Some(value)," should "return this option" in {
    assert((Some(0) orElse Some(1)) === Some(0))
    assert((Some(0) orElse None) === Some(0))
  }

  "orElse(alternative), when applied to None," should "return the given alternative" in {
    assert((None orElse Some(1)) === Some(1))
    assert((None orElse None) === None)
  }

  def even(int: Int): Boolean = int % 2 == 0

  "filter(predicate), when applied to Some(value) which satisfies the predicate," should "return this option" in {
    assert((Some(0) filter even) === Some(0))
  }

  "filter(predicate), when applied to Some(value) which doesn't satisfy the predicate," should "return None" in {
    assert((Some(1) filter even) === None)
  }

  "filter(predicate), when applied to None," should "return None" in {
    assert((None filter (_ => true)) === None)
  }

  // Chapter 4, exercise 2

  import Option._

  behavior of "variance"

  it should "return the variance of the given sequence of numbers" in {
    assert(variance(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)) === Some(8.25))
  }

  it should "return None if applied to an empty sequence" in {
    assert(variance(List()) === None)
  }

  // Chapter 4, exercise 3

  behavior of "map2"

  it should "return None if any of the given arguments is None" in {
    assert(map2(Some(1), None)(_ + _) === None)
    assert(map2(None: Option[Int], Some(1))(_ + _) === None)
    assert(map2(None: Option[Int], None)(_ + _) === None)
  }

  it should "return Some(f(a, b)) if both arguments are Some" in {
    assert(map2(Some(1), Some(2))(_ + _) === Some(3))
  }

  // Chapter 4, exercise 4

  behavior of "sequence"

  it should "return None if the given list contains None" in {
    assert(sequence(List(None)) === None)
    assert(sequence(List(None, Some(1))) === None)
    assert(sequence(List(Some(1), None)) === None)
    assert(sequence(List(Some(1), Some(2), None)) === None)
  }

  it should "return Some containing a list of all option values" in {
    assert(sequence(List()) === Some(List()))
    assert(sequence(List(Some(1))) === Some(List(1)))
    assert(sequence(List(Some(1), Some(2))) === Some(List(1, 2)))
  }

  // Chapter 4, exercise 5

  behavior of "traverse"

  def evenInts(int: Int): Option[Int] = if (even(int)) Some(int) else None

  it should "apply the given function to the elements of the given list" in {
    assert(traverse(List(2, 4))(evenInts) === Some(List(2, 4)))
  }

  it should "return None if any of the Options produced by the given function is None" in {
    assert(traverse(List(1))(evenInts) === None)
    assert(traverse(List(2, 4, 6, 7, 8))(evenInts) === None)
  }

  it should "return Some list of all the Option values produced by the given function" in {
    assert(traverse(List(2, 4, 6, 8))(evenInts) === Some(List(2, 4, 6, 8)))
  }

  it should "return Some(Nil) if the given list is empty" in {
    assert(traverse(Nil: List[Int])(evenInts) === Some(Nil))
  }
}