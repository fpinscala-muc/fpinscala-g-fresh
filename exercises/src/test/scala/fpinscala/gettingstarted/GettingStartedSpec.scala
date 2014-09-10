
package fpinscala.gettingstarted

import scala.math.Ordering.Implicits._

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

import MyModule._
import PolymorphicFunctions._

@RunWith(classOf[JUnitRunner])
class GettingStartedSpec extends FlatSpec {

  behavior of "fib"

  it should "calculate the nth fibonacci number" in {
    val fibonacciNumbers = for (n <- 1 to 10) yield fib(n)
    assert(fibonacciNumbers === List(1, 1, 2, 3, 5, 8, 13, 21, 34, 55))
  }

  behavior of "isSorted"

  def gt(a: Int, b: Int) = a > b
  def lexicallyGreater(a: String, b: String) = a > b

  it should "return true for an empty array" in {
    assert(isSorted(Array[Int](), gt))
  }

  it should "return true for an array with only one element" in {
    assert(isSorted(Array("Arthur"), lexicallyGreater))
  }

  it should "return true for an array which is already sorted" in {
    assert(isSorted(Array(1, 2, 3), gt))
  }

  it should "return false for an unsorted array" in {
    assert(! isSorted(Array(1, 3, 2), gt))
  }

  behavior of "partial1"

  def sum(a: Int, b: Int) = a + b

  it should "return a partially applied function using the given argument" in {
    val f = partial1(1, sum)
    assert(f(1) === 2)
    assert(f(2) === 3)
    assert(f(4) === 5)
  }

  behavior of "curry"

  it should "return a unary function that partially applies the given binary function" in {
    val f = curry(sum)
    assert(f(1)(1) === 2)
    assert(f(1)(2) === 3)
    assert(f(2)(4) === 6)
  }

  behavior of "uncurry"

  it should "return a binary function from the curried version of the function" in {
    val curriedSum = sum _ curried
	val f = uncurry(curriedSum)
	assert(f(1, 2) === 3)
  }

  behavior of "compose"

  def multiply(a: Int, b: Int) = a * b

  it should "return a function which 'chains' the two given functions: compose(f, g) := g(f())" in {
    val plusOne = sum(_: Int, 1)
    val timesTwo = multiply(_: Int, 2)
	val f = compose(plusOne, timesTwo)
	assert(f(1) === 3)
	assert(f(2) === 5)
	assert(f(3) === 7)
	val g = compose(timesTwo, plusOne)
	assert(g(1) === 4)
	assert(g(2) === 6)
	assert(g(3) === 8)
  }
}