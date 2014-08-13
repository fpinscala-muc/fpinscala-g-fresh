package fpinscala.errorhandling

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.Matchers
import org.scalatest.Matchers._

@RunWith(classOf[JUnitRunner])
class EitherSpec extends FlatSpec with Matchers {

  // Chapter 4, exercise 6

  def incrementByOne(int: Int): Int = int + 1

  "map(f), when applied to Right(value)," should "return Right(f(value))" in {
    Right(1) map incrementByOne should equal (Right(2))
  }

  "map(f), when applied to Left," should "return this" in {
    val error = new Exception
    val left: Either[Exception, Int] = Left(error)
    left map incrementByOne should be theSameInstanceAs left
  }

  case object ParseError extends RuntimeException

  def parseInt(string: String): Either[Exception, Int] =
    try Right(Integer.valueOf(string))
    catch {
      case e: Exception => Left(ParseError)
    }

  "flatMap(f), when applied to Right(value)," should "return f(value)" in {
    Right("42") flatMap parseInt should equal (Right(42))
    Right("fourty-two") flatMap parseInt should equal (Left(ParseError))
  }

  "flatMap(f), when applied to Left," should "return this" in {
    val left = Left(new Exception)
	left flatMap parseInt should be theSameInstanceAs left
  }

  "orElse(alternative), when applied to Right(value)," should "return this" in {
    val right = Right(42)
    right orElse Right(42) should be theSameInstanceAs right
  }

  "orElse(alternative), when applied to Left," should "the given alternative" in {
    Left(ParseError) orElse Right(42) should equal (Right(42))
  }

  def sum(a: Int, b: Int): Int = a + b

  "map2(other)(f), when applied to Right(value) with a Right argument," should "return Right(f(get, other.get))" in {
    Right(1).map2(Right(2))(sum) should equal (Right(3))
  }

  "map2(other)(f), when applied to Right(value) with a Left argument," should "the given Left" in {
    val left = Left(new Exception)
    Right(1).map2(left)(sum) should be theSameInstanceAs left
  }

  "map2(other)(f), when applied to Left with a Right argument," should "return this" in {
    val left = Left(new Exception)
    left.map2(Right(2))(sum) should be theSameInstanceAs left
  }

  "map2(other)(f), when applied to Left with a Left argument," should "return this" in {
    val left = Left(new Exception)
    left.map2(Left(new Exception))(sum) should be theSameInstanceAs left
  }
}