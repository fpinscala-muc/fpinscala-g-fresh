
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

  case class ParseError(input: String) extends RuntimeException

  def parseInt(string: String): Either[Exception, Int] =
    try Right(Integer.valueOf(string))
    catch {
      case e: Exception => Left(new ParseError(string))
    }

  "flatMap(f), when applied to Right(value)," should "return f(value)" in {
    Right("42") flatMap parseInt should equal (Right(42))
    Right("fourty-two") flatMap parseInt should equal (Left(ParseError("fourty-two")))
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

  // Chapter 4, exercise 7
  import Either._

  "sequence(list), when applied to a list of Right values," should "combine values into a single Right" in {
    sequence(List(Right(1), Right(2), Right(3))) should equal (Right(List(1, 2, 3)))
  }

  "sequence(list), when applied to an empty list," should "return Right containing an empty list" in {
    sequence(Nil) should equal (Right(Nil))
  }

  "sequence(list), when applied to a list containing Left," should "return the first Left value" in {
    val left = Left(new Exception)
    sequence(List(left)) should be theSameInstanceAs left
    sequence(List(Right(42), left)) should be theSameInstanceAs left
    sequence(List(left, Right(42))) should be theSameInstanceAs left
    sequence(List(left, Right(42), Left(new Exception))) should be theSameInstanceAs left
  }

  "traverse(list)(f), applied to a list of valid values," should "combine values produced by f into a single Right" in {
    traverse(List("1", "2", "3"))(parseInt) should equal (Right(List(1, 2, 3)))
  }

  "traverse(list)(f), applied to an empty list," should "return Right containing an empty list" in {
    traverse(Nil)(parseInt) should equal (Right(Nil))
  }

  "traverse(list)(f), applied to a list with invalid values," should "return the first Left value produced by f" in {
    traverse(List("a"))(parseInt) should equal (Left(ParseError("a")))
    traverse(List("1", "b", "3"))(parseInt) should equal (Left(ParseError("b")))
    traverse(List("a", "b", "3"))(parseInt) should equal (Left(ParseError("a")))
  }
}