package fpinscala.errorhandling

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner

@RunWith(classOf[JUnitRunner])
class OptionSpec extends FlatSpec {

  behavior of "map"

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

  behavior of "flatMap"

  def f(int: Int): Option[String] = if (even(int)) Some(int.toString) else None

  "flatMap(f), when applied to Some(value)," should "return f(value)" in {
    assert((Some(0) flatMap f) === Some("0"))
    assert((Some(1) flatMap f) === None)
  }

  "flatMap(f), when applied to None," should "return None" in {
    assert((None flatMap f) === None)
  }

  behavior of "orElse"

  "orElse(alternative), when applied to Some(value)," should "return this option" in {
    assert((Some(0) orElse Some(1)) === Some(0))
    assert((Some(0) orElse None) === Some(0))
  }

  "orElse(alternative), when applied to None," should "return the given alternative" in {
    assert((None orElse Some(1)) === Some(1))
    assert((None orElse None) === None)
  }

  behavior of "filter"

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
}