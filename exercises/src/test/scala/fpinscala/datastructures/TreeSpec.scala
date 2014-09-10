
package fpinscala.datastructures

import org.junit.runner.RunWith
import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import fpinscala.datastructures.Tree._

trait SharedTests {
  self: FlatSpec =>

  def sizeFunction(size: Tree[Any] => Int) {
    it should "return the total number of nodes in the tree" in {
      assert(size(Leaf(1)) === 1)
      assert(size(Branch(Leaf(1), Leaf(2))) === 3)
      assert(size(Branch(Branch(Leaf(1), Leaf(2)), Leaf(3))) === 5)
    }
  }

  def maximumFunction(maximum: Tree[Int] => Int) {
    it should "return the maximum value from the tree" in {
      assert(maximum(Leaf(1)) === 1)
      assert(maximum(Branch(Leaf(3), Leaf(1))) === 3)
      assert(maximum(Branch(Leaf(5), Branch(Leaf(7), Leaf(2)))) === 7)
    }
  }

  def depthFunction(depth: Tree[Any] => Int) {
    it should "return the maximum depth of the tree" in {
      assert(depth(Leaf(1)) === 0)
      assert(depth(Branch(Leaf(1), Leaf(1))) === 1)
      assert(depth(Branch(Leaf(1), Branch(Branch(Leaf(2), Leaf(3)), Leaf(4)))) === 3)
    }
  }

  // Polymorphic function arguments are (currently) no supported :-(
//  def mapFunction[A,B](map: Tree[A] => (A => B) => Tree[B]) {
//    it should "apply the function to all elements of the tree" in {
//      def double(value: Int): Int = 2 * value
//      assert(map(Leaf(1))(double) === Leaf(2))
//      assert(map(Branch(Leaf(1), Leaf(2)))(double) === Branch(Leaf(2), Leaf(4)))
//    }
//  }
}

@RunWith(classOf[JUnitRunner])
class TreeSpec extends FlatSpec with SharedTests {

  behavior of "size"

  it should behave like sizeFunction(size)

  behavior of "maximum"

  it should behave like maximumFunction(maximum)

  behavior of "depth"

  it should behave like depthFunction(depth)

  behavior of "map"

//  it should behave like mapFunction(map)
  it should "apply the function to all elements of the tree" in {
    def double(value: Int): Int = 2 * value
    assert(map(Leaf(1))(double) === Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(double) === Branch(Leaf(2), Leaf(4)))
  }

  behavior of "fold"

  it should "perform a pre-order traversal of the tree" in {
    val tree = Branch(Leaf("A"), Branch(Leaf("B"), Leaf("C")))
    assert(fold(tree)(v => v)(_ + _) === "ABC")
  }

  behavior of "sizeViaFold"

  it should behave like sizeFunction(sizeViaFold)

  behavior of "maximumViaFold"

  it should behave like maximumFunction(maximumViaFold)

  behavior of "depthViaFold"

  it should behave like depthFunction(depthViaFold)

  behavior of "mapViaFold"

//  it should behave like mapFunction(mapViaFold)
  it should "apply the function to all elements of the tree" in {
    def double(value: Int): Int = 2 * value
    assert(map(Leaf(1))(double) === Leaf(2))
    assert(map(Branch(Leaf(1), Leaf(2)))(double) === Branch(Leaf(2), Leaf(4)))
  }
}