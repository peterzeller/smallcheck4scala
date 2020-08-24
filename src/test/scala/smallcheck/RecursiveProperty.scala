package smallcheck

import org.scalatest.funsuite.AnyFunSuite
import Property._
import shapeless._
import ops.function._
import shapeless.Generic.Aux
import smallcheck.Property.TestableExtensions
import smallcheck.SmallCheck.check
import Series._
import org.scalatest.matchers.should.Matchers
import shapeless.ops.coproduct
import shapeless.ops.nat.ToInt

/**
 * Examples from the original SmallCheck
 */
class RecursiveProperty extends AnyFunSuite with Matchers {

  import Property._


  sealed abstract class Tree {
    def toList: List[Int] = this match {
      case Node(left, n, right) => left.toList ++ List(n) ++ right.toList
      case Leaf() => List()
      case Blub() => List(1)
    }

    def size: Int = this match {
      case Node(left, n, right) =>
        left.size + right.size + 1
      case Leaf() => 0
      case Blub() => 0
    }

    def sum: Int = this match {
      case Node(left, n, right) =>
        left.sum + right.sum + n
      case Leaf() => 0
      case Blub() => 1
    }

  }

  case class Node(left: Tree, n: Int, right: Tree) extends Tree

  case class Leaf() extends Tree

  case class Blub() extends Tree


  test("gen tree") {
    an[SmallCheckFailedException] should be thrownBy
      SmallCheck.check(100) { (t: Tree) =>
        t.sum < 10
      }
  }

  test("gen tree 2") {
    SmallCheck.check(13) { (t: Tree) =>
      t.sum == t.toList.sum
    }
  }
}
