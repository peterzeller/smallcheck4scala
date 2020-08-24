package smallcheck

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import smallcheck.SmallCheck.check
import smallcheck.Property.TestableExtensions

/**
 * Examples from the original SmallCheck
 */
class ListProperties extends AnyFunSuite with Matchers {
  import Property._

  test("rev-rev") {
    check(7) { (xs: LazyList[Int]) =>
      xs == xs.reverse.reverse
    }
  }

  def isPrefix(xs: List[Int], ys: List[Int]): Boolean = (xs, ys) match {
    case (Nil, _) => true
    case (_, Nil) => false
    case (x::xs, y::ys) => x == y || isPrefix(xs, ys)
  }

  test("prefix-complete") {
    check(5) { (xs: List[Int], ys: List[Int]) =>
      isPrefix(xs, xs ++ ys)
    }
  }

  test("prefix-sound") {
    an[SmallCheckFailedException] should be thrownBy
    check(5) { (xs: List[Int], ys: List[Int]) =>
      isPrefix(xs, ys) ==>
        exists { (zs: List[Int]) => ys == (xs ++ zs) }
    }
  }

  test("union-1") {
    an[SmallCheckFailedException] should be thrownBy
    check(5) { (xs: List[Boolean], ys: List[Boolean]) =>
      exists { (zs: List[Boolean]) =>
        forAll { (b: Boolean) =>
          zs.contains(b) == (xs.contains(b) || ys.contains(b))
        }
      }
    }
  }

  test("union-2") {
    check(5) { (xs: LazyList[Boolean], ys: LazyList[Boolean]) =>
      existsDeeperBy(_ * 2) { (zs: LazyList[Boolean]) =>
        forAll { (b: Boolean) =>
          zs.contains(b) == (xs.contains(b) || ys.contains(b))
        }
      }
    }
  }

}
