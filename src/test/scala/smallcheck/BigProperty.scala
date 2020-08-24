package smallcheck

import org.scalatest.funsuite.AnyFunSuite
import shapeless._
import ops.function._
import Property._
import org.scalatest.matchers.should.Matchers

/**
 * Examples from the original SmallCheck
 */
class BigProperty extends AnyFunSuite with Matchers {


  test("blub") {
    val ser: Series[List[Int]] = implicitly

    for (i <- 0 to 4) {
      println(s"### size ${i} ####")
      for (x <- ser(i))
        println(s"  ${x}")
    }
  }


  test("prefix-complete") {
    an[SmallCheckFailedException] should be thrownBy
    SmallCheck.check(100) { (xs: List[Int], ys: List[Int]) =>
      println(s"$xs -- $ys ")
      xs.sum + ys.sum <= 9
    }
  }


  //  test("prefix-complete") {
  //    SmallCheck.check(100) { (xs: List[Int], ys: List[Int]) =>
  //      println(s"$xs -- $ys")
  //      xs.length + ys.length <= 9
  //    }
  //  }


}
