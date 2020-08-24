package smallcheck

import org.scalatest.funsuite.AnyFunSuite
import Property._
import shapeless._
import ops.function._
import smallcheck.Property.TestableExtensions
import smallcheck.SmallCheck.check

/**
 * Examples from the original SmallCheck
 */
class NumberProperties extends AnyFunSuite {

  import Property._
  import Series.{seriesList}

  val primes: LazyList[Int] = {
    def sieve(s: LazyList[Int]): LazyList[Int] = s match {
      case p #:: t =>
        p #:: sieve(for (x <- t if p * p > x || x % p > 0) yield x)
    }

    sieve(LazyList.from(2))
  }

  val seriesNat = Series { (d: Int) =>
    for (i <- 0 to d) yield i
  }

  val seriesPrimes = Series { (d: Int) =>
    primes.take(d)
  }

  test("primes1") {
    check(10) {
      forAll(seriesNat) { n =>
        n > 1 ==>
          forAll(seriesPrimes) { p =>
            p % n > 0 || p == n
          }
      }
    }
  }

  test("primes2") {
    check(10) {
      forAll(seriesNat) { n =>
        n > 0 ==>
          exists(seriesList(seriesNat)) { exps =>
            (exps.isEmpty || exps.last != 0) &&
              n == ((primes, exps).zipped map (scala.math.pow(_, _))).product.toInt
          }
      }
    }
  }

}
