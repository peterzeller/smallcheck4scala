package smallcheck


import smallcheck.Property.{Fail, Ok, Testable}

import scala.annotation.tailrec

object SmallCheck {

  def check[T](depth: Int)(prop: T)(implicit testable: Testable[T]): Unit = {
    var numTests = 0
    var numIgnored = 0
    for (d <- 0 until depth) {
      val res = testable.test(d, prop)
      numTests += res.numTests
      numIgnored += res.numIgnored
      res match {
        case ok: Ok =>
        case f: Fail =>
          throw f.cause.toException
      }
    }
    println(s"Checked $numTests inputs")
    if (numIgnored > 0)
      println(s"($numIgnored ignored by preconditions)")
  }


}
