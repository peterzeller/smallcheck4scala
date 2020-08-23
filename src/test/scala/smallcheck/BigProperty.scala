package smallcheck

/**
 * Examples from the original SmallCheck
 */
object BigProperty extends Properties("List properties") {
  import Property._
  

  property("prefix-complete") =
    forAll { (xs: List[Int], ys: List[Int]) =>
      println(s"$xs -- $ys")
      xs.length + ys.length <= 9
    }


  def main(args: Array[String]): Unit = {
    val time = System.currentTimeMillis()
    Drivers.smallCheck(100, BigProperty)
    println(s"time = ${(System.currentTimeMillis() - time)/1000.0}ms")
  }
}
