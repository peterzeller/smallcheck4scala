package smallcheck

import shapeless.ops.function.FnToProduct
import shapeless._, ops.function._

import scala.language.implicitConversions

trait Property {
}

object Property {

  implicit class TestableExtensions[T](elem: T)(implicit t: Testable[T]) {

    def ==>[T2](other: T2)(implicit t2: Testable[T2]): SimpleTestable =
      SimpleTestable { d =>
        t.test(d, elem) match {
          case ok: Ok =>
            t2.test(d, other)
          case fail: Fail =>
            if (fail.isException)
              fail
            else
              Ok(1, 1)
        }
      }

  }

  /**
   * A testable is a value with a function from a depth to a sequence of test cases
   */
  trait Testable[T] {
    def test(depth: Int, elem: T): TestResult
  }

  case class SimpleTestable(run: Int => TestResult)

  implicit def simple: Testable[SimpleTestable] = new Testable[SimpleTestable] {
    override def test(depth: Int, elem: SimpleTestable): TestResult =
      elem.run(depth)
  }


  /**
   * A function is testable
   */
  //  implicit def testableFunction[A, R](implicit s: Series[A], rt: Testable[R]): Testable[A => R] =
  //    new Testable[A => R] {
  //      override def test(depth: Int, f: A => R): TestResult = {
  //        forall1(f).test(depth, f)
  //      }
  //    }


  //  implicit def testableFunction2[A1, A2, R](implicit s: Series[(A1,A2)], rt: Testable[R]): Testable[(A1, A2) => R] =
  //      inverse(f => f.tupled)
  //
  //  implicit def testableFunction3[A1, A2, A3, R](implicit s: Series[(A1,A2, A3)], rt: Testable[R]): Testable[(A1, A2, A3) => R] =
  //        inverse(f => f.tupled)

  private def tryFunc[R, A](depth: Int, f: A => R, arg: A)(implicit rt: Testable[R]): TestResult = {
    try {
      rt.test(depth, f(arg))
    } catch {
      case t: Throwable =>
        Fail(1, 0, ExceptionCause(t))
    }
  }

  implicit def testableFunctionShapeless[F, A <: HList, R](implicit ftp: FnToProduct.Aux[F, A => R], s: Series[A], rt: Testable[R]): Testable[F] = {
    new Testable[F] {
      override def test(depth: Int, elem: F): TestResult = {
        runForall(depth, Forall(s, rt, ftp(elem)))
      }
    }
  }

  def inverse[A, B](f: A => B)(implicit testable: Testable[B]): Testable[A] =
    new Testable[A] {
      override def test(depth: Int, elem: A): TestResult =
        testable.test(depth, f(elem))
    }

  implicit def testableBool: Testable[Boolean] =
    new Testable[Boolean] {
      override def test(depth: Int, elem: Boolean): TestResult =
        if (elem) Ok(1, 0) else Fail(1, 0, Message("property was false"))
    }


  sealed trait TestResult {
    def numTests: Int

    def numIgnored: Int

    final def isOk: Boolean = this.isInstanceOf[Ok]

  }

  case class Ok(numTests: Int, numIgnored: Int) extends TestResult

  case class Fail(numTests: Int, numIgnored: Int, cause: Cause) extends TestResult {
    def isException: Boolean = cause.isException

  }

  sealed trait Cause {
    def isException: Boolean = this match {
      case ExceptionCause(exception) => true
      case Message(msg) => false
      case Explained(msg, root) => root.isException
    }

    def toException: Throwable = {
      def rec(msg: String, c: Cause): Throwable = {

        def plus(a: String, b: String) = {
          if (a.isEmpty) b
          else if (b.isEmpty) a
          else s"$a\n$b"
        }

        c match {
          case ExceptionCause(exception) =>
            if (msg.isEmpty) exception
            else new SmallCheckFailedException(msg, exception)
          case Message(m) =>
            new SmallCheckFailedException(plus(msg, m))
          case Explained(m, root) =>
            rec(plus(msg, m), root)
        }
      }

      rec("", this)
    }

  }

  case class SmallCheckFailedException(message: String, cause: Throwable = null) extends Exception(message, cause)



  case class ExceptionCause(exception: Throwable) extends Cause

  case class Message(msg: String) extends Cause

  case class Explained(msg: String, root: Cause) extends Cause


  case class Forall[A, R](series: Series[A], testable: Testable[R], func: A => R)

  case class Exists[A, R](series: Series[A], testable: Testable[R], func: A => R)


  def forAll[F, A <: HList, R](func: F)(implicit ftp: FnToProduct.Aux[F, A => R], series: Series[A], testable: Testable[R]): Forall[A, R] = Forall(series, testable, ftp(func))


  def forAll[A, R](series: Series[A])(func: A => R)(implicit testable: Testable[R]): Forall[A, R] = Forall(series, testable, func)

  def exists[A, R](series: Series[A])(func: A => R)(implicit testable: Testable[R]): Exists[A, R] = Exists(series, testable, func)

  def exists[F, A <: HList, R](func: F)(implicit ftp: FnToProduct.Aux[F, A => R], series: Series[A], testable: Testable[R]): Exists[A, R] = Exists(series, testable, ftp(func))

  def existsDeeperBy[F, A <: HList, R](d: Int => Int)(func: F)(implicit ftp: FnToProduct.Aux[F, A => R], series: Series[A], testable: Testable[R]): Exists[A, R] =
    Exists(series.deepen(d), testable, ftp(func))

  implicit def forallTestable[A, R]: Testable[Forall[A, R]] =
    new Testable[Forall[A, R]] {
      override def test(depth: Int, f: Forall[A, R]): TestResult =
        runForall(depth, f)
    }

  private def runForall[R, A](depth: Int, f: Forall[A, R]): TestResult = {
    var tests = 0
    var ignored = 0
    for (a <- f.series(depth)) {
      val res = tryFunc(depth, f.func, a)(f.testable)
      ignored += res.numIgnored
      tests += res.numTests
      res match {
        case _: Ok =>
        case f: Fail =>
          return Fail(tests, ignored, Explained(s"Failed for ${show(a)}", f.cause))
      }
    }
    Ok(tests, ignored)
  }

  implicit def existsTestable[A, R]: Testable[Exists[A, R]] =
    new Testable[Exists[A, R]] {
      override def test(depth: Int, f: Exists[A, R]): TestResult = {
        runExists(depth, f)
      }
    }


  private def runExists[R, A](depth: Int, f: Exists[A, R]): TestResult = {
    var tests = 0
    var ignored = 0
    for (a <- f.series(depth)) {
      val res = tryFunc(depth, f.func, a)(f.testable)
      ignored += res.numIgnored
      tests += res.numTests
      res match {
        case _: Ok =>
          return Ok(tests, ignored)
        case f: Fail =>
          if (f.isException)
            return Fail(tests, ignored, Explained(s"Failed for ${show(a)}", f.cause))
      }
    }
    Fail(tests, ignored, Message("No value exists."))
  }


  def show(x: Any): String = x match {
    case l: HList =>
      "(" + toList(l).mkString(", ") + ")"
    case _ =>
      x.toString
  }

  def toList(l: HList): List[Any] = l match {
    case c: shapeless.::[_, _] => c.head :: toList(c.tail)
    case _: HNil => List()
  }

}
