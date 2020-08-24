package smallcheck

import com.sun.tools.javac.jvm.Gen
import shapeless._
import ops.function._
import shapeless.ops.hlist.Tupler
import smallcheck.LazyListUtils.{LazyListExtensions, LazyListOfLazyListExtensions}

import scala.collection.Factory
import scala.language.higherKinds
import shapeless._
import ops.hlist._

/**
 * A series generates a sequence of test values for a given depth
 */
sealed abstract class Series[A] extends Function1[Int, LazyList[A]] {

  def upCast[B >: A]: Series[B] = new Series[B] {
    override def apply(d: Int): LazyList[B] = Series.this(d)
  }

  /** make the sum (union) of this series with that series */
  def ++[B >: A](that: => Series[B]): Series[B] = new Series[B] {
    def apply(d: Int): LazyList[B] = Series.this.apply(d) ++ that.apply(d)
  }

  /** make the product of this series with that series */
  def **[B](that: Series[B]): Series[(A, B)] =
    for (a <- Series.this; b <- that) yield (a, b)

  /** deepen this series with the transformation f */
  def deepen(f: Int => Int): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = Series.this.apply(f(d))
  }

  /** map the output of this series */
  def map[B](f: A => B): Series[B] = new Series[B] {
    def apply(d: Int): LazyList[B] =
      Series.this.apply(d).map(f)
  }

  /** flatMap the output of this series */
  def flatMap[B](f: A => Series[B]): Series[B] = new Series[B] {
    def apply(d: Int): LazyList[B] =
      Series.this.apply(d).flatMap(f(_).apply(d))
  }

  /** Filter (lazily) the output of this series */
  def withFilter(p: A => Boolean): Series[A] = new Series[A] {
    override def apply(d: Int): LazyList[A] =
      Series.this.apply(d).filter(p)
  }

  /** Change the depth of this series by multiplying with the given factor f */
  def sized(f: Double): Series[A] = new Series[A] {
    override def apply(d: Int): LazyList[A] =
      Series.this.apply((d * f).toInt)
  }

  def limited(limit: Int => Int): Series[A] = new Series[A] {
    override def apply(d: Int): LazyList[A] =
      Series.this.apply(d).take(limit(d))
  }

}

object Series {

  /** The wrap a series function as a Series object */
  def apply[A](f: Int => Iterable[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = f.apply(d).to(LazyList)
  }

  /** The constant series for a sequence of values */
  def constant[A](s: Iterable[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = s.to(LazyList)
  }

  /** The constant series for a sequence of values, limited by depth */
  def constantLimited[A](s: Iterable[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = s.to(LazyList).take(d)
  }

  /** The series for a single value (nullary function) */
  def const[A](a: A): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] =
      if (d <= 0) LazyList() else LazyList(a)
  }

  /** The series for a single value (nullary function) */
  @deprecated("use Series.const instead")
  def cons0[A](a: A): Series[A] = const(a)

  /** The series for a unary function */
  def cons1[A, B](f: A => B)(implicit sa: Series[A]): Series[B] = new Series[B] {
    def apply(d: Int): LazyList[B] = if (d > 0) for (a <- sa(d - 1)) yield f(a) else LazyList()
  }

  /** The series for a binary function */
  def cons2[A, B, C](f: (A, B) => C)(implicit sa: Series[A], sb: Series[B]): Series[C] =
    new Series[C] {
      def apply(d: Int): LazyList[C] =
        if (d > 0) {
          for {
            a <- sa(d - 1)
            b <- sb(d - 1)
          } yield f(a, b)
        } else {
          LazyList()
        }
    }

  /** The series for a ternary function */
  def cons3[A, B, C, D](f: (A, B, C) => D)
    (implicit sa: Series[A], sb: Series[B], sc: Series[C]): Series[D] =
    new Series[D] {
      def apply(d: Int): LazyList[D] =
        if (d > 0) {
          for {
            a <- sa(d - 1)
            b <- sb(d - 1)
            c <- sc(d - 1)
          } yield f(a, b, c)
        } else {
          LazyList()
        }
    }

  /** The product of a series with itself */
  def double[A](implicit s: Series[A]): Series[(A, A)] = s ** s

  /** The series of Unit */
  implicit lazy val seriesUnit: Series[Unit] = new Series[Unit] {
    def apply(d: Int): LazyList[Unit] = LazyList(())
  }

  /** The series of Boolean */
  implicit lazy val seriesBool: Series[Boolean] = new Series[Boolean] {
    def apply(d: Int): LazyList[Boolean] = LazyList(true, false)
  }

  /** The series of Int */
  implicit def seriesInt: Series[Int] = new Series[Int] {
    def apply(d: Int): LazyList[Int] = LazyList(0) ++ (1 to d).flatMap(i => LazyList(i, -i))
  }

  /** The series of Long */
  implicit lazy val seriesLong: Series[Long] = seriesInt.map(_.toLong)

  /** The series of BigInt */
  implicit lazy val seriesBigInt: Series[BigInt] = seriesInt.map(BigInt(_))

  /** The series of Byte */
  implicit lazy val seriesByte: Series[Byte] = new Series[Byte] {
    def apply(d: Int): LazyList[Byte] = {
      val d2 = scala.math.min(d, Byte.MaxValue)
      (0 to d2).to(LazyList).flatMap(i => LazyList(i.toByte, (-i - 1).toByte))
    }
  }

  /** The series of Short */
  implicit lazy val seriesShort: Series[Short] = new Series[Short] {
    def apply(d: Int): LazyList[Short] = {
      val d2 = scala.math.min(d, Short.MaxValue)
      (0 to d2).to(LazyList).flatMap(i => LazyList(i.toShort, (-i - 1).toShort))
    }
  }

  /** The series of Double */
  implicit lazy val seriesDouble: Series[Double] =
    for {
      sig <- seriesInt
      exp <- seriesInt
      if sig % 2 == 1 || sig == 0 && exp == 0
    } yield sig.toDouble * scala.math.pow(2.0, exp.toDouble)

  /** The series of Float */
  implicit lazy val seriesFloat: Series[Float] = seriesDouble.map(_.toFloat)

  /** The series of Char */
  implicit lazy val serialChar: Series[Char] = new Series[Char] {
    def apply(d: Int): LazyList[Char] = (('a' to 'z') take (d + 1)).to(LazyList)
  }

  /** The series of Option[A] for series of A */
  implicit def seriesOption[A](implicit s: Series[A]): Series[Option[A]] =
    const(None: Option[A]) ++ cons1(Some(_: A))

  /** The series of Either[A,B] for series of A and B */
  implicit def seriesEither[A, B](implicit
    sa: Series[A], sb: Series[B]
  ): Series[Either[A, B]] = cons1(Left(_: A): Either[A, B]) ++ cons1(Right(_: B))

  /** The series of List[A] for series of A */
  implicit def seriesList[A](implicit s: Series[A]): Series[List[A]] =
    const(List.empty[A]) ++ cons2((h: A, t: List[A]) => h :: t)(s, seriesList(s))

  /** The series of LazyList[A] for series of A */
  implicit def seriesStream[A](implicit s: Series[A]): Series[LazyList[A]] =
    const(LazyList.empty[A]) ++ cons2((a: A, b: LazyList[A]) => a #:: b)(s, seriesStream(s))

  /** The series of List[A] for series of A */
  implicit def seriesSet[A](implicit s: Series[A]): Series[Set[A]] = {
    def genSets(d: Int, list: LazyList[A]): LazyList[Set[A]] = {
      if (d <= 0) LazyList(Set())
      else
        Set[A]() #::
          (for {
            init <- list.tails.to(LazyList)
            if init.nonEmpty
            tl <- genSets(d - 1, init.tail)
          } yield tl + init.head)
    }

    new Series[Set[A]]() {
      override def apply(d: Int): LazyList[Set[A]] = {
        genSets(d, s.apply(d))
      }
    }
  }

  /** The series of maps from K to V */
  implicit def seriesMap[K, V](implicit k: Series[K], v: Series[V]): Series[Map[K, V]] = {
    def genMaps(d: Int, keys: LazyList[K]): LazyList[Map[K, V]] = {
      if (d <= 0) LazyList(Map())
      else
        Map[K, V]() #::
          (for {
            init <- keys.tails.to(LazyList)
            if init.nonEmpty
            key = init.head
            value <- v(d - 1)
            tl <- genMaps(d - 1, init.tail)
          } yield tl + (key -> value))
    }

    new Series[Map[K, V]]() {
      override def apply(d: Int): LazyList[Map[K, V]] = {
        genMaps(d, k.apply(d))
      }
    }
  }


  /** The series of String */
  implicit def seriesString(implicit s: Series[List[Char]]): Series[String] =
    s.map(_.mkString)

  implicit val seriesHlistNil: Series[HNil] = new Series[HNil] {
    override def apply(v1: Int): LazyList[HNil] = LazyList(HNil)
  }

  implicit def seriesHlistCons[A, B <: HList](implicit a: Lazy[Series[A]], b: Series[B]): Series[A :: B] =
    for {
      x <- Series.lzy(a.value)
      xs <- b
    } yield x :: xs


  implicit val seriesCNil: Series[CNil] = new Series[CNil] {
    override def apply(v1: Int): LazyList[CNil] = LazyList()
  }

  implicit def seriesCEither[A, B <: Coproduct](implicit a: Lazy[Series[A]], b: Series[B]): Series[A :+: B] =
    Series.lzy(a.value).map(x => Inl(x)) ++ b.map(x => Inr(x))


  /** shapeless powered tuples */
  //   def seriesTuple[Tup <: Product, H <: HList](implicit tup: Tupler.Aux[H, Tup], s: Series[H]): Series[Tup] =
  //    for (x <- s) yield tup.apply(x)


  /** shapeless powered series for generic types */
  implicit def generic[A, R](implicit gen: Generic.Aux[A, R], s: Lazy[Series[R]]): Series[A] =
    for (x <- Series.lzy(s.value)) yield gen.from(x)


  //  /** The series of tuples */
  //   def seriesTuple2[A, B](implicit
  //    sa: Series[A], sb: Series[B]
  //  ): Series[(A, B)] = sa ** sb
  //
  //  /** The series of triples */
  //   def seriesTuple3[A, B, C](implicit
  //    sa: Series[A], sb: Series[B], sc: Series[C]
  //  ): Series[(A, B, C)] =
  //    for (a <- sa; b <- sb; c <- sc) yield (a, b, c)
  //
  //  /** The series of quadruples */
  //   def seriesTuple4[A, B, C, D](implicit
  //    sa: Series[A], sb: Series[B], sc: Series[C], sd: Series[D]
  //  ): Series[(A, B, C, D)] =
  //    for (a <- sa; b <- sb; c <- sc; d <- sd) yield (a, b, c, d)


  /** Goes through the different choices in breadth first order */
  def oneOfSeries[T](choices: Series[T]*): Series[T] =
    breadthFirst(constant(choices))

  /** Goes through the different choices in breadth first order */
  def oneOfSeries[T](choices: Iterable[Series[T]]): Series[T] =
    breadthFirst(constant(choices))

  /** A series selecting from a list of constants */
  def oneOf[T](choices: T*): Series[T] =
    constantLimited(choices)

  /** A series selecting from a list of constants */
  def oneOfList[T](choices: Iterable[T]): Series[T] =
    constantLimited(choices)

  def breadthFirst[T](choices: Series[Series[T]]): Series[T] = new Series[T] {
    override def apply(d: Int): LazyList[T] =
      choices(d).map(_.apply(d)).flattenBreadthFirst
  }

  def depthFirst[T](choices: Series[Series[T]]): Series[T] = new Series[T] {
    override def apply(d: Int): LazyList[T] =
      choices(d).map(_.apply(d)).flattenDepthFirst
  }

  def lzy[T](value: => Series[T]): Series[T] = new Series[T] {
    private lazy val valueCache: Series[T] = value

    override def apply(d: Int): LazyList[T] = {
      if (d <= 0) LazyList()
      else valueCache.apply(d - 1)
    }
  }


  /** Series of all combinations of the given Series */
  def sequence[T](value: List[Series[T]]): Series[List[T]] =
    new Series[List[T]] {
      override def apply(d: Int): LazyList[List[T]] =
        LazyListUtils.allCombinations(value.map(v => v(d)))
    }


  def frequency[T](values: List[(Int, Series[T])]): Series[T] = {
    val sum = values.map(_._1).sum
    require(sum > 0)
    val sorted = values.sortBy(_._1).reverse

    def walk(ar: List[(Int, LazyList[T])], pos: Int): LazyList[T] = {
      if (ar.isEmpty)
        LazyList()
      else {
        val (f, l) = ar(pos)
        val (start, rest) = l.splitAt(f)
        val (newAr, newPos1) =
          if (rest.isEmpty) {
            // remove entry
            (ar.patch(pos, List(), 1), pos)
          } else {
            (ar.updated(pos, (f, rest)), pos + 1)
          }
        val newPos = if (newPos1 >= newAr.length) 0 else newPos1
        start #::: walk(newAr, newPos)
      }

    }

    new Series[T] {
      override def apply(d: Int): LazyList[T] = {
        val withCounts: List[(Int, LazyList[T])] =
          for ((f, s) <- sorted) yield {
            val n = d * f / sum
            f -> LazyList().lazyAppendedAll {
              s(n)
            }
          }
        walk(withCounts, 0)
      }
    }
  }

//  def main(args: Array[String]): Unit = {
//    val freq = frequency(List(
//      10 -> new Series[Int] {
//        override def apply(g: Int): LazyList[Int] =
//          (1 to g).to(LazyList)
//      },
//      1 -> seriesSet[Int](seriesInt)
//    ))
//
//    for (d <- 1 to 30) {
//      println(s"## $d ######")
//      for (x <- freq(d))
//        println(s"  $x")
//    }
//  }

}
