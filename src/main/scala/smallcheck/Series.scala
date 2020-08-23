package smallcheck

/**
 * A series generates a sequence of test values for a given depth
 */
sealed abstract class Series[A] extends Function1[Int, LazyList[A]] {

  /** make the sum (union) of this series with that series */
  def ++(that: => Series[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = Series.this.apply(d) ++ that.apply(d)
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

}

object Series {

  /** The wrap a series function as a Series object */
  def apply[A](f: Int => Seq[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = f.apply(d).to(LazyList)
  }

  /** The constant series for a sequence of values */
  def constant[A](s: Seq[A]): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = s.to(LazyList)
  }

  /** The series for a single value (nullary function) */
  def cons0[A](a: A): Series[A] = new Series[A] {
    def apply(d: Int): LazyList[A] = LazyList(a)
  }

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
  implicit lazy val seriesInt: Series[Int] = new Series[Int] {
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
  implicit lazy val seresShort: Series[Short] = new Series[Short] {
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
    cons0(None: Option[A]) ++ cons1(Some(_: A))

  /** The series of Either[A,B] for series of A and B */
  implicit def seriesEither[A, B](implicit
    sa: Series[A], sb: Series[B]
  ): Series[Either[A, B]] = cons1(Left(_: A): Either[A, B]) ++ cons1(Right(_: B))

  /** The series of List[A] for series of A */
  implicit def seriesList[A](implicit s: Series[A]): Series[List[A]] =
    cons0(List.empty[A]) ++ cons2((h: A, t: List[A]) => h :: t)(s, seriesList(s))

  /** The series of LazyList[A] for series of A */
  implicit def seriesStream[A](implicit s: Series[A]): Series[LazyList[A]] =
    cons0(LazyList.empty[A]) ++ cons2(LazyList.cons(_: A, _: LazyList[A]))(s, seriesStream(s))

  /** The series of String */
  implicit def seriesString(implicit s: Series[List[Char]]): Series[String] =
    s.map(_.mkString)

  /** The series of tuples */
  implicit def seriesTuple2[A, B](implicit
    sa: Series[A], sb: Series[B]
  ): Series[(A, B)] = sa ** sb

  /** The series of triples */
  implicit def seriesTuple3[A, B, C](implicit
    sa: Series[A], sb: Series[B], sc: Series[C]
  ): Series[(A, B, C)] =
    for (a <- sa; b <- sb; c <- sc) yield (a, b, c)

  /** The series of quadruples */
  implicit def seriesTuple4[A, B, C, D](implicit
    sa: Series[A], sb: Series[B], sc: Series[C], sd: Series[D]
  ): Series[(A, B, C, D)] =
    for (a <- sa; b <- sb; c <- sc; d <- sd) yield (a, b, c, d)
}
