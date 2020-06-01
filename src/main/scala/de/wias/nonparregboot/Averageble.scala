package de.wias.nonparregboot

import cats._
import cats.data._
import cats.implicits._
import cats.kernel.instances.IntGroup
import NEV._


trait Averageble[T] extends Semigroup[T] { self =>
  def |/|(x:T, cnt: IntP): T

  def compose[U](other: Averageble[U]): Averageble[(T, U)] = new Averageble[(T, U)] {
    override def |/|(x: (T, U), cnt: IntP): (T, U) = (self.|/|(x._1, cnt), other.|/|(x._2, cnt))

    override def combine(x: (T, U), y: (T, U)): (T, U) = (self.combine(x._1, y._1), other.combine(x._2, y._2))
  }
}

object Averageble {
  implicit class AvgWrapper[T:Averageble](value: T) {
    def |/|(cnt: IntP): T = implicitly[Averageble[T]].|/|(value, cnt)
  }

  def average[T: Averageble](a: NEV[T]): T = a.reduce |/| size(a)

  implicit def intAverageble: IntGroup with Averageble[Int] = new IntGroup with Averageble[Int]  {
    override def |/|(x: Int, cnt: IntP): Int = x / cnt
  }

  implicit def doubleAverageble: Averageble[Double] = new Averageble[Double]  {
    override def |/|(x: Double, cnt: IntP): Double = x / cnt

    override def combine(x: Double, y: Double): Double = x + y
  }

  implicit def DVAverageble: Averageble[DV] = new Averageble[DV] {
    override def |/|(x: DV, cnt: IntP): DV = x / cnt.toDouble

    override def combine(x: DV, y: DV): DV = x + y
  }

  implicit def functionAverageble[A, B: Averageble]: Averageble[A => B] = new Averageble[A => B] {
    override def |/|(x: A => B, cnt: IntP): A => B = (a: A) => x(a) |/| cnt

    override def combine(x: A => B, y: A => B): A => B = (a: A) => x(a) |+| y(a)
  }

  implicit def readerAverageble[A: Averageble, E]: Averageble[Reader[E, A]] = new Averageble[Reader[E, A]] {
    type C[B] = Reader[E, B]
    override def |/|(fa: C[A], cnt: IntP): C[A] = for {
      a <- fa
    } yield a |/| cnt

    override def combine(fx: C[A], fy: C[A]): C[A] = for {
      x <- fx
      y <- fy
    } yield x |+| y
  }
}
