package de.wias.nonparregboot

import cats._
import cats.data._
import cats.implicits._
import cats.kernel.instances.IntGroup

trait Averageble[T] extends Semigroup[T] { self =>
  def |/|(x:T, cnt: Int): T

  def compose[U](other: Averageble[U]): Averageble[(T, U)] = new Averageble[(T, U)] {
    override def |/|(x: (T, U), cnt: Int): (T, U) = (self.|/|(x._1, cnt), other.|/|(x._2, cnt))

    override def combine(x: (T, U), y: (T, U)): (T, U) = (self.combine(x._1, y._1), other.combine(x._2, y._2))
  }
}

object Averageble {
  implicit class AvgWrapper[T:Averageble](value: T) {
    def |/|(cnt: Int): T = implicitly[Averageble[T]].|/|(value, cnt)
  }


  def average[T: Averageble](a: NonEmptyVector[T]) =
    a.reduce |/| a.length

  implicit def intAverageble = new IntGroup with Averageble[Int]  {
    override def |/|(x: Int, cnt: Int): Int = x / cnt
  }

  implicit def doubleAverageble = new Averageble[Double]  {
    override def |/|(x: Double, cnt: Int): Double = x / cnt

    override def combine(x: Double, y: Double): Double = x + y
  }

  implicit def DVAverageble = new Averageble[DV] {
    override def |/|(x: DV, cnt: Int): DV = x / cnt.toDouble

    override def combine(x: DV, y: DV): DV = x + y
  }

  implicit def functionAverageble[A, B: Averageble]: Averageble[A => B] = new Averageble[A => B] {
    override def |/|(x: A => B, cnt: Int): A => B = (a: A) => x(a) |/| cnt

    override def combine(x: A => B, y: A => B): A => B = (a: A) => x(a) |+| y(a)
  }
}



