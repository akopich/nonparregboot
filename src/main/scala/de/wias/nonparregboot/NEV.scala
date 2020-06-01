package de.wias.nonparregboot

import cats._
import cats.data._
import cats.implicits._
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV


object NEV {

  def NEV[A](a: A): NEV[A] = NonEmptyVector(a, Vector())

  def unzip3[A, B, C](abcs: NEV[(A, B, C)]): (NEV[A], NEV[B], NEV[C]) = abcs match {
    case NonEmptyVector((a, b, c), tail) =>
      val (as, bs, cs) = tail.unzip3
      (NonEmptyVector(a, as), NonEmptyVector(b, bs), NonEmptyVector(c, cs))
  }

  def unzip[A, B](abcs: NEV[(A, B)]): (NEV[A], NEV[B]) = abcs match {
    case NonEmptyVector((a, b), tail) =>
      val (as, bs) = tail.unzip
      (NonEmptyVector(a, as), NonEmptyVector(b, bs))
  }

  def size[A](as : NEV[A]): IntP = refineV[Positive](as.size.toInt) match {
    case Right(size) => size
    case _ => throw new ArithmeticException("Size of a non-empty vector is not positive. I should have never been thrown")
  }

  case class EmptySeqToNEVException() extends Exception

  def toNEV[T](ts: Seq[T]): NEV[T] = ts match {
    case head +: tail => NonEmptyVector(head, tail.toVector)
    case _ => throw EmptySeqToNEVException()
  }

  def group[T](ts: NEV[T], size: IntP): NEV[NEV[T]] = toNEV(ts.toVector.grouped(size).map(toNEV).toSeq)
}

