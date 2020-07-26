package de.wias.random

import cats.data._
import de.wias.nonparregboot.NEV

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

  case class EmptySeqToNEVException() extends Exception

  def toNEV[T](ts: Seq[T]): NEV[T] = ts match {
    case head +: tail => NonEmptyVector(head, tail.toVector)
    case _ => throw EmptySeqToNEVException()
  }

  def group[T](ts: NEV[T], size: Pos): NEV[NEV[T]] = toNEV(ts.toVector.grouped(size).map(toNEV).toSeq)
}

