package de.wias.nonparregboot

import cats.kernel.PartialOrder
import cats._
import cats.data._
import cats.implicits._

case class ChainingCMP[T: PartialOrder](maybeVal: Option[T]) {
  def <=(other: T): ChainingCMP[T] = propagate(_ <= _, other)

  def >=(other: T): ChainingCMP[T] = propagate(_ >= _, other)

  private def propagate(f: (T, T) => Boolean, other : T): ChainingCMP[T] = maybeVal match {
    case None => ChainingCMP(None)
    case Some(value) => if (f(value, other)) ChainingCMP(other.some) else ChainingCMP(None)
  }

  def chain = this
}

object ChainingCMP {
  implicit def toCMP[T: PartialOrder](v: T): ChainingCMP[T] = ChainingCMP(v.some)

  implicit def toBool[T: PartialOrder](cmp: ChainingCMP[T]): Boolean = cmp.maybeVal.isDefined
}
