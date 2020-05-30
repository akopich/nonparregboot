package de.wias.nonparregboot

import Averageble._
import Times._
import cats.data.NonEmptyVector

import scala.reflect.ClassTag

object StochasticAverager {
  def apply[T: Averageble: ClassTag](iter: Int)(fun: => T) : T = {
    val head +: tail = 0 parTimes fun

    average(NonEmptyVector(head, tail.toVector))
  }
}
