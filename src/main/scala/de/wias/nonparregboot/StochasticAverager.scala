package de.wias.nonparregboot

import Averageble._
import Times._
import cats.data.NonEmptyVector

import scala.reflect.ClassTag

object StochasticAverager {
  def apply[T: Averageble: ClassTag](iter: IntP)(fun: => T) : T = average(iter parTimes fun)
}
