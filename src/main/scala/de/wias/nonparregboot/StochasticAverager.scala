package de.wias.nonparregboot

import scala.collection.parallel.CollectionConverters._
import Averageble._
import cats.data.NonEmptyVector

object StochasticAverager {
  def apply[T: Averageble](iter: Int)(fun: => T) : T = {
    val head +: tail = (0 until iter).par.map(_ => fun).seq

    average(NonEmptyVector(head, tail.toVector))
  }
}
