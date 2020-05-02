package de.wias.nonparregboot

import scala.collection.parallel.CollectionConverters._

object SuccessProbabilityEstimator {
  def apply(iter: Int)(fun: => Boolean ) : Double = {
    (0 until iter).par.map(_ => fun).count(x => x).toDouble / iter
  }
}
