package de.wias.nonparregboot

import cats.kernel.Semigroup
import KRR._
import breeze.linalg.DenseVector

object BreezeCats {

}

class ToDV(val d: Iterable[Double]) {
  def toDV: DV = DenseVector(d.toArray)
}

object ToDV {
  implicit def to(d: Iterable[Double]): ToDV = new ToDV(d)
}