package de.wias.nonparregboot

import cats.kernel.Semigroup
import KRR._
import breeze.linalg.DenseVector

object BreezeCats {

}

class toDV(val d: Iterable[Double]) {
  def toDV: DV = DenseVector(d.toArray)
}

object toDV {
  implicit def to(d: Iterable[Double]): toDV = new toDV(d)
}