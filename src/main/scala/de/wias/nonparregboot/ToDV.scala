package de.wias.nonparregboot

import breeze.linalg.DenseVector

class ToDV(val d: Iterable[Double]) {
  def toDV: DV = DenseVector(d.toArray)
}

object ToDV {
  implicit def to(d: Iterable[Double]): ToDV = new ToDV(d)
}