package de.wias.nonparregboot

import breeze.linalg.DenseVector

object ToDV {
  implicit class ToDVWrapper(d: Iterable[Double]) {
    def toDV: DV = DenseVector(d.toArray)
  }
}