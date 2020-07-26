package de.wias.nonparregboot

import breeze.linalg.DenseVector
import scalapurerandom._

object ToDV {
  implicit class ToDVWrapper(d: Iterable[Double]) {
    def toDV: DV = DenseVector(d.toArray)
  }

  implicit class ArrayToDVWrapper(d: Array[Double]) {
    def toDV: DV = DenseVector(d)
  }

  implicit class DoubleToDVWrapper(d: Double) {
    def toDV: DV = DenseVector(d)
  }
}