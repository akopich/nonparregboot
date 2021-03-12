package de.wias.nonparregboot.classifier

import breeze.linalg.{DenseMatrix, DenseVector, Transpose}
import scalapurerandom.{DM, DV}
import org.platanios.tensorflow.api.{Tensor => TFTensor}
import org.platanios.tensorflow.api.Shape


class WrappedDV[T](private val v: DV) {
  def toTensor = TFTensor(v.toScalaVector().map(_.toFloat)).transpose()
}

class WrappedTDV(private val v: Transpose[DV]) {
  def toTensor = new WrappedDV(v.t).toTensor.reshape(Shape(-1))
}

class WrappedDM(private val m: DM) {
  def toTensor = {
    val rows = (0 until m.rows).map(i => new WrappedTDV(m(i, ::)).toTensor)
    TFTensor(rows :_*)
  }
}

trait TensorflowBreezeConverters {
  implicit def wrap(v: DV) = new WrappedDV(v)

  implicit def wrap(v: Transpose[DV]) = new WrappedTDV(v)

  implicit def wrap(m: DM) = new WrappedDM(m)
}

