package de.wias.nonparregboot.classifier

import breeze.linalg.{DenseVector, softmax, sum}
import breeze.numerics.exp
import org.platanios.tensorflow.api.core.Shape
import org.platanios.tensorflow.api.tensors.Tensor
import scalapurerandom.DV

case class ClassificationResults(scores: TFloat // nstar * m
                                ) {
  def predictedClasses: Classes = scores.argmax(axes = Tensor.ones[Int](Shape())).toInt

  def probabilities: OFloat = scores.softmax(axis = 1)
}
