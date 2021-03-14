package de.wias.nonparregboot.classifier


import spire.syntax.field._
import breeze.linalg.{DenseVector, diag}
import cats.data.NonEmptyVector
import cats.kernel.Monoid
import de.wias.tfrandom.{Random, RandomOutput}
import de.wias.tfrandom.TFRandom._
import org.platanios.tensorflow.api.{Shape, TF, tf}
import org.platanios.tensorflow.api.core.types.IsHalfOrFloatOrDouble
import org.platanios.tensorflow.api.tensors.Tensor
import scalapurerandom.PosInt

object sampleClassificationDataset {
  def apply(sigma: Float)(n: PosInt): Random[(Covariates, Classes)] = {
    val means = Tensor(Tensor(1f, 1f), Tensor(-1f, -1f), Tensor(1f, -1f), Tensor(-1f, 1f))

    for {
      classes <- int(4, Shape(n.toInt))
      centers = tf.gather(means, classes, axis = Tensor.zeros[Int](Shape()))
      noise   <- gaussian(0f, sigma, Shape(n.toInt, 2))
    } yield (centers + noise, classes)
  }
}
