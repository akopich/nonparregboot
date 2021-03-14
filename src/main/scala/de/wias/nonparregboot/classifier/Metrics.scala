package de.wias.nonparregboot.classifier

import algebra.ring.AdditiveSemigroup
import breeze.plot.PaintScaleFactory.singletonFactoryForPaintScale
import scalapurerandom.{Averageble, NEV, PosInt}
import org.platanios.tensorflow.api._
import cats._
import cats.arrow.Category.ops.toAllCategoryOps
import cats.data._
import cats.implicits._

trait Metrics {
  val accuracy: Metric = (truth: Classes, yhat: ClassificationResults) => {
    Map("accuracy" -> (yhat.predictedClasses === truth).toFloat.mean().scalar)
  }

  val entropy: Metric = (truth: Classes, yhat: ClassificationResults) => {
    val logits = yhat.probabilities.log
    val onehot: Output[Float] = tf.oneHot(truth.reshape(Shape(-1)), Tensor.ones[Int](Shape()) * logits.shape(1))
    Map("negative_entropy" -> (logits * onehot).evaluate.sum().scalar)
  }

  implicit def metricSemi: AdditiveSemigroup[Metric] = (x: Metric, y: Metric) =>
    (truth: Classes, yhat: ClassificationResults) => x(truth, yhat) |+| y(truth, yhat)

  implicit def metricAverageble: Averageble[Metric] = new Averageble[Metric] {
    override val semi: AdditiveSemigroup[Metric] = metricSemi

    override def |/|(x: Metric, cnt: PosInt): Metric = (truth: Classes, yhat: ClassificationResults) => {
      x(truth, yhat) map  { case(key, value) => (key, value / cnt.toInt)}
    }
  }
}
