package de.wias.nonparregboot.classifier

import breeze.numerics.log
import scalapurerandom.NEV
import cats.implicits._
import cats.kernel.Semigroup


trait Metrics {
  val accuracy: Metric = (truth: NEV[Int], yhat: NEV[ClassificationResult]) => {
    Map("accuracy" -> truth.zipWith(yhat.map(_.predictedClass))(_ == _).map(b => if (b) 1 else 0).reduce.toDouble / truth.size)
  }

  val entropy: Metric = (truth: NEV[Int], yhat: NEV[ClassificationResult]) => {
    Map("entropy" -> truth.zipWith(yhat.map(_.probabilities))((yi, probs) => log(probs(yi))).reduce)
  }

  implicit def metricSemi: Semigroup[Metric] = (x: Metric, y: Metric) =>
                                               (truth: NEV[Int], yhat: NEV[ClassificationResult]) => {
    x(truth, yhat) |+| y(truth, yhat)
  }
}
