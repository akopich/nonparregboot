package de.wias.nonparregboot.classifier

import breeze.linalg.{DenseVector, softmax, sum}
import breeze.numerics.exp
import scalapurerandom.DV

case class ClassificationResult(scores: DV) {
  def predictedClass: Int = scores.toScalaVector().zipWithIndex.maxBy(_._1)._2

  def probs: DV = {
    val exps = exp(scores)
    exps / sum(exps)
  }
}
