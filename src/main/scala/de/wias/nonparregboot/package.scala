package de.wias

import breeze.linalg.{DenseVector, all}
import cats.PartialOrder
import cats.data.NonEmptyVector
import cats._
import cats.data._
import cats.implicits._

package object nonparregboot {
  type NEV[A] = NonEmptyVector[A]

  type DV = DenseVector[Double]

  type Kernel = (DV, DV) => Double

  type Covariates = NEV[DV]

  type Responses = DV

  type FStarValues = DV

  type Predictor = Covariates => Responses

  type EnsemblePredictor = NEV[Predictor]

  type Learner = (Covariates, Responses) => Predictor

  type EnsembleLearner = (Covariates, Responses) => EnsemblePredictor

  type DataSampler = Pos => (Covariates, Responses, FStarValues)

  implicit val partialOrderDV: PartialOrder[DV] =
    (x: DV, y: DV) => if (all(x <:< y)) -1d else
                      if (all(x >:> y)) 1d else 0d

  def between(l: DV, m: DV, u: DV): Boolean = l < m && m < u

  def ensemblePredict(ep: EnsemblePredictor, x: Covariates): NEV[Responses] = ep.map(_(x))

  case class NonPositiveToPosException() extends Exception
}
