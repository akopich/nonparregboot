package de.wias

import breeze.linalg.{DenseMatrix, DenseVector, all}
import cats.PartialOrder
import cats.data.NonEmptyVector
import cats._
import cats.data._
import cats.implicits._
import scalapurerandom._

package object nonparregboot {
  type Kernel = (DV, DV) => Double

  type Covariates = NEV[DV]

  type Responses = DV

  type FStarValues = DV

  type Predictor = Covariates => Responses

  type EnsemblePredictor = NEV[Predictor]

  type Learner = (Covariates, Responses) => Predictor

  type EnsembleLearner = (Covariates, Responses) => EnsemblePredictor

  type DataSampler = PosInt => Random[(Covariates, Responses, FStarValues)]

  implicit val partialOrderDV: PartialOrder[DV] =
    (x: DV, y: DV) => if (all(x <:< y)) -1d else
                      if (all(x >:> y)) 1d else 0d

  def between(l: DV, m: DV, u: DV): Boolean = l < m && m < u

  def ensemblePredict(ep: EnsemblePredictor, x: Covariates): NEV[Responses] = ep.map(_(x))

  def zip[T,U](ts: NEV[T], us: NEV[U]): NEV[(T, U)] = NonEmptyVector((ts.head, us.head), ts.tail.zip(us.tail))

  case class NonPositiveToPosException() extends Exception
}
