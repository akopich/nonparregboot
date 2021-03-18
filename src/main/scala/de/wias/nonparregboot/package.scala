package de.wias

import breeze.linalg.{DenseMatrix, DenseVector, all}
import cats.PartialOrder
import cats.data.NonEmptyVector
import cats._
import cats.data._
import cats.implicits._
import scalapurerandom._
import scalapurerandom.RandomMT._

package object nonparregboot {
  type Kernel = (DV, DV) => Double

  type Covariates[In] = NEV[In]

  type Responses = DV

  type FStarValues = DV

  type Predictor[In] = Covariates[In] => Responses

  type EnsemblePredictor[In] = NEV[Predictor[In]]

  type Learner[In] = (Covariates[In], Responses) => Predictor[In]

  type EnsembleLearner[In] = (Covariates[In], Responses) => EnsemblePredictor[In]

  type DataSampler[In] = PosInt => Random[(Covariates[In], Responses, FStarValues)]

  implicit val partialOrderDV: PartialOrder[DV] =
    (x: DV, y: DV) => if (all(x <:< y)) -1d else
                      if (all(x >:> y)) 1d else 0d

  def between(l: DV, m: DV, u: DV): Boolean = l < m && m < u

  def ensemblePredict[In](ep: EnsemblePredictor[In], x: Covariates[In])
                         (implicit psf: PSFunctor[NEV]): NEV[Responses] = (ep: NEV[Predictor[In]]).pmap(_(x))

  def zip[T,U](ts: NEV[T], us: NEV[U]): NEV[(T, U)] = NonEmptyVector((ts.head, us.head), ts.tail.zip(us.tail))

  case class NonPositiveToPosException() extends Exception
}
