package de.wias

import breeze.linalg.{DenseVector, all}
import cats.PartialOrder
import cats.data.NonEmptyVector
import cats._
import cats.data._
import cats.implicits._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV

package object nonparregboot {
  type IRP = Int Refined Positive

  type NEV[A] = NonEmptyVector[A]

  type DV = DenseVector[Double]

  type Kernel = (DV, DV) => Double

  type Covariates = NEV[DV]

  type Responses = DV

  type FStarValues = DV

  type Predictor = Covariates => Responses

  type EnsemblePredictor = NonEmptyVector[Predictor]

  type Learner = (Covariates, Responses) => Predictor

  type EnsembleLearner = (Covariates, Responses) => EnsemblePredictor

  type DataSampler = IRP => (Covariates, Responses, FStarValues)

  implicit val partialOrderDV: PartialOrder[DV] =
    (x: DV, y: DV) => if (all(x <:< y)) -1d else
                      if (all(x >:> y)) 1d else 0d


  def between(l: DV, m: DV, u: DV) = l < m && m < u

  def ensemblePredict(ep: EnsemblePredictor, x: Covariates) = ep.map(_(x))

  def size[A](as : NonEmptyVector[A]): IRP = refineV[Positive](as.size.toInt) match {
    case Right(size) => size
    case _ => throw new ArithmeticException("Size of a non-empty vector is not positive. I should have never been thrown")
  }

  def toNEV[T](ts: Seq[T]): NEV[T] = ts match {
    case head +: tail => NonEmptyVector(head, tail.toVector)
  }

  def group[T](ts: NEV[T], size: IRP): NEV[NEV[T]] = toNEV(ts.toVector.grouped(size.value).map(toNEV).toSeq)

  def toIRP(i: Int): IRP = refineV[Positive](i) match {
    case Right(size) => size
    case _ => throw new ArithmeticException("I should have never been thrown")
  }

  def NEV[A](a: A): NEV[A] = NonEmptyVector(a, Vector())
}
