package de.wias.nonparregboot

import java.time.format.SignStyle

import breeze.linalg.{DenseMatrix, DenseVector, all, cholesky}
import breeze.stats.distributions.MultivariateGaussian
import cats._
import cats.data._
import cats.implicits._

import Function._
import ToDV._

object KRR {
  type DV = DenseVector[Double]

  type Kernel = (DV, DV) => Double

  type Covariates = IndexedSeq[DV]

  type Responses = DV

  type Predictor = Covariates => Responses

  type EnsemblePredictor = NonEmptyVector[Predictor]

  type Learner = (Covariates, Responses) => Predictor

  type EnsembleLearner = (Covariates, Responses) => EnsemblePredictor

  type DataSampler = (Int) => (Covariates, Responses, DV)


  implicit val partialOrderDV: PartialOrder[DV] = (x: DV, y: DV) => if (all(x <:< y)) -1d else
                                                                      if (all(x >:> y)) 1d else 0d

  def between(l: DV, m: DV, u: DV) = l < m && m < u

  def ensemblePredict(ep: EnsemblePredictor, x: Covariates) = ep.map(_(x))

  def fastKRR(P: Int, rho: Double, kernel: Kernel): EnsembleLearner = (x: Covariates, y: Responses) => {
    val chunkSize = x.size / P
    val learner = krr(rho, kernel)
    val head +: tail = (x.grouped(chunkSize) zip y.toArray.grouped(chunkSize).map(_.toSeq.toDV)).map(tupled(learner)).toVector
    NonEmptyVector(head, tail)
  }

  def krr(rho: Double, kernel: Kernel): Learner = (X: Covariates, Y: Responses) => (Xstar: Covariates) => {
    val K = getK(X, X, kernel) + (X.size * rho) * DenseMatrix.eye[Double](X.size)
    val L = cholesky(K)
    val alpha = L.t \ (L \ Y)

    val kstar = getK(X, Xstar, kernel)
    val mean = kstar.t * alpha
    mean
  }

  private def getK(X: Covariates, X1:Covariates, kernel: Kernel) = {
    val K = DenseMatrix.zeros[Double](X.size, X1.size)

    for ((a, i) <- X.zipWithIndex; (b, j) <- X1.zipWithIndex) {
      val k = kernel(a, b)
      K(i, j) += k
    }

    K
  }
}


