package de.wias.nonparregboot

import java.time.format.SignStyle

import breeze.linalg.{DenseMatrix, DenseVector, all, cholesky}
import breeze.stats.distributions.MultivariateGaussian
import cats._
import cats.data._
import cats.implicits._

import Function._
import ToDV._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import NEV._


object KRR {
  def fastKRR(P: Int, rho: Double, kernel: Kernel): EnsembleLearner = (x: Covariates, y: Responses) => {
    val chunkSize = toIRP((x.size / P).toInt)
    val learner = krr(rho, kernel)

    val groupedResponses: NEV[Responses] = toNEV(y.toArray.grouped(chunkSize).map(_.toSeq.toDV).toSeq)
    val groupedCovariates: NEV[Covariates] = group(x, chunkSize)
    groupedCovariates.zipWith(groupedResponses)(learner)
  }

  def krr(rho: Double, kernel: Kernel): Learner = (X: Covariates, Y: Responses) => (Xstar: Covariates) => {
    val K = getK(X, X, kernel) + (X.size * rho) * DenseMatrix.eye[Double](size(X))
    val L = cholesky(K)
    val alpha = L.t \ (L \ Y)

    val kstar = getK(X, Xstar, kernel)
    val mean = kstar.t * alpha
    mean
  }

  private def getK(X: Covariates, X1:Covariates, kernel: Kernel) = {
    val K = DenseMatrix.zeros[Double](size(X), size(X1))

    for ((a, i) <- X.toVector.view.zipWithIndex; (b, j) <- X1.toVector.view.zipWithIndex) {
      K(i, j) = kernel(a, b)
    }

    K
  }
}


