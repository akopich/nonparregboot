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

    for ((a, i) <- X.view.zipWithIndex; (b, j) <- X1.view.zipWithIndex) {
      K(i, j) = kernel(a, b)
    }

    K
  }
}


