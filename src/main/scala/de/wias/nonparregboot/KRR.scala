package de.wias.nonparregboot

import breeze.linalg._
import cats._
import cats.data._
import cats.implicits._
import scalapurerandom._

import ToDV._

object KRR {
  def fastKRR(P: PosInt, rho: Double, kernel: Kernel): EnsembleLearner [DV]= (x: Covariates[DV], y: Responses) => {
    val chunkSize = PosInt((x.size / P.toInt).toInt)
    val learner = krr(rho, kernel)

    val groupedResponses: NEV[Responses] = toNEV(y.toArray.grouped(chunkSize.toInt).map(_.toSeq.toDV).toSeq)
    val groupedCovariates: NEV[Covariates[DV]] = group(x, chunkSize)
    groupedCovariates.zipWith(groupedResponses)(learner)
  }

  def krr(rho: Double, kernel: Kernel): Learner[DV] = (X: Covariates[DV], Y: Responses) => (Xstar: Covariates[DV]) => {
    val K = getK(X, X, kernel) + (X.size * rho) * DenseMatrix.eye[Double](X.length)
    val L = cholesky(K)
    val alpha = L.t \ (L \ Y)

    val kstar = getK(X, Xstar, kernel)
    val mean = kstar.t * alpha
    mean
  }

  private def getK(X: Covariates[DV], X1:Covariates[DV], kernel: Kernel) = {
    val K = DenseMatrix.zeros[Double](X.length, X1.length)

    for ((a, i) <- X.toVector.view.zipWithIndex; (b, j) <- X1.toVector.view.zipWithIndex) {
      K(i, j) = kernel(a, b)
    }

    K
  }
}


