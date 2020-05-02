package de.wias.nonparregboot

import breeze.linalg.{DenseMatrix, DenseVector, cholesky}
import breeze.stats.distributions.MultivariateGaussian
import cats.Apply


object KRR {
  type DV = DenseVector[Double]

  type Kernel = (DV, DV) => Double

  type Covariates = Seq[DV]

  type Responses = DV

  type Predictor = Covariates => Responses

  type Learner = (Covariates, Responses) => Predictor

  type DataSampler = () => (Covariates, Responses)


  implicit val applyIndSeq: Apply[IndexedSeq] = new Apply[IndexedSeq] {
    override def ap[A, B](ff: IndexedSeq[A => B])(fa: IndexedSeq[A]): IndexedSeq[B] = ff.zip(fa).map { case(f, a) => f(a) }

    override def map[A, B](fa: IndexedSeq[A])(f: A => B): IndexedSeq[B] = fa.map(f)
  }

  def apply(rho: Double, kernel: Kernel): Learner = (X: Covariates, Y: Responses) => (Xstar: Covariates) => {
    val K = getK(X, X, kernel) + (X.size * rho) * DenseMatrix.eye[Double](X.size)
    val L = cholesky(K)
    val alpha = L.t \ (L \ Y)

    val kstar = getK(X, Xstar, kernel)
    val mean = kstar.t * alpha
    mean
  }

  private def getK(X: Covariates, X1:Covariates, kernel: Kernel) = {
    val K = DenseMatrix.zeros[Double](X.size, X1.size)

    for ((a, i) <- X.zipWithIndex; (b, j) <- X.zipWithIndex) {
      val k = kernel(a, b)
      K(i, j) += k
    }

    K
  }
}


