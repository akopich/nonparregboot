package de.wias.nonparregboot

import breeze.linalg.{DenseMatrix, DenseVector, cholesky}
import breeze.stats.distributions.MultivariateGaussian
import cats.Apply


object GPR {
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

  def apply(sigma2: Double, kernel: Kernel): Learner = (X: Covariates, Y: Responses) => {
    val K = getK(X, X, kernel) + sigma2*DenseMatrix.eye[Double](X.size)
    val L = cholesky(K)
    val alpha = L.t \ (L \ Y)

    (Xstar: Covariates) => {
      val kstar = getK(X, Xstar, kernel)
      val mean = kstar.t * alpha
      mean
    }
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


