package de.wias.nonparregboot

import breeze.linalg.{DenseMatrix, DenseVector, cholesky}
import breeze.stats.distributions.MultivariateGaussian
import cats.Apply
import cats.kernel.Semigroup
import Function._

object KRR {
  type DV = DenseVector[Double]

  type Kernel = (DV, DV) => Double

  type Covariates = IndexedSeq[DV]

  type Responses = DV

  type Predictor = Covariates => Responses

  type EnsemblePredictor = Seq[Predictor]

  type Learner = (Covariates, Responses) => Predictor

  type EnsembleLearner = (Covariates, Responses) => EnsemblePredictor

  type DataSampler = () => (Covariates, Responses, DV)


  implicit val vecSemigroup = new Semigroup[DV] {
    override def combine(x: DV, y: DV): DV = x + y
  }

  implicit val applyIndSeq: Apply[IndexedSeq] = new Apply[IndexedSeq] {
    override def ap[A, B](ff: IndexedSeq[A => B])(fa: IndexedSeq[A]): IndexedSeq[B] = ff zip fa map { case(f, a) => f(a) }

    override def map[A, B](fa: IndexedSeq[A])(f: A => B): IndexedSeq[B] = fa.map(f)
  }

  def ensemblePredict(ep: EnsemblePredictor, x: Covariates) = ep.map(_(x))

  def fastKRR(P: Int, rho: Double, kernel: Kernel): EnsembleLearner = (x: Covariates, y: Responses) => {
    val chunkSize = x.size / P
    val learner = krr(rho, kernel)
    (x.grouped(chunkSize) zip y.toArray.grouped(chunkSize).map(DenseVector.apply)).map(tupled(learner)).toVector
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


