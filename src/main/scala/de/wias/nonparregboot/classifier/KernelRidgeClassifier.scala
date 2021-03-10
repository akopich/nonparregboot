package de.wias.nonparregboot.classifier

import breeze.linalg._
import breeze.numerics.{exp, _}
import breeze.optimize.FirstOrderMinimizer.{FunctionValuesConverged, GradientConverged, ProjectedStepConverged}
import breeze.optimize.{DiffFunction, FirstOrderMinimizer, LBFGS}
import breeze.plot.{Figure, plot, scatter}
import com.github.fommil.netlib.BLAS
import de.wias.nonparregboot.KRR.getK
import de.wias.nonparregboot.{Covariates, Kernel, Matern52, Matern72, Responses}
import scalapurerandom._
import cats._
import cats.data._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import spire.syntax.field._
import de.wias.nonparregboot.ToDV._
import Function._

import java.awt.Color


object KernelRidgeClassifier {
  def diffFunction(y: Classes, K: DM, lambda: Double, m: Int) = new DiffFunction[DV] {
      override def calculate(alphavec: DV): (Double, DV) = {
        val n = y.length
        val alpha = new DenseMatrix[Double](n, m, alphavec.data, 0)
        val (loss, gradmat) = lossandgrad(y, K, alpha, lambda, m)
        val grad = gradmat.flatten()
        (loss, grad)
      }
    }

  def lossandgrad(y: Classes, K: DM,
           alpha: DM, // n * m
           lambda: Double, m: Int) = {
    val Kalphak = K * alpha // n * m
    val expKalphak: DM = exp(Kalphak)
    val yindexed = y.toVector.filter(_ < m).zipWithIndex
    val denominator = sum(expKalphak(*, ::))  // length = n

    val termI = -yindexed.map { case(yi, i) => Kalphak(i, yi) }.sum
    val termII = sum(log(denominator))
    val termIII = 0.5 * lambda *  sum(alpha *:* Kalphak)

    val loss =  termI + termII + termIII

    val grad = DenseMatrix.zeros[Double](y.length, m)

    for ((yi, i) <- yindexed) {
      grad(::, yi) -= K(::, i)
    }

    val p = expKalphak(::, *) / denominator // n * m
    grad += K * p
    grad += Kalphak * lambda

    (loss, grad)
  }

  def krc(lambda: Double,
          kernel: Kernel,
          optimizer: Optimizer): ClassifierTrainer[DV] = (X: Covariates[DV], Y: Classes, init: Init) => {
    val m = Y.maximum + 1
    val n = X.length
    val K = getK(X, X, kernel)
    val loss = diffFunction(Y, K, lambda, m)
    val optimalState = optimizer.minimizeAndReturnState(loss, init)
    val isConvergenceAchieved = optimalState.convergenceReason.exists {
      case FunctionValuesConverged | GradientConverged | ProjectedStepConverged => true
      case _ => false
    }
    if (isConvergenceAchieved) {
      val alphaStar = new DenseMatrix[Double](n, m, optimalState.x.data, 0)
      Right((Xstar: Covariates[DV]) => {
        val Kstar = getK(Xstar, X, kernel)
        val scores = Kstar * alphaStar
        toNEV((0 until Xstar.length).map(i => ClassificationResult(scores(i, ::).t)))
      })
    } else Left(OptimizationFail(optimalState))
  }

  def fastKRC(P: PosInt, trainer: ClassifierTrainer[DV])(implicit psf: PSFunctor[NEV]) =
    (X: Covariates[DV], y: Classes, init: Init) => {
      val chunkSize = PosInt((X.size / P.toInt).toInt)
      val groupedResponses: NEV[Classes] = group(y, chunkSize)
      val groupedCovariates: NEV[Covariates[DV]] = group(X, chunkSize)
      val groupedInits: NEV[Init] = group(toNEV(init.toScalaVector()), PosInt(init.size / P.toInt)).map(_.toVector.toDV)
      val grouped: NEV[(Covariates[DV], Classes, Init)] = groupedCovariates.zipWith(groupedResponses) { (x, y) => (x, y) }
                                                                     .zipWith(groupedInits){ case((x1,x2), y) => (x1, x2, y) }

      (grouped pmap tupled(trainer)).sequence
  }

  def gaussianInitGenerator(Y: Classes): Random[DV] = {
    val m = Y.maximum + 1
    val n = Y.length

    standardGaussian(m * n)
  }
}

