package de.wias.nonparregboot.classifier

import breeze.linalg._
import breeze.numerics.{exp, _}
import breeze.optimize.FirstOrderMinimizer.{FunctionValuesConverged, GradientConverged, ProjectedStepConverged}
import breeze.optimize.{DiffFunction, FirstOrderMinimizer, LBFGS}
import breeze.plot.{Figure, plot, scatter}
import com.github.fommil.netlib.BLAS
import de.wias.nonparregboot.KRR.getK
import de.wias.nonparregboot.{Covariates, Kernel, Matern52, Matern72}
import scalapurerandom._
import cats._
import cats.data._
import cats.implicits._
import de.wias.nonparregboot.Plot.covToDV

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
    val denominator = sum(expKalphak(::, *))  // length = n

    val termI = -yindexed.map {case(yi, i) => Kalphak(i, yi) }.sum
    val termII = sum(log(denominator))
    val termIII = 0.5 * lambda *  sum(alpha *:* Kalphak)

    val loss =  termI + termII + termIII

    val grad = DenseMatrix.zeros[Double](y.length, m)

    for ((yi, i) <- yindexed) {
      grad(::, yi) -= K(::, i)
    }

    val p = expKalphak(*, ::) / denominator.t // n * m
    grad += K * p
    grad += Kalphak * lambda

    (loss, grad)
  }

  def krc(lambda: Double,
          kernel: Kernel,
          optimizer: Optimizer,
          init: DV): ClassifierTrainer[DV] = (X: Covariates[DV], Y: Classes) => {
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
        toNEV((0 until Xstar.length).map(i => chooseClass(scores(i, ::).t)))
      })
    } else Left(OptimizationFail(optimalState))
  }

  def chooseClass(v: DenseVector[Double]) = {
    v.toScalaVector().zipWithIndex.maxBy(_._1)._2
  }

}

object ClassifierApp extends App {
  println(BLAS.getInstance().getClass.getName)
  val (covariates, classes) = sampleClassificationDataset.apply(p"1000").sample(getGen(13L))
  val (covariatesTest, classesTest) = sampleClassificationDataset.apply(p"1000").sample(getGen(12223L))


  val alpha = DenseVector[Double](breeze.stats.distributions.Gaussian(0, 1).sample(4000): _*)
  val optimizer = new LBFGS[DV](FirstOrderMinimizer.defaultConvergenceCheck[DV](-1, 1e-3), 7)
  val yhat = KernelRidgeClassifier.krc(1d, Matern72(1), optimizer, alpha)(covariates, classes).map(_(covariatesTest))
  println(yhat.map(_.toVector.zip(classesTest.toVector).count{case(a,b)=> a == b}))
}