package de.wias.nonparregboot.classifier

import breeze.linalg._
import breeze.numerics.{exp, _}
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
        val alpha = new DenseMatrix[Double](n, m-1, alphavec.data, 0)
        val (loss, gradmat) = lossandgrad(y, K, alpha, lambda, m)
        val grad = gradmat.flatten()
        (loss, grad)
      }
    }

  def lossandgrad(y: Classes, K: DM,
           alpha: DM, // n * (m-1)
           lambda: Double, m: Int) = {
    val Kalphak = K * alpha // n * (m-1)
    val expKalphak: DM = exp(Kalphak)
    val yindexed = y.toVector.filter(_ + 1 < m).zipWithIndex
    val denominator = sum(expKalphak(::, *)) + 1d // length = n

    val termI = -yindexed.map {case(yi, i) => Kalphak(i, yi) }.sum
    val termII = sum(log(denominator))
    val termIII = 0.5 * lambda *  sum(alpha *:* Kalphak)

    val loss =  termI + termII + termIII

    val grad = DenseMatrix.zeros[Double](y.length, m - 1)

    for ((yi, i) <- yindexed) {
      grad(::, yi) -= K(::, i)
    }

    val p = expKalphak(*, ::) / denominator.t // n * (m-1)
    grad += K * p
    grad += Kalphak * lambda

    (loss, grad)
  }

  def krc(lambda: Double, kernel: Kernel): ClassifierTrainer[DV] = (X: Covariates[DV], Y: Classes) => {
    val m = Y.maximum + 1
    val n = X.length
    val K = getK(X, X, kernel)
    val loss = diffFunction(Y, K, lambda, m)
    val alpha = DenseVector[Double](breeze.stats.distributions.Gaussian(0, 1).sample(n * (m-1)): _*)
    val alphaStarVec = new LBFGS[DV]().minimize(loss, alpha)
    val alphaStar =  new DenseMatrix[Double](n, m-1, alphaStarVec.data, 0)
    (Xstar: Covariates[DV]) => {
      val Kstar = getK(Xstar, X, kernel)
      val scores = Kstar * alphaStar
      toNEV((0 until Xstar.length).map(i => chooseClass(scores(i, ::).t)))
    }
  }

  def chooseClass(v: DenseVector[Double]) = (v.toScalaVector() :+ 0d).zipWithIndex.maxBy(_._1)._2

}

object wtf extends App {
  println(BLAS.getInstance().getClass.getName)
  val (covariates, classes) = sampleClassificationDataset.apply(p"1000").sample(getGen(13L))
//
//  val figure = Figure()
//  val p = figure.subplot(0)
//
//  p += scatter(DenseVector(covariates.map(_(0)).toVector:_*),
//    DenseVector(covariates.map(_(1)).toVector:_*),_ => 0.01, colors = _ => Color.RED )

  val yhat = KernelRidgeClassifier.krc(100d, Matern72(1))(covariates, classes)(covariates)
  println(yhat.toVector.zip(classes.toVector).count{case(a,b)=> a==b && b<2 })
  println(classes.count(_<2))
}