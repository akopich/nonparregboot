package de.wias.nonparregboot


import breeze.linalg._
import breeze.stats.distributions.MultivariateGaussian
import smile.math.kernel.MercerKernel
import BreezeCats._
import breeze.numerics.sin
import cats._
import cats.implicits._
import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import KRR._
import com.github.fommil.netlib.BLAS

object Main extends App {
  println(BLAS.getInstance().getClass.getName)
  val sigma2 = 0.01
  val n = 20000
  val P = 400
  val s = 3d
  val rho = 0.001 * math.pow(n, -2 * s / (2 * s + 1))

  val ft = SuccessProbabilityEstimator(200) {
    val fstar = (x: Double) => sin(x * math.Pi * 2d)
    val (x, y, _) = SampleDataset(n, sigma2, fstar)()
    val (t, _, f) = SampleDataset(10, sigma2, fstar)()
    val el = KRR.fastKRR(P, rho, Matern52(1d))
    val (l, u) = Bootstrap.confidenceIntevals(5000, 0.95, el, x, y, t)
    between(l, f, u)
  }

  println(ft)
}
