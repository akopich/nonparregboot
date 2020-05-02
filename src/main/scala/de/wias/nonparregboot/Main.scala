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

object Main extends App {
  private val sigma2 = 0.01
  val n = 10
  val s = 3d
  val rho = 0.01 * math.pow(n, -2*s / (2*s + 1))
  println(rho)
  val fstar = (x: Double) => sin(x * math.Pi * 2d)
  val (x, y) = SampleDataset(n, sigma2, fstar)()
  val predictor = KRR(rho, Matern52(1d))(x, y)
  val yhat = predictor(x)
  println(norm(yhat - y)/sqrt(n))
}
