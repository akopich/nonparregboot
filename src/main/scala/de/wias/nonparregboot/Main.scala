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

object Main extends App {
  private val sigma2 = 0.01
  val n = 1000
  val (x, y) = SampleDataset(n, sigma2, x => sin(x * math.Pi * 2d ))()
  val learner = GPR(sigma2, Matern52(1d))(x, y)
  val yhat = learner(x)
  println(norm(yhat - y)/sqrt(n))
}
