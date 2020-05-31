package de.wias.nonparregboot


import java.awt.Color

import breeze.linalg._
import breeze.numerics.sin
import breeze.plot._
import breeze.stats.distributions.{Gaussian, Laplace, RandBasis, ThreadLocalRandomGenerator, Uniform}
import ToDV._
import KRR._
import de.wias.nonparregboot.Bootstrap.predictWithConfidence
import de.wias.nonparregboot.Plot.{covToDV, targets}
import eu.timepit.refined.numeric.Positive
import org.apache.commons.math3.random.MersenneTwister
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

object Plot extends App {
  def covToDV(xs: Covariates) = xs.map(_(0)).toVector.toDV

  implicit val randBasis: RandBasis = new RandBasis(new ThreadLocalRandomGenerator(new MersenneTwister(42069)))

  val xGen : () => Double     = () => {
    val g = Laplace(0.5, 0.08).sample()
    if (g >= 0d && g <= 1d) g else xGen()
  }
  val noiseGen = () => Gaussian(0d, 1d).sample()
  val fstar: Double => Double = x => sin(x * math.Pi * 2d)
  val sampler = SampleDataset(xGen, noiseGen, fstar)
  val n: IntP = refineMV[Positive](2048)
  val P: IntP = 32
  val (xs, ys, fs) = sampler(n)
  val targets : Covariates = toNEV(linspace(0d, 1d, length = 10).valuesIterator.map(_.toDV).toVector)

  val s = 3d
  val rho = 0.001 * math.pow(n.value, -2 * s / (2 * s + 1))
  val el = fastKRR(P, rho, Matern52(1d))
  val (fhat, (l, u)) : (DV, (DV, DV)) = predictWithConfidence(5000, 0.95, el, xs, ys, targets)

  val figure = Figure()
  val p = figure.subplot(0)


  p += plot(linspace(0d, 1d), linspace(0d, 1d).map(fstar), colorcode = "blue")
  p+= scatter(DenseVector(-0.05, 1.05), DenseVector(0d, 0d), _ => 0d, colors = _ => Color.RED)
  p += scatter(covToDV(targets), fhat, _ => 0.01, colors = _ => Color.RED)

  for (((t, l), u) <- covToDV(targets).toArray.zip(l.toArray).zip(u.toArray)) {
    p += plot(DenseVector(t, t), DenseVector(l, u), colorcode = "red")
  }


  p += scatter(covToDV(xs), ys, _ => 0.01, colors = _ => Color.WHITE)
  figure.saveas("/Users/avanesov/pic.pdf")
}
