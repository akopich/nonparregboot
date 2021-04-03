package de.wias.nonparregboot


import java.awt.Color

import breeze.linalg._
import breeze.numerics.{abs, sin}
import breeze.plot._
import breeze.stats.distributions.{Gaussian, Laplace, RandBasis, ThreadLocalRandomGenerator, Uniform}
import ToDV._
import KRR._
import de.wias.nonparregboot.Bootstrap.predictWithConfidence
import org.apache.commons.math3.random.MersenneTwister
import Bootstrap._

import scala.{Tuple2 => &}
import cats._
import cats.data._
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import scalapurerandom._
import ParFunctorInstances._
import ParReducibleInstance._


object Plot extends IOApp {
  def covToDV(xs: Covariates[DV]) = xs.map(_(0)).toVector.toDV


  override def run(args: List[String]): IO[ExitCode] = {
    val targetXGen = mixture(gaussian(0d, 1d), gaussian(1d, 1d))
      .iterateUntil(x => x > 0d && x < 1d)

    val xGen = uniform01
    val noiseGen = (x: Double) => gaussian(0d, 1d * Math.exp(abs(Math.pow(x - 0.5, 2d)) ) )
    val fstar: Double => Double = x => sin(x * math.Pi * 2d)
    val n = pow(p"2", p"14")
    val P = pow(p"2", p"7")
    val sampler = sampleDataset(xGen, noiseGen, fstar)(n)
    val targetSampler = sampleDataset(targetXGen, noiseGen, fstar)(p"10")

    val s = 3d
    val rho = 0.001 * math.pow(n.toInt, -2 * s / (2 * s + 1))
    val el = fastKRR(P, rho, Matern52(1d))

    val rio: RandomT[IO, Unit] = for {
      (xs, ys, _) <- sampler.transformF(_.value.pure[IO])
      (targets, _, _) <- targetSampler.transformF(_.value.pure[IO])
      (fhat, randomBounds) = predictWithConfidence(bootPar(p"1000", bootAvgOnceWithWeights), 0.95, el(xs, ys), targets)
      (u, l) <- randomBounds
    } yield {
      val figure = Figure()
      val p = figure.subplot(0)


      p += plot(linspace(0d, 1d), linspace(0d, 1d).map(fstar), colorcode = "blue")
      p += scatter(DenseVector(-0.05, 1.05), DenseVector(0d, 0d), _ => 0d, colors = _ => Color.RED)
      p += scatter(covToDV(targets), fhat, _ => 0.01, colors = _ => Color.RED)

      for (t & l & u <- covToDV(targets).toArray.zip(l.toArray).zip(u.toArray)) {
        p += plot(DenseVector(t, t), DenseVector(l, u), colorcode = "red")
      }

//      p += scatter(covToDV(xs), ys, _ => 0.01, colors = _ => Color.WHITE)
      figure.saveas(args.head)
    }
    rio.sample(getGen(103)).as(ExitCode.Success)
  }
}
