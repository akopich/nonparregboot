package de.wias.nonparregboot

import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian
import cats._
import cats.data._
import cats.implicits._

import ToDV._
import Times._

object SampleDataset {
  def apply(xGen: () => Double,
            noiseGen: () => Double,
            fstar: Double => Double): DataSampler = (n: Int) =>  {
    val (covariates, responses, fs) = n.times {
      val x = xGen()
      val eps = noiseGen()
      val f = fstar(x)
      val y = f + eps
      (x.toDV, y, f)
    }.unzip3

    (covariates, responses.toDV, fs.toDV)
  }
}
