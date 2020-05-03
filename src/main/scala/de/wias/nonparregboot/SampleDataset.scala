package de.wias.nonparregboot

import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian
import cats._
import cats.data._
import cats.implicits._

object SampleDataset {
  import KRR._
  import toDV._

  def apply(n: Int, sigma2: Double, fstar: Double => Double): DataSampler = () =>  {
    val covariates = UniformOnCubeDistribution(1).sample(n)
    val noise = Gaussian(0, sqrt(sigma2)).sample(n).toDV
    val f = covariates.map(((x: DV) => x(0)) >>> fstar).toDV
    val responses: Responses = f + noise
    (covariates, responses, f)
  }
}
