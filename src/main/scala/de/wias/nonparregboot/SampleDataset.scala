package de.wias.nonparregboot

import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian
import cats._
import cats.data._
import cats.implicits._

object SampleDataset {
  import KRR._

  def apply(n: Int, sigma2: Double, fstar: Double => Double): DataSampler = () =>  {
    val covariates = UniformOnCubeDistribution(1).sample(n)
    val noise = DenseVector.apply(Gaussian(0, sqrt(sigma2)).sample(n).toArray)
    val f = DenseVector(covariates.map(((x: DV) => x(0)) >>> fstar).toArray)
    val responses: Responses = f + noise
    (covariates, responses, f)
  }
}
