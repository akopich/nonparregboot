package de.wias.nonparregboot

import breeze.linalg.{DenseMatrix, DenseVector, any}
import breeze.stats.distributions.{ContinuousDistr, Moments, Rand, RandBasis}
import GPR._
import breeze.numerics.{log, logI}

case class UniformOnCubeDistribution(dim : Int)(implicit rand: RandBasis = Rand)  extends ContinuousDistr[DV] {
  override def unnormalizedLogPdf(x: DV): Double = logI(any(x >:> 0d) && any(x <:< 1d))

  override def logNormalizer: Double = log(1)

  override def draw(): DV = DenseVector.fill(dim)(rand.uniform.get)
}
