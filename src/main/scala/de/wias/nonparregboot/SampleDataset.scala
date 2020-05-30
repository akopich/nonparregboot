package de.wias.nonparregboot

import breeze.linalg.DenseVector
import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian
import cats._
import cats.data._
import cats.implicits._
import ToDV._
import Times._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

object SampleDataset {
  def apply(xGen: () => Double,
            noiseGen: () => Double,
            fstar: Double => Double): DataSampler = (n: IRP) =>  {
    val (covariates, responses, fs) = unzip3(n.times {
      val x = xGen()
      val eps = noiseGen()
      val f = fstar(x)
      val y = f + eps
      (x.toDV, y, f)
    })

    (covariates, responses.toVector.toDV, fs.toVector.toDV)
  }

  def unzip3[A, B, C](abcs: NonEmptyVector[(A, B, C)]): (NEV[A], NEV[B], NEV[C]) = abcs match {
    case NonEmptyVector((a, b, c), tail) =>
      val (as, bs, cs) = tail.unzip3
      (NonEmptyVector(a, as), NonEmptyVector(b, bs), NonEmptyVector(c, cs))
  }
}
