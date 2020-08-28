package de.wias.nonparregboot

import ToDV._
import cats._
import cats.data._
import cats.implicits._
import scalapurerandom._

object sampleDataset {
  def apply(xGen:     Random[Double],
            noiseGen: Double => Random[Double],
            fstar:    Double => Double): DataSampler[DV] = (n: PosInt) =>  {
    val covariatesResponsesFs = n.times { for {
        x   <- xGen
        eps <- noiseGen(x)
      } yield {
        val f = fstar(x)
        val y = f + eps
        (x.toDV, y, f)
      }
    }.sequence

    for {
      list <- covariatesResponsesFs
    } yield {
      val (covariates, responses, fs) = unzip3(list)
      (covariates, responses.toVector.toDV, fs.toVector.toDV)
    }
  }
}
