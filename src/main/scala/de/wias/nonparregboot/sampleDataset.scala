package de.wias.nonparregboot

import ToDV._
import NEV._
import de.wias.random.RandomPure._
import cats._
import cats.data._
import cats.implicits._

object sampleDataset {
  def apply(xGen:     Random[Double],
            noiseGen: Random[Double],
            fstar: Double => Double): DataSampler = (n: Pos) =>  {
    val covariatesResponsesFs = n.times { for {
        x   <- xGen
        eps <- noiseGen
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
