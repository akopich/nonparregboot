package de.wias.nonparregboot

import ToDV._
import NEV._

object sampleDataset {
  def apply(xGen:     () => Double,
            noiseGen: () => Double,
            fstar: Double => Double): DataSampler = (n: Pos) =>  {
    val (covariates, responses, fs) = unzip3(n.times {
      val x = xGen()
      val eps = noiseGen()
      val f = fstar(x)
      val y = f + eps
      (x.toDV, y, f)
    })

    (covariates, responses.toVector.toDV, fs.toVector.toDV)
  }
}
