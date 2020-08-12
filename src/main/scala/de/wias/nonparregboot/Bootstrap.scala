package de.wias.nonparregboot

import scalapurerandom._
import breeze.linalg._
import cats.implicits._
import ToDV._
import org.apache.commons.math3.stat.descriptive.rank.Percentile

object Bootstrap {
  def predictWithBall[In](boot: NEV[Responses] => Random[NEV[Responses]],
                      alpha: Double,
                      ep: EnsemblePredictor[In],
                      t: Covariates[In]): (Responses, Random[Double]) = {
    val responses = ensemblePredict(ep, t)
    val fhat = average(responses)
    val distances: Random[NEV[Double]] = boot(responses).map(_.map(squaredDistance(_, fhat)))
    val quantile: Random[Double] = distances.map(d =>  new Percentile().evaluate(d.toVector.toArray, alpha * 100) / size(t).toInt)
    (fhat, quantile)
  }

  def predictWithConfidence[In](boot: NEV[Responses] => Random[NEV[Responses]],
                            alpha: Double,
                            ep: EnsemblePredictor[In],
                            t: Covariates[In]): (Responses, Random[(DV, DV)]) = {
    val resps = ensemblePredict(ep, t)
    val fhat = average(resps)
    val bounds: Random[(DV, DV)] = boot(resps).map { preds =>
      val predsSorted = preds.map(_.toArray).toVector.transpose.map(_.sorted)
      var i = -1
      var prob = 1d
      do {
        i += 1
        val (l, u) = getBounds(predsSorted, i)
        prob = chanceRejection(preds, l, u)
      } while(prob > alpha)
      getBounds(predsSorted, max(0, i - 1))
    }
    (fhat, bounds)
  }

  def getBounds(predsSorted: Seq[Seq[Double]], i: Int): (DV, DV) = {
    (predsSorted.map(_(i)).toDV, predsSorted.map(e => e(e.size - i - 1)).toDV)
  }

  def chanceRejection(preds: NEV[Responses], lower: DV, upper: DV): Double = {
    preds.count(between(lower, _, upper)).toDouble / preds.size
  }

  def boot(iter: PosInt, bootAvgOnce: NEV[Responses] => Random[Responses])(resps : NEV[Responses]): Random[NEV[Responses]] = {
    iter times bootAvgOnce(resps) sequence
  }

  def bootAvgOnceWithReturn(resp: NEV[Responses]): Random[Responses] = {
    intVector(size(resp)).map(indxs => average(indxs.map(resp.toVector)))
  }

  def bootAvgOnceWithWeights(resp: NEV[Responses]): Random[Responses] = {
    weightVector(size(resp)).map(weights => average(zip(weights, resp).map { case (w, v) => v * w }))
  }

  def intVector(size: PosInt): Random[NEV[Int]] = Random { gen =>
    gen(mt => size times mt.nextInt(0, size.dec.toInt))
  }

  def weightVector(size: PosInt): Random[NEV[Double]] = Random { gen =>
    gen(mt => size times mt.nextGaussian(1d, 1d))
  }
}
