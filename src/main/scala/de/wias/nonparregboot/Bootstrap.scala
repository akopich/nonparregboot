package de.wias.nonparregboot

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import cats._
import cats.data._
import cats.implicits._
import de.wias.random.Averageble._
import ToDV._
import org.apache.commons.math3.stat.descriptive.rank.Percentile
import de.wias.random.NEV._
import de.wias.random.Nat._
import de.wias.random.HeadTailDecomposable._
import de.wias.nonparregboot.Bootstrap.boot
import de.wias.random.Pos
import de.wias.random.RandomPure._

object Bootstrap {
  def predictWithBall(boot: NEV[Responses] => Random[NEV[Responses]],
                      alpha: Double,
                      ep: EnsemblePredictor,
                      t: Covariates): (Responses, Random[Double]) = {
    val responses = ensemblePredict(ep, t)
    val fhat = average(responses)
    val distances: Random[NEV[Double]] = boot(responses).map(_.map(squaredDistance(_, fhat)))
    val quantile: Random[Double] = distances.map(d =>  new Percentile().evaluate(d.toVector.toArray, alpha * 100) / size(t))
    (fhat, quantile)
  }

  def predictWithConfidence(boot: NEV[Responses] => Random[NEV[Responses]],
                            alpha: Double,
                            ep: EnsemblePredictor,
                            t: Covariates): (Responses, Random[(DV, DV)]) = {
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
      val (u, l) = getBounds(predsSorted, max(0, i - 1))
      (u, l)
    }
    (fhat, bounds)
  }

  def getBounds(predsSorted: Seq[Seq[Double]], i: Int): (DV, DV) = {
    (predsSorted.map(_(i)).toDV, predsSorted.map(e => e(e.size - i - 1)).toDV)
  }

  def chanceRejection(preds: NEV[Responses], lower: DV, upper: DV): Double = {
    preds.count(between(lower, _, upper)).toDouble / preds.size
  }

  def boot(iter: Pos, bootAvgOnce: NEV[Responses] => Random[Responses])(resps : NEV[Responses]): Random[NEV[Responses]] = {
    iter times bootAvgOnce(resps) sequence
  }

  def bootAvgOnceWithReturn(resp: NEV[Responses]): Random[Responses] = {
    intVector(size(resp)).map(indxs => average(indxs.map(resp.toVector)))
  }

  def bootAvgOnceWithWeights(resp: NEV[Responses]): Random[Responses] = {
    weightVector(size(resp)).map(weights => average(zip(weights, resp).map { case (w, v) => v * w }))
  }

  def intVector(size: Pos): Random[NEV[Int]] = Random { gen =>
    gen(mt => size times mt.nextInt(0, size - 1))
  }

  def weightVector(size: Pos): Random[NEV[Double]] = Random { gen =>
    gen(mt => size times {
      mt.nextGaussian() + 1d
    })
  }
}
