package de.wias.nonparregboot

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import cats._
import cats.data._
import cats.implicits._
import Averageble._
import Times._
import ToDV._
import org.apache.commons.math3.stat.descriptive.rank.Percentile
import NEV._

object Bootstrap {
  def predictWithBall(iters: IntP,
                      alpha: Double,
                      ep: EnsemblePredictor,
                      t: Covariates): (Responses, Double) = {
    val responses = ensemblePredict(ep, t)
    val fhat = average(responses)
    val distances = boot(iters, responses).map(squaredDistance(_, fhat))
    val quantile = new Percentile().evaluate(distances.toVector.toArray, alpha * 100)
    (fhat, quantile / size(t))
  }

  def predictWithConfidence(iters: IntP,
                            alpha: Double,
                            ep: EnsemblePredictor,
                            t: Covariates): (Responses, (DV, DV)) = {
    val resps = ensemblePredict(ep, t)
    val fhat = average(resps)
    val preds = boot(iters, resps)
    val predsSorted = preds.map(_.toArray).toVector.transpose.map(_.sorted)
    var i = -1
    var prob = 1d
    do {
      i += 1
      val (l, u) = getBounds(predsSorted, i)
      prob = chanceRejection(preds, l, u)
    } while(prob > alpha)
    val (u, l) = getBounds(predsSorted, max(0, i - 1))
    (fhat, (u, l))
  }

  def getBounds(predsSorted: Seq[Seq[Double]], i: Int): (DV, DV) = {
    (predsSorted.map(_(i)).toDV, predsSorted.map(e => e(e.size - i - 1)).toDV)
  }

  def chanceRejection(preds: NEV[Responses], lower: DV, upper: DV) = {
    preds.count(between(lower, _, upper)).toDouble / preds.size
  }

  def boot(iter: IntP, resps : NEV[Responses]) = {
    iter times sampleBootPredictors(resps)
  }

  def rand[A](as: NEV[A]): A = as.toVector(rand(size(as)))

  def rand(i: Int)(implicit randbasis: RandBasis = Rand): Int = randbasis.randInt.sample() % i

  def sampleBootPredictors(resp: NEV[Responses]): Responses = average(size(resp) times rand(resp))

  def subsample(length: IntP, x: Covariates) : Covariates = length times rand(x)
}
