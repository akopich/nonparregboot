package de.wias.nonparregboot

import breeze.linalg.{DenseVector, any, max}
import breeze.stats.distributions.{Rand, RandBasis}
import cats._
import cats.data._
import cats.implicits._
import Averageble._

import ToDV._

object Bootstrap {

  def predictWithConfidence(iters: Int, alpha: Double, el: EnsembleLearner, x: Covariates, y: Responses, t: Covariates) = {
    val resps = el(x, y).map(_(t))
    println("PREDICTED!")
    val fhat = average(resps)
    val preds = boot(iters, resps)
    val predsSorted = preds.map(_.toArray).transpose.map(_.sorted)
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

  def chanceRejection(preds: Seq[Responses], lower: DV, upper: DV) = {
    preds.count(between(lower, _, upper)).toDouble / preds.size
  }

  def boot(iter: Int, resps : NonEmptyVector[Responses]) = {
    0 until iter map(_ => sampleBootPredictors(resps))
  }

  def sampleBootPredictors(resp: NonEmptyVector[Responses])(implicit rand: RandBasis = Rand) = {
    average(resp map (_ => resp.get(rand.randInt.sample() % resp.length).get))
  }
}
