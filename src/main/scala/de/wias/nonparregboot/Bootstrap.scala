package de.wias.nonparregboot

import breeze.linalg.{DenseVector, any, max}
import breeze.stats.distributions.{Rand, RandBasis}
import cats._
import cats.data._
import cats.implicits._
import Averageble._
import Times._
import ToDV._
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

object Bootstrap {
  def predictWithBall(iters: Int, alpha: Double, el: EnsembleLearner, x: Covariates, y: Responses) = {
    el(x, y).map {predictor =>

    }
  }

  def predictWithConfidence(iters: IRP,
                            alpha: Double,
                            el: EnsembleLearner,
                            x: Covariates, y: Responses,
                            t: Covariates): (Responses, (DV, DV)) = {
    val resps = el(x, y) map (_(t))
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

  def boot(iter: IRP, resps : NonEmptyVector[Responses]) = {
    iter times sampleBootPredictors(resps)
  }

  def rand[A](as: NonEmptyVector[A])(implicit randbasis: RandBasis = Rand) =
    as.toVector(randbasis.randInt.sample() % size(as))


  def sampleBootPredictors(resp: NonEmptyVector[Responses])(implicit randbasis: RandBasis = Rand) = {
    average(size(resp) times rand(resp))
  }

  def subsample(size: Int, x: Covariates, y: Responses)(implicit randbasis: RandBasis = Rand): (Covariates, Responses) = ???
}
