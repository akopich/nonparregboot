package de.wias.nonparregboot

import breeze.linalg.{DenseVector, any, max}
import breeze.stats.distributions.{Rand, RandBasis}
import de.wias.nonparregboot.KRR.{Covariates, Learner, Responses}
import cats._
import cats.data._
import cats.implicits._
import Averageble._

import KRR._
import ToDV._

import scala.reflect.ClassTag

object Bootstrap {

  def predictWithConfidence(iters: Int, alpha: Double, el: EnsembleLearner, x: Covariates, y: Responses, t: Covariates) = {
    val ep = el(x, y)
    val (fhat, preds) = boot(iters, ep, t)
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

  def boot(iter: Int, ep: EnsemblePredictor, t: Covariates) = {
    val resps = ep.map(_(t))
    (average(resps), (0 until iter).map(_ => average(sampleBootPredictors(resps))) )
  }

  def sampleBootPredictors(resp: NonEmptyVector[Responses])(implicit rand: RandBasis = Rand) = {
    resp map (_ => resp.get(rand.randInt.sample() % resp.length).get)
  }

  def choose[T: ClassTag](elems: IndexedSeq[T])(indx: Array[Int]) = indx.map(_ % elems.size).map(elems)
}
