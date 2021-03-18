package de.wias.nonparregboot

import scalapurerandom._
import scalapurerandom.RandomMT._
import breeze.linalg._
import cats._
import cats.data._
import cats.implicits._
import ToDV._
import cats.effect.{ContextShift, IO}
import de.wias.nonparregboot.Bootstrap.intVector
import org.apache.commons.math3.stat.descriptive.rank.Percentile

object Bootstrap {
  def predictWithBall[In, F[_]: Applicative](boot: NEV[Responses] => RandomT[F, NEV[Responses]],
                                              alpha: Double,
                                              ep: EnsemblePredictor[In],
                                              t: Covariates[In])
                                            (implicit psf: PSFunctor[NEV], psr: PSReducible[NEV]): (Responses, RandomT[F, Double]) = {
    val responses = ensemblePredict(ep, t)
    val fhat = average(responses)
    val distances: RandomT[F, NEV[Double]] = boot(responses).map(_.map(squaredDistance(_, fhat)))
    val quantile: RandomT[F, Double] = distances.map(d =>  new Percentile().evaluate(d.toVector.toArray, alpha * 100) / size(t).toInt)
    (fhat, quantile)
  }

  def predictWithConfidence[In, F[_]: Applicative](boot: NEV[Responses] => RandomT[F, NEV[Responses]],
                                                    alpha: Double,
                                                    ep: EnsemblePredictor[In],
                                                    t: Covariates[In])
                                                  (implicit PSFunctor: PSFunctor[NEV], psr: PSReducible[NEV]): (Responses, RandomT[F, (DV, DV)]) = {
    val resps = ensemblePredict(ep, t)
    val fhat = average(resps)
    val bounds: RandomT[F, (DV, DV)] = boot(resps).map { preds =>
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

  def bootPar(iter: PosInt, bootAvgOnce: NEV[Responses] => Random[Responses])
             (resps : NEV[Responses])
             (implicit contextShift: ContextShift[IO]): RandomT[IO, NEV[Responses]] = {
    samplePar(bootAvgOnce(resps), iter).map( x => toNEV(x.toList))
  }

  def bootAvgOnceWithReturnWithMirroring(resp: NEV[Responses])(implicit psr: PSReducible[NEV]): Random[Responses] = {
    val fhat = average(resp)
    for {
      indxs <- intVector(size(resp))
      signs <- boolVector(size(resp))
    } yield average(indxs.zipWith(signs) { case(indx, sign) =>
      val r = resp.toVector(indx)
      if (sign) r else fhat - (r - fhat)
    })
  }

  def bootAvgOnceWithReturn(resp: NEV[Responses])(implicit psr: PSReducible[NEV]): Random[Responses] = for {
      indxs <- intVector(size(resp))
  } yield average(indxs.map(resp.toVector))

  def bootAvgOnceWithWeights(resp: NEV[Responses])(implicit psr: PSReducible[NEV]): Random[Responses] = {
    weightVector(size(resp)).map(weights => average(zip(weights, resp).map { case (w, v) => v * w }))
  }

  def intVector(size: PosInt): Random[NEV[Int]] = Random { gen =>
    gen(mt => size times mt.nextInt(0, size.dec.toInt))
  }

  def boolVector(size: PosInt): Random[NEV[Boolean]] = Random { gen =>
    gen(mt => size times mt.nextBoolean())
  }

  def weightVector(size: PosInt): Random[NEV[Double]] = Random { gen =>
    gen(mt => size times mt.nextInt(2) * 2)
  }
}
