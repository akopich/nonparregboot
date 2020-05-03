package de.wias.nonparregboot

import breeze.linalg.{DenseVector, any, max}
import breeze.stats.distributions.{Rand, RandBasis}
import de.wias.nonparregboot.KRR.{Covariates, Learner, Responses}
import cats._
import cats.data._
import cats.implicits._

import Function._
import scala.reflect.ClassTag

case class Bootstrap[T](value: T)

object Bootstrap {
  import KRR._
  import ToDV._

  implicit val bootMonad = new Monad[Bootstrap] {
    override def flatMap[A, B](fa: Bootstrap[A])(f: A => Bootstrap[B]): Bootstrap[B] = f(fa.value)

    @annotation.tailrec
    override def tailRecM[A, B](a: A)(f: A => Bootstrap[Either[A, B]]): Bootstrap[B] = f(a) match {
      case Bootstrap(Left(nextA)) => tailRecM(nextA)(f)
      case Bootstrap(Right(b))    => new Bootstrap(b)
    }

    override def pure[A](x: A): Bootstrap[A] = new Bootstrap(x)
  }

  def confidenceIntevals(iters: Int, alpha: Double, el: EnsembleLearner, x: Covariates, y: Responses, t: Covariates) = {
    val preds = boot(iters, el, x, y, t)
    val predsSorted = preds.map(_.toArray).transpose.map(_.sorted)
    var i = -1
    var prob = 1d
    do {
      i += 1
      val (l, u) = getBounds(predsSorted, i)
      prob = chanceRejection(preds, l, u)
    } while(prob > alpha)
    getBounds(predsSorted, max(0, i - 1))
  }

  def getBounds(predsSorted: Seq[Seq[Double]], i: Int): (DV, DV) = {
    (predsSorted.map(_(i)).toDV, predsSorted.map(e => e(e.size - i - 1)).toDV)
  }

  def chanceRejection(preds: Seq[Responses], lower: DV, upper: DV) = {
    preds.count(between(lower, _, upper)).toDouble / preds.size
  }

  def boot(iter: Int, el: EnsembleLearner, x: Covariates, y: Responses, t: Covariates) = {
    val ep = el(x, y)
    val resps = ep.map(_(t))
    (0 until iter).map(_ => sampleBootPredictors(resps).reduce(_ + _) / resps.length.toDouble)
  }

  def sampleBootPredictors(resp: Seq[Responses])(implicit rand: RandBasis = Rand) = {
    val indxs: Seq[Int] = rand.randInt.sample(resp.size).map(_ % resp.size)
    indxs.map(resp)
  }

  def choose[T: ClassTag](elems: IndexedSeq[T])(indx: Array[Int]) = indx.map(_ % elems.size).map(elems)
}
