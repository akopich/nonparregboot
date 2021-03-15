package de.wias.nonparregboot.classifier

import breeze.linalg.DenseMatrix
import org.platanios.tensorflow.api.{Graph, Session, Shape, Tensor, tf}
import scalapurerandom.{DV, toNEV}
import cats._
import cats.data._
import cats.implicits._
import org.log4s.Logger
import scribe.Logging


object TFKRC  extends Logging {

  def getLoss(K: OFloat, Y: Classes, alpha: VF, lambda: Float) = {
    import org.platanios.tensorflow.api._

    val Kalpha = tf.matmul(K, alpha) // n * m
    val expKalphak = Kalpha.exp
    val termI = - Y.toArray.zipWithIndex.map { case(yi, i) => Kalpha(i, yi) }.reduce(_ + _)
    val termII = tf.sum(expKalphak, 1).log.sum()
    val termIII = 0.5f * lambda * tf.sum(tf.multiply(alpha, Kalpha))

    termI +  termII + termIII
  }


  def krc(lambda: Float,
          kernel: Kernel,
          tol: Float): ClassifierTrainer = (X: Covariates, Y: Classes, init: Init) => {
    val K = kernel(X, X)

    val alpha = tf.variable[Float]("alpha", init.shape, initializer = tf.ConstantInitializer(init))

    val lossOp = getLoss(K, Y, alpha, lambda)
    val trainOp = tf.train.Adam(0.01f).minimize(lossOp)

    val session = Session()
    session.run(targets = tf.globalVariablesInitializer())

    val iters = LazyList.from(0)
    val losses =  iters.map { iter =>
      val lossT = session.run(targets = trainOp, fetches = lossOp)
      val loss = lossT.scalar
      logger.debug(f"iter=$iter loss=$loss")
      loss
    }

    val ((totalIters, _), loss) = iters.zip(losses).zip(losses.tail).takeWhile { case((iter, prevLoss), loss) =>
      math.abs(prevLoss - loss) / prevLoss > tol
    }.last

    logger.info(f"Fitted KRC in $totalIters iterations with loss=$loss")
    val alphaStar = session.run(fetches = alpha.value)

    (Xstar: Covariates) => {
      val Kstar = kernel(Xstar, X)
      ClassificationResults(tf.matmul(Kstar, alphaStar).evaluate)
    }
  }
}
