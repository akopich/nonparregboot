package de.wias.nonparregboot.classifier

import breeze.linalg.DenseMatrix
import org.platanios.tensorflow.api.{Graph, Output, Session, Shape, Tensor, tf}
import scalapurerandom.{DV, toNEV}
import cats._
import cats.data._
import cats.implicits._
import org.log4s.Logger
import org.platanios.tensorflow.api.ops.training.optimizers.Optimizer
import scribe.Logging


object TFKRC  extends Logging {

  def getLoss(K: OFloat, Ywithrange: Output[Int], alpha: VF, lambda: Float) = {
    import org.platanios.tensorflow.api._

    val Kalpha = tf.matmul(K, alpha) // n * m
    val expKalphak = Kalpha.exp
    val termI = - Kalpha.gatherND(Ywithrange).sum()
    val termII = tf.sum(expKalphak, 1).log.sum()
    val termIII = 0.5f * lambda * tf.sum(tf.multiply(alpha, Kalpha))

    termI +  termII + termIII
  }


  def krc(lambda: Float,
          kernel: Kernel,
          optimizer: Optimizer,
          tol: Float): ClassifierTrainer = (X: Covariates, Y: Classes, init: Init) => {
    val K = kernel(X, X)

    val alpha = tf.variable[Float]("alpha", init.shape, initializer = tf.ConstantInitializer(init))

    val n = alpha.shape(0)
    val index = Tensor(0 until n).reshape(-1)
    val doubleIndex = tf.stack(List[Output[Int]](index, Y), axis=1)

    val lossOp = getLoss(K, doubleIndex, alpha, lambda)
    val trainOp = optimizer.minimize(lossOp)

    val session = Session()
    session.run(targets = tf.globalVariablesInitializer())

    val iters = LazyList.from(1)
    val losses =  iters.map { iter =>
      val lossT = session.run(targets = trainOp, fetches = lossOp)
      val loss = lossT.scalar
      logger.debug(f"iter=$iter loss=$loss")
      loss
    }

    val ((totalIters, _), loss) = iters.zip(losses).zip(losses.tail).takeWhile { case((_, prevLoss), loss) =>
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
