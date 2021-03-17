package de.wias.nonparregboot.classifier

import org.platanios.tensorflow.api.{Graph, Output, Session, Shape, Tensor, tf}
import cats._
import cats.data._
import cats.implicits._
import io.odin.Logger
import org.platanios.tensorflow.api.ops.training.optimizers.Optimizer

object TFKRC {

  def getLoss(K: OFloat, Ywithrange: Output[Int], alpha: VF, lambda: Float) = {
    import org.platanios.tensorflow.api._

    val Kalpha = tf.matmul(K, alpha) // n * m
    val expKalphak = Kalpha.exp
    val termI = - Kalpha.gatherND(Ywithrange).sum()
    val termII = tf.sum(expKalphak, 1).log.sum()
    val termIII = 0.5f * lambda * tf.sum(tf.multiply(alpha, Kalpha))

    termI +  termII + termIII
  }


  def krc[F[_]: Monad](lambda: Float,
          kernel: Kernel,
          optimizer: Optimizer,
          tol: Float)(implicit logger: Logger[F]): ClassifierTrainer[F] = (X: Covariates, Y: Classes, init: Init) => {
    val K = kernel(X, X)

    val alpha = tf.variable[Float]("alpha", init.shape, initializer = tf.ConstantInitializer(init))

    val n = alpha.shape(0)
    val index = Tensor(0 until n).reshape(-1)
    val doubleIndex = tf.stack(List[Output[Int]](index, Y), axis=1)

    val lossOp = getLoss(K, doubleIndex, alpha, lambda)
    val trainOp = optimizer.minimize(lossOp)

    val session = Session()
    session.run(targets = tf.globalVariablesInitializer())

    val step = StateT[F, (Int, Float), (Int, Float, Float)] { case (iter, prev) =>
      for {
        loss <- session.run(targets = trainOp, fetches = lossOp).scalar.pure[F]
        _    <- logger.debug(f"iter=$iter loss=$loss")
      } yield ((iter+1, loss), (iter+1, prev, loss))
    }

    val chainOfSteps = step.untilM_(for {
      (iter, prev, loss) <- step
    } yield iter > 2 && math.abs(prev - loss) / prev < tol).run((0, 0f))

    for {
      ((totalIters, loss), _) <- chainOfSteps
      _ <- logger.info(f"Fitted KRC in $totalIters iterations with loss=$loss")
    } yield {
      val alphaStar = session.run(fetches = alpha.value)

      (Xstar: Covariates) => {
        val Kstar = kernel(Xstar, X)
        ClassificationResults(tf.matmul(Kstar, alphaStar).evaluate)
      }
    }
  }
}
