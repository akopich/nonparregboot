package de.wias.nonparregboot.classifier

import breeze.linalg.DenseMatrix
import de.wias.nonparregboot.KRR.getK
import de.wias.nonparregboot.{Covariates, Kernel}
import org.platanios.tensorflow.api.{Graph, Session, Shape, Tensor, tf}
import scalapurerandom.{DV, toNEV}
import cats._
import cats.data._
import cats.implicits._


object TFKRC {

  def getLoss(K: TFloat, Y: Vector[Int], alpha: VF, lambda: Float) = {
    import org.platanios.tensorflow.api._

    val Kalpha = tf.matmul(K, alpha) // n * m
    val expKalphak = Kalpha.exp
    val termI = - Y.zipWithIndex.map { case(yi, i) => Kalpha(i, yi) }.reduce(_ + _)
    val termII = tf.sum(expKalphak, 1).log.sum()
    val termIII = 0.5f * lambda * tf.sum(tf.multiply(alpha, Kalpha))

    termI +  termII + termIII
  }


  def krc(lambda: Float,
          kernel: Kernel) = (X: Covariates[DV], Y: Classes, init: Init, Xstar: Covariates[DV]) => {


    val m = Y.maximum + 1
    val n = X.length
    val K = getK(X, X, kernel).toTensor
    val YTensor : TI = Tensor(Y.toVector).reshape(-1)

    val alpha = tf.variable[Float]("alpha", Shape(n, m), initializer = tf.RandomNormalInitializer())


    val lossOp = getLoss(K, Y.toVector, alpha, lambda)
    val trainOp = tf.train.Adam(0.01f).minimize(lossOp)


    val session = Session()
    session.run(targets = tf.globalVariablesInitializer())


    for (i <- 0 to 100) {
      val loss = session.run(targets = trainOp, fetches = lossOp)
      println(loss.summarize())
    }

    val alphaTensor = session.run(fetches = alpha.value)

    val alphaStar = DenseMatrix.zeros[Float](n, m)
    // GOVNOKOD ALERT
    for (i <- 0 until n; j <- 0 until m)
      alphaStar(i,j) = alphaTensor(i,j).scalar

    val Kstar = getK(Xstar, X, kernel).map(_.toFloat)
    val scores = (Kstar * alphaStar).map(_.toDouble)
    toNEV((0 until Xstar.length).map(i => ClassificationResult(scores(i, ::).t)))

  }
}
