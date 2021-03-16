package de.wias.nonparregboot.classifier

import breeze.optimize.{FirstOrderMinimizer, LBFGS}
import cats._
import cats.data._
import cats.implicits._
import com.github.fommil.netlib.BLAS
import de.wias.tfrandom.{Random, sampleMean}
import org.platanios.tensorflow.api.core.Shape
import scalapurerandom.{AveragebleHelper, HasSizeHelper, NEL, NatHelperTrait}
import spire.syntax.field._
import scalapurerandom.SeqReducibleInstance._
import org.log4s
import scribe.Logger
import scribe.Level
import cats.effect.{IO, _}
import org.platanios.tensorflow.api.tf

import scala.concurrent.duration.MILLISECONDS

object ClassifierApp extends IOApp with AveragebleHelper with NatHelperTrait with HasSizeHelper {
  def timeAndPrint(s: => Any)(implicit clock: Clock[IO]) = for {
    start  <- clock.monotonic(MILLISECONDS)
    _      <- IO(println(s))
    finish <- clock.monotonic(MILLISECONDS)
    _      <- IO(println(f"Completed in ${(finish - start).toFloat / 1000}s"))
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    Logger.root.clearHandlers()
                .clearModifiers()
                .withHandler(minimumLevel = Some(Level.Debug))
                .replace()

    val metric = accuracy + entropy

    val kernel: Kernel = Matern72(1f)
    val lambda = 1f

    val optimizer = tf.train.Adam(0.01f)
    val classifierTrainer = TFKRC.krc(lambda, kernel, optimizer, 1e-5f)

    val n = 123
    val testSize = 30
    val m = 4
    val result: Random[MetricValue] = for {
      (covariates, classes) <- sampleClassificationDataset(1f)(n)
      (covariatesTest, classesTest) <- sampleClassificationDataset(1f)(testSize)
      init <- standardGaussian[Float](Shape(n, m))
    } yield {
      val classifier = classifierTrainer(covariates, classes, init)
      val yhat = classifier(covariatesTest)
      metric(classesTest, yhat)
    }

    val iters = p"10"
    val avgMetricValue: Random[MetricValue] = sampleMean(result, iters)
    timeAndPrint(avgMetricValue.sample(13)).as(ExitCode.Success)
  }
}
