package de.wias.nonparregboot.classifier

import breeze.optimize.{FirstOrderMinimizer, LBFGS}
import cats._
import cats.data.StateT.fromState
import cats.data._
import cats.implicits._
import com.github.fommil.netlib.BLAS
import de.wias.tfrandom.{Random, RandomT, sampleMean}
import io.odin.{Level, Logger, consoleLogger}
import org.platanios.tensorflow.api.core.Shape
import scalapurerandom.{AveragebleHelper, HasSizeHelper, NEL, NatHelperTrait}
import spire.syntax.field._
import scalapurerandom.SeqReducibleInstance._
import cats.effect.{IO, _}
import io.odin.formatter.Formatter
import org.platanios.tensorflow.api.tf

import scala.concurrent.duration.MILLISECONDS

object ClassifierApp extends IOApp with AveragebleHelper with NatHelperTrait with HasSizeHelper {
  def timeAndPrint[F[_]: Effect](s: => F[_])(implicit clock: Clock[F]) = for {
    start  <- clock.monotonic(MILLISECONDS)
    result <- s
    _      <- println(result).pure[F]
    finish <- clock.monotonic(MILLISECONDS)
    _      <- println(f"Completed in ${(finish - start).toFloat / 1000}s").pure[F]
  } yield ()

  override def run(args: List[String]): IO[ExitCode] = {
    implicit val logger: Logger[IO] = consoleLogger(minLevel = Level.Info, formatter = Formatter.colorful)

    val metric = accuracy + entropy

    val kernel: Kernel = Matern72(1f)
    val lambda = 1f

    val optimizer = tf.train.Adam(0.01f)
    val classifierTrainer = TFKRC.krc[IO](lambda, kernel, optimizer, 1e-5f)

    val n = 1234
    val testSize = 30
    val m = 4
    val result: Random[IO[MetricValue]] = for {
      (covariates, classes) <- sampleClassificationDataset(1f)(n)
      (covariatesTest, classesTest) <- sampleClassificationDataset(1f)(testSize)
      init <- standardGaussian[Float](Shape(n, m))
    } yield {
      for {
        classifier <- classifierTrainer(covariates, classes, init)
      } yield {
        val yhat = classifier(covariatesTest)
        metric(classesTest, yhat)
      }
    }

    val iters = p"10"
    val avgMetricValue: RandomT[IO, Map[String, Double]] = sampleMean(fromState(result), iters)

    timeAndPrint(avgMetricValue.sample(13)).as(ExitCode.Success)
  }
}
