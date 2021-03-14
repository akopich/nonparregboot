package de.wias.nonparregboot.classifier

import breeze.optimize.{FirstOrderMinimizer, LBFGS}
import cats._
import cats.data._
import cats.implicits._

import cats.effect.{ExitCode, IO, IOApp}
import com.github.fommil.netlib.BLAS
import de.wias.nonparregboot.Matern72
import scalapurerandom._

object ClassifierApp extends IOApp {


  def printIO(s: Any) = IO{println(s)}

  override def run(args: List[String]): IO[ExitCode] = {
    val metric = accuracy |+| entropy

    val kernel = Matern72(1)
    val lambda = 1f

    val classifierTrainer = TFKRC.krc(lambda, kernel)

    val n = p"123"
    val (covariates, classes) = sampleClassificationDataset(0.1f)(n).sample(13)
    (printIO(covariates.evaluate.summarize()) |+| printIO(classes.evaluate.summarize())).as(ExitCode.Success)

  }
}
