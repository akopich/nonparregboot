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

  import KernelRidgeClassifier._
  println(scala.collection.parallel.availableProcessors)
  println(BLAS.getInstance().getClass.getName)

  def printIO(s: Any) = IO{println(s)}

  override def run(args: List[String]): IO[ExitCode] = {
    import SeqReducibleInstance._
    val optimizer = new LBFGS[DV](FirstOrderMinimizer.defaultConvergenceCheck[DV](-1, 1e-9), 7)

    val n = p"123"
    val testSize = p"100"

    val kernel = Matern72(1)
    val lambda = 1d

    val metric = accuracy |+| entropy

    val classifierTrainer = krc(lambda, kernel, optimizer)

    val result: Random[OptRes[MetricValue]] = for {
      (covariates, classes)         <- sampleClassificationDataset(n)
      (covariatesTest, classesTest) <- sampleClassificationDataset(testSize)
      init                          <- gaussianInitGenerator(classes) * const(1)
    } yield {
      for {
        classifier <- classifierTrainer(covariates, classes, init)
        yhat = classifier(covariatesTest)
        } yield {
        metric(classesTest, yhat)
      }
    }

    sampleMean(result, p"10").sample(getGen(13L)) match {
      case Right(cnt) => printIO(cnt).as(ExitCode.Success)
      case Left(fail) =>  printIO(fail).as(ExitCode(1))
    }
  }

}
