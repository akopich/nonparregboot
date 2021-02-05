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

  println(BLAS.getInstance().getClass.getName)


  override def run(args: List[String]): IO[ExitCode] = {
    val optimizer = new LBFGS[DV](FirstOrderMinimizer.defaultConvergenceCheck[DV](-1, 1e-3), 7)

    val n = p"1000"
    val testSize = p"1000"

    val metric = accuracy |+| entropy

    val result = (for {
      (covariates, classes)         <- sampleClassificationDataset(n)
      (covariatesTest, classesTest) <- sampleClassificationDataset(testSize)
      init                          <- gaussianInitGenerator(classes) * const(1)
    } yield {
      val optimizedClassifier = krc(0.1d, Matern72(1), optimizer, init)(covariates, classes)

      val yhat = optimizedClassifier.map(_ (covariatesTest))
      yhat.map(metric(classesTest, _))
    }).sample(getGen(13L))

    result match {
      case Right(cnt) => IO {
        println(cnt)
      }.as(ExitCode.Success)
      case Left(fail) => IO {
        println(fail)
      }.as(ExitCode(1))
    }
  }

}
