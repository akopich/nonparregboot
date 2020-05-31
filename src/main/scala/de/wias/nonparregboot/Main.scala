package de.wias.nonparregboot

import breeze.numerics.sin
import cats._
import cats.implicits._
import cats.effect._
import com.github.fommil.netlib.BLAS
import Averageble._
import Bootstrap._
import breeze.stats.distributions.{Gaussian, Uniform}
import cats.data.NonEmptyList
import de.wias.nonparregboot.runExperiment.ExperimentResult
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._

case class ExperimentConfig(sampler: DataSampler,
                            trainSize: IntP, targetSize: IntP, partitions: IntP,
                            s: Double, kernel: Kernel,
                            bootIter: IntP,
                            experIter: IntP,
                            experiment: (IntP, EnsembleLearner,
                              Covariates, Responses, Covariates, FStarValues) => ExperimentResult
                           )

object runExperiment {
  type ExperimentResult = (Double, Double)

  implicit def tupleAvg(implicit avg: Averageble[Double]): Averageble[ExperimentResult] = avg.compose(avg)

  def apply(config: ExperimentConfig): ExperimentResult = config match {
    case ExperimentConfig(sampler, trainSize, targetSize, partitions, s, kernel, bootIter, experIter, experiment) =>
      StochasticAverager(experIter) {
        val (x, y, _) = sampler(trainSize)
        val (t, _, ft) = sampler(targetSize)
        val rho = 0.001 * math.pow(trainSize.toDouble, -2 * s / (2 * s + 1))
        val el = KRR.fastKRR(partitions, rho, kernel)
        experiment(bootIter, el, x, y, t, ft)
      }
  }
}

object Main extends IOApp {
  implicit val showConf: Show[(ExperimentConfig, (Double, Double))] = {
    case (ExperimentConfig(_, trainSize, targetSize, partitions, _, _, _, _, _), (rmse, coverage)) =>
      s"n=$trainSize\tt=$targetSize\tP=$partitions\t\trmse=${math.sqrt(rmse)}\tcoverage=$coverage"
  }

  def withBounds(bootIter: IntP, el: EnsembleLearner,
                 x: Covariates, y: Responses, t: Covariates, ft: FStarValues) = {
      val (that, (l, u)) = predictWithConfidence(bootIter, 0.95, el, x, y, t)
      (MSE(that, ft), if (between(l, ft, u)) 1d else 0d)
  }

  def withBall(bootIter: IntP, el: EnsembleLearner,
                 x: Covariates, y: Responses, t: Covariates, ft: FStarValues) = {
      val (that, quantile) = predictWithBall(bootIter, 0.95, el, x, y, t)
      val mse = MSE(that, ft)
      (mse, if (mse < quantile) 1d else 0d)
  }

  def configureAndRun(n: IntP, P: IntP, t: IntP, bootIter: IntP, avgIter: IntP) = IO {
    val xGen     = () => Uniform(0d, 1d).sample()
    val noiseGen = () => Gaussian(0d, 1d).sample()
    val sampler = SampleDataset(xGen, noiseGen, x => sin(x * math.Pi * 2d))
    val experimentConfig = ExperimentConfig(sampler, n, t, P, 3d, Matern52(1d), bootIter, avgIter, withBall)
    val result: ExperimentResult = runExperiment(experimentConfig)
    println((experimentConfig, result).show)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val ps :: ts :: Nil = List(7 to 12, 1 to 9).map(_.map(math.pow(2, _).toInt))

    val n : IntP = 65536 // 2^16
    val tasks = for (p <- ps; t <- ts) yield configureAndRun(n, toIRP(p), toIRP(t), 5000, 200)

    NonEmptyList(IO {
      println(BLAS.getInstance().getClass.getName)
    }, tasks.toList).reduce
  }.as(ExitCode.Success)
}
