package de.wias.nonparregboot

import breeze.numerics.sin
import cats._
import cats.implicits._
import cats.effect._
import cats.data._
import com.github.fommil.netlib.BLAS
import Averageble._
import Bootstrap._
import breeze.stats.distributions.{Gaussian, Uniform}
import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.auto._
import eu.timepit.refined.numeric._


object Main extends IOApp {
  type Conf[T] = Reader[ExperimentConfig, T]

  type ExperimentResult = (Double, Double)

  implicit def tupleAvg(implicit avg: Averageble[Double]): Averageble[ExperimentResult] = avg.compose(avg)

  case class ExperimentConfig(sampler: DataSampler,
                              trainSize: IntP, targetSize: IntP, partitions: IntP,
                              s: Double, kernel: Kernel,
                              bootIter: IntP,
                              experIter: IntP,
                              checkCoverage: (IntP, EnsemblePredictor,
                                               Covariates, FStarValues
                                             ) => ExperimentResult
                             )

  implicit val showConf: Show[(ExperimentConfig, (Double, Double))] = {
    case (ExperimentConfig(_, trainSize, targetSize, partitions, _, _, _, _, _), (rmse, coverage)) =>
      s"n=$trainSize\tt=$targetSize\tP=$partitions\t\trmse=${math.sqrt(rmse)}\tcoverage=$coverage"
  }

  def trainData: Conf[(Covariates, Responses)] = Reader { conf =>
    val (x, y, _) = conf.sampler(conf.trainSize)
    (x, y)
  }

  def targetData: Conf[(Covariates, FStarValues)] = Reader { conf =>
    val (t, _, ft) = conf.sampler(conf.targetSize)
    (t, ft)
  }

  def rho: Conf[Double] = Reader { conf => 0.001 * math.pow(conf.trainSize.toDouble, -2 * conf.s / (2 * conf.s + 1)) }

  def el(rho: Double): Conf[EnsembleLearner] = Reader { conf => KRR.fastKRR(conf.partitions, rho, conf.kernel) }

  def run(ep: EnsemblePredictor,
          t: Covariates, ft: FStarValues): Conf[ExperimentResult] = Reader { conf =>
    conf.checkCoverage(conf.bootIter, ep, t, ft)
  }

  def runExperiment: Conf[ExperimentResult] = for {
    (x, y)  <- trainData
    (t, ft) <- targetData
    rho     <- rho
    el      <- el(rho)
    result  <- run(el(x, y), t, ft)
  } yield result

  def checkCoverageBounds(bootIter: IntP,
                          ep: EnsemblePredictor,
                          t: Covariates, ft: FStarValues): ExperimentResult = {
      val (that, (l, u)) = predictWithConfidence(bootIter, 0.95, ep, t)
      (MSE(that, ft), if (between(l, ft, u)) 1d else 0d)
  }

  def checkCoverageBall(bootIter: IntP,
                        ep: EnsemblePredictor,
                        t: Covariates, ft: FStarValues): ExperimentResult = {
      val (that, quantile) = predictWithBall(bootIter, 0.95, ep, t)
      val mse = MSE(that, ft)
      (mse, if (mse < quantile) 1d else 0d)
  }

  def configureAndRun(n: IntP, P: IntP, t: IntP, bootIter: IntP, avgIter: IntP) = IO {
    val xGen     = () => Uniform(0d, 1d).sample()
    val noiseGen = () => Gaussian(0d, 1d).sample()
    val sampler = sampleDataset(xGen, noiseGen, x => sin(x * math.Pi * 2d))
    val experimentConfig = ExperimentConfig(sampler, n, t, P, 3d, Matern52(1d), bootIter, avgIter, checkCoverageBall)
    val result: ExperimentResult = runExperiment(experimentConfig)
    println((experimentConfig, result).show)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val functor = implicitly[Functor[List]].compose(implicitly[Functor[Vector]])
    val ps :: ts :: Nil = functor.map(List(7 to 12, 1 to 9).map(_.map(math.pow(2, _).toInt).toVector))(toIRP)

    val n : IntP = 65536 // 2^16
    val tasks = for (p <- ps; t <- ts) yield configureAndRun(n, p, t, 5000, 200)

    NonEmptyList(IO {
      println(BLAS.getInstance().getClass.getName)
    }, tasks.toList).reduce
  }.as(ExitCode.Success)
}
