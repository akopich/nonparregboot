package de.wias.nonparregboot


import breeze.linalg._
import breeze.stats.distributions.MultivariateGaussian
import smile.math.kernel.MercerKernel
import BreezeCats._
import breeze.numerics.sin
import cats._
import cats.implicits._
import cats.effect._
import breeze.linalg._
import breeze.numerics._
import breeze.plot._
import KRR._
import com.github.fommil.netlib.BLAS
import Averageble._
import cats.data.NonEmptyList
import de.wias.nonparregboot.Experimentor.ExperimentResult

case class ExperimentConfig(sampler: DataSampler,
                            trainSize: Int, targetSize: Int, partitions: Int,
                            s: Double, kernel: Kernel,
                            bootIter: Int,
                            experIter: Int
                           )

object Experimentor {
  type ExperimentResult = (Double, Double)

  implicit val tupleAvg: Averageble[ExperimentResult] = implicitly[Averageble[Double]]
    .compose(implicitly[Averageble[Double]])

  def apply(config: ExperimentConfig): ExperimentResult = config match {
    case ExperimentConfig(sampler, trainSize, targetSize, partitions, s, kernel, bootIter, experIter) =>
      StochasticAverager(experIter) {
        val rho = 0.001 * math.pow(trainSize, -2 * s / (2 * s + 1))
        val (x, y, _) = sampler(trainSize)
        val (t, _, ft) = sampler(targetSize)
        val el = KRR.fastKRR(partitions, rho, kernel)
        val (ep, (l, u)) = Bootstrap.confidenceIntevals(bootIter, 0.95, el, x, y, t)
        (MSE(average(ep), t, ft), if (between(l, ft, u)) 1d else 0d)
      }
  }
}

object Main extends IOApp {
  implicit val showConf: Show[(ExperimentConfig, (Double, Double))] = {
    case (ExperimentConfig(_, trainSize, targetSize, partitions, _, _, _, _), (rmse, coverage)) =>
      s"n=$trainSize\tt=$targetSize\tP=$partitions\t\trmse=${math.sqrt(rmse)}\tcoverage=$coverage"
  }

  def runSignle(n: Int, P: Int, t: Int, bootIter: Int, avgIter: Int) = IO {
    val sampler = SampleDataset(0.01, x => sin(x * math.Pi * 2d))
    val experimentConfig = ExperimentConfig(sampler, n, t, P, 3d, Matern52(1d), bootIter, avgIter)
    val result: ExperimentResult = Experimentor(experimentConfig)
    println((experimentConfig, result).show)
  }

  override def run(args: List[String]): IO[ExitCode] = {
    val ps :: ts :: Nil = List((7 to 13), (1 to 9)).map(_.map(math.pow(2, _).toInt))

    val n = math.pow(2, 16).toInt
    val tasks = for (p <- ps; t <- ts) yield runSignle(n, p, t, 5000, 200)

    NonEmptyList(IO {
      println(BLAS.getInstance().getClass.getName)
    }, tasks.toList).reduce
  }.as(ExitCode.Success)
}
