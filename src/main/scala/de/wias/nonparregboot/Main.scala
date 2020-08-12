package de.wias.nonparregboot

import breeze.numerics.sin
import cats._
import cats.data._
import cats.implicits._
import cats.effect._
import com.github.fommil.netlib.BLAS
import Bootstrap._
import cats.~>
import scalapurerandom._

object Main extends IOApp {
  type Conf[In, T] = Reader[ExperimentConfig[In], T]
  def Conf[In, T] = Reader[ExperimentConfig[In], T] _

  type ConfRandom[In, T] = ReaderT[Random, ExperimentConfig[In], T]
  def ConfRandom[In, T](f: ExperimentConfig[In] => Random[T]): ConfRandom[In, T] = Kleisli(f)

  type ExperimentResult = (Double, Double)

  implicit def tupleAvg(implicit avg: Averageble[Double]): Averageble[ExperimentResult] = avg.compose(avg)

  case class ExperimentConfig[In](sampler: DataSampler[In],
                              trainSize: PosInt, targetSize: PosInt, partitions: PosInt,
                              s: Double, kernel: Kernel,
                              bootIter: PosInt,
                              bootOnce: NEV[Responses] => Random[Responses],
                              experIter: PosInt,
                              checkCoverage: (NEV[Responses] => Random[NEV[Responses]],
                                               EnsemblePredictor[In],
                                               Covariates[In], FStarValues
                                             ) => Random[ExperimentResult]
                             )

  implicit def showConf[In]: Show[(ExperimentConfig[In], (Double, Double))] = {
    case (ExperimentConfig(_, trainSize, targetSize, partitions, _, _, _, _, _, _), (rmse, coverage)) =>
      s"n=$trainSize\tt=$targetSize\tP=$partitions\t\trmse=${math.sqrt(rmse)}\tcoverage=$coverage"
  }

  def trainData: ConfRandom[DV, (Covariates[DV], Responses)] = ConfRandom { conf => for {
      (x, y, _) <- conf.sampler(conf.trainSize)
    } yield (x, y)
  }

  def targetData: ConfRandom[DV, (Covariates[DV], FStarValues)] = ConfRandom { conf => for {
      (t, _, ft) <- conf.sampler(conf.targetSize)
    } yield (t, ft)
  }

  def rho: Conf[DV, Double] = Reader { conf: ExperimentConfig[DV] => 0.001 * math.pow(conf.trainSize.toInt, -2 * conf.s / (2 * conf.s + 1)) }

  def el(rho: Double): Conf[DV, EnsembleLearner[DV]] = Reader { conf => KRR.fastKRR(conf.partitions, rho, conf.kernel) }

  def run(ep: EnsemblePredictor[DV],
          t: Covariates[DV],
          ft: FStarValues): ConfRandom[DV, ExperimentResult] = ConfRandom { conf =>
    conf.checkCoverage(boot(conf.bootIter, conf.bootOnce), ep, t, ft)
  }


  def averager(once: ConfRandom[DV, ExperimentResult]): ConfRandom[DV, IO[ExperimentResult]] = {
    val f = Conf { conf: ExperimentConfig[DV] => sampleMeanPar(_: Random[ExperimentResult], conf.experIter) }
    ConfRandom((f <*> Conf(once.run)).run)
  }

  def lift[T, In](k : Kleisli[Id, ExperimentConfig[In], T]): Kleisli[Random, ExperimentConfig[In], T] =
    k.mapK(λ[Id ~> Random](_.pure[Random]))

  def runExperiment: ConfRandom[DV, ExperimentResult] = for {
    (x, y)  <- trainData
    (t, ft) <- targetData
    rho     <- lift(rho)
    el      <- lift(el(rho))
    result  <- run(el(x, y), t, ft)
  } yield result

  def checkCoverageBounds[In](boot: NEV[Responses] => Random[NEV[Responses]],
                          ep: EnsemblePredictor[In],
                          t: Covariates[In], ft: FStarValues): Random[ExperimentResult] = {
    val (that, bounds) = predictWithConfidence(boot, 0.95, ep, t)
    bounds.map { case(l, u) =>
      (MSE(that, ft), if (between(l, ft, u)) 1d else 0d)
    }
  }

  def checkCoverageBall[In](boot: NEV[Responses] => Random[NEV[Responses]],
                        ep: EnsemblePredictor[In],
                        t: Covariates[In], ft: FStarValues): Random[ExperimentResult] = {
      val (that, quantileR) = predictWithBall(boot, 0.95, ep, t)
      val mse = MSE(that, ft)
      quantileR.map{ quantile => (mse, if (mse < quantile) 1d else 0d) }
  }

  def configure(n: PosInt, P: PosInt, t: PosInt, bootIter: PosInt, avgIter: PosInt): ExperimentConfig[DV] =  {
    val xGen = uniform01
    val noiseGen = gaussian(0d, 1d)
    val sampler = sampleDataset(xGen, noiseGen, x => sin(x * math.Pi * 2d))
    ExperimentConfig(sampler, n, t, P, 3d, Matern52(1d), bootIter, bootAvgOnceWithReturn, avgIter, checkCoverageBounds)
  }

  def print(res: IO[ExperimentResult]): Conf[DV, IO[Unit]] = Conf { conf =>
      res.map(r => println((conf, r).show) )
  }

  def runAverage: ConfRandom[DV, IO[Unit]] = for {
    result <- averager(runExperiment)
    io     <- lift(print(result))
  } yield io

  override def run(args: List[String]): IO[ExitCode] = {
    val functor = implicitly[Functor[List]] compose implicitly[Functor[List]]
    val ps :: ts :: Nil = functor.map(List(7 to 12 toList, 1 to 9 toList))(PosInt.apply _ >>>  (pow(p"2", _)))

    val gen = getGen(13L)

    val n : PosInt = pow(p"2", p"16")
    val confs = for (p <- ps; t <- ts) yield configure(n, p, t, p"5000", p"200")
    val rios = confs.map(runAverage(_)).sequence
    val tasks = rios.sample(gen).reduce(_ |+| _)

    IO {
      println(BLAS.getInstance().getClass.getName)
    } |+| tasks
  }.as(ExitCode.Success)
}
