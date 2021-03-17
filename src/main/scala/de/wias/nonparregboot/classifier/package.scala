package de.wias.nonparregboot

import algebra.ring.AdditiveSemigroup
import breeze.optimize.{DiffFunction, FirstOrderMinimizer}
import de.wias.tfrandom.TFRandom
import org.platanios.tensorflow.api.ops.variables.Variable
import org.platanios.tensorflow.api._
import scalapurerandom.{DV, NEV, PSFunctor, PosInt, Random}
import scalapurerandom._

package object classifier extends TensorflowBreezeConverters with OutputEvaluates with Metrics with TFRandom {
  type Classes = Tensor[Int]

  type OFloat = Output[Float]

  type TFloat = Tensor[Float]

  type Covariates = TFloat

  type ClassificationDataSampler = PosInt => Random[(Covariates, Classes)]

  type Kernel = (Covariates, Covariates) => OFloat

  type Classifier = Covariates => ClassificationResults

  type EnsembleClassifier = NEV[Classifier]

  type Init = OFloat

  type ClassifierTrainer[F[_]] = (Covariates, Classes, Init) => F[Classifier]

  type MetricValue = Map[String, Double]

  implicit def metricValueIsAverageble = new Averageble[MetricValue] {
    import algebra.instances.all._
    override val semi: AdditiveSemigroup[MetricValue] = implicitly[AdditiveSemigroup[MetricValue]]

    override def |/|(x: MetricValue, cnt: PosInt): MetricValue = x.map{case(key, value) => (key, value |/| cnt)}
  }

  type Metric = (Classes, ClassificationResults) => MetricValue

  type TI = Tensor[Int]

  type VF = Variable[Float]
}
