package de.wias.nonparregboot

import algebra.ring.AdditiveSemigroup
import breeze.optimize.{DiffFunction, FirstOrderMinimizer}
import org.platanios.tensorflow.api.ops.variables.Variable
import org.platanios.tensorflow.api.tensors.Tensor
import scalapurerandom.{DV, NEV, PSFunctor, PosInt, Random}
import scalapurerandom._

package object classifier extends Metrics with TensorflowBreezeConverters with OutputEvaluates {
  type Classes = NEV[Int]

  type ClassificationDataSampler[In] = PosInt => Random[(Covariates[In], Classes)]

  type Classifier[In] = Covariates[In] => NEV[ClassificationResult]

  type EnsembleClassifier[In] = NEV[Classifier[In]]

  def ensemblePredict[In](ec: EnsembleClassifier[In])
                         (implicit psf: PSFunctor[NEV]): Classifier[In] = (x: Covariates[In]) => {
    toNEV((ec: NEV[Classifier[In]]).pmap(_(x).toVector).toVector.transpose)
      .pmap(vec => aggregate(toNEV(vec)))
  }

  def aggregate(rs: NEV[ClassificationResult]): ClassificationResult = {
    import SeqReducibleInstance._
    ClassificationResult(average(rs.map(_.scores)))
  }

  type Init = DV

  type ClassifierTrainer[In] = (Covariates[In], Classes, Init) => OptRes[Classifier[In]]

  type Optimizer = FirstOrderMinimizer[DV, DiffFunction[DV]]

  type OptimizerState = FirstOrderMinimizer[DV, DiffFunction[DV]]#State

  class OptimizationFail(val state: OptimizerState) {
    override def toString: String = s"value=${state.value} iter=${state.iter} " +
      s"convergence reason=${state.convergenceReason} convergence info=${state.convergenceInfo}"
  }

  object OptimizationFail {
    def apply(state: OptimizerState) = new OptimizationFail(state)
  }

  type OptRes[T] = Either[OptimizationFail, T]

  type MetricValue = Map[String, Double]

  implicit def metricValueIsAverageble = new Averageble[MetricValue] {
    import algebra.instances.all._
    override val semi: AdditiveSemigroup[MetricValue] = implicitly[AdditiveSemigroup[MetricValue]]

    override def |/|(x: MetricValue, cnt: PosInt): MetricValue = x.map{case(key, value) => (key, value |/| cnt)}
  }

  type Metric = (NEV[Int], NEV[ClassificationResult]) => MetricValue

  type TFloat = Tensor[Float]

  type TI = Tensor[Int]

  type VF = Variable[Float]
}
