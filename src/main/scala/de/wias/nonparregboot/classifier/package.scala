package de.wias.nonparregboot

import breeze.optimize.{DiffFunction, FirstOrderMinimizer}
import scalapurerandom.{DV, NEV, PosInt, Random}

package object classifier {
  type Classes = NEV[Int]

  type ClassificationDataSampler[In] = PosInt => Random[(Covariates[In], Classes)]

  type Classifier[In] = Covariates[In] => NEV[ClassificationResult]

  type ClassifierTrainer[In] = (Covariates[In], Classes) => OptRes[Classifier[In]]

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
}
