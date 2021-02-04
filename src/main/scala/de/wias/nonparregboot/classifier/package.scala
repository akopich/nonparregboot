package de.wias.nonparregboot

import scalapurerandom.{NEV, PosInt, Random}

package object classifier {
  type Classes = NEV[Int]

  type ClassificationDataSampler[In] = PosInt => Random[(Covariates[In], Classes)]

  type Classifier[In] = Covariates[In] => Classes

  type ClassifierTrainer[In] = (Covariates[In], Classes) => Classifier[In]
}
