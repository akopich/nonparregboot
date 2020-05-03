package de.wias.nonparregboot

import KRR._
import breeze.linalg._

object RMSE {
  def apply(p: Predictor, x: Covariates, f: DV) = norm(p(x) - f)
}
