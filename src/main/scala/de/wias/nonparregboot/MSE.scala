package de.wias.nonparregboot

import KRR._
import breeze.linalg._

object MSE {
  def apply(p: Predictor, x: Covariates, f: DV) = math.pow(norm(p(x) - f), 2) / x.size
}
