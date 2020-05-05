package de.wias.nonparregboot

import KRR._
import breeze.linalg._

object MSE {
  def apply(fhat: DV, f: DV) = math.pow(norm(fhat - f), 2) / f.size
}
