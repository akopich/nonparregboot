package de.wias.nonparregboot

import breeze.linalg._
import scalapurerandom._

object MSE {
  def apply(fhat: DV, f: DV): Double = squaredDistance(fhat, f) / f.size
}
