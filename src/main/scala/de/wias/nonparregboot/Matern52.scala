package de.wias.nonparregboot

import breeze.linalg._
import breeze.numerics.{exp, sqrt}
import scalapurerandom._

object Matern52 {
  def apply(scale : Double) : Kernel = (a: DV, b: DV) => {
    val d = norm(a - b) / scale * sqrt(5d)
    (1d + d + d * d / 3d) * exp(-d)
  }
}