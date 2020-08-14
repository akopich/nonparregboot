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

object Matern72 {
  def apply(scale : Double) : Kernel = (a: DV, b: DV) => {
    val d = norm(a - b) / scale * sqrt(7d)
    exp(-d) * (1d/15d * d*d*d + 0.4d * d*d +  d + 1d )
  }
}
