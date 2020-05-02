package de.wias.nonparregboot

import cats.kernel.Semigroup

object BreezeCats {
  import KRR._

  implicit val semigroupDV = new Semigroup[DV] {
    override def combine(x: DV, y: DV): DV = x + y
  }
}
