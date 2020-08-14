package de.wias.nonparregboot

import breeze.linalg.DenseVector
import org.scalactic.{Equality, TolerantNumerics}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest._
import org.scalatest.flatspec.AnyFlatSpec

class Matern52Test extends AnyFlatSpec  {
  implicit val doubleEquality: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-3)

  "matern 5/2 " should "coincide with sklearn" in {
    val a = DenseVector(3.24, 1.534)
    val b = DenseVector(-3.63, 9.34)

    val kernel = Matern52(1d)

    assert(kernel(a, b) === 1.63094612e-08)
  }

  "matern 7/2" should "conincide with sklearn" in {
    val a = DenseVector(1d, 2d)
    val b = DenseVector(2d, 3d)

    val kernel = Matern72(1d)
    assert(kernel(a, b) === 0.32806701)
  }
}
