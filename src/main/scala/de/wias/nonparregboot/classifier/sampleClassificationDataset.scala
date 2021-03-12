package de.wias.nonparregboot.classifier


import spire.syntax.field._
import breeze.linalg.{DenseVector, diag}
import cats.data.NonEmptyVector
import de.wias.nonparregboot.Covariates
import scalapurerandom._

object sampleClassificationDataset {
  def apply(n: PosInt): Random[(Covariates[DV], Classes)] = {
    val means = Vector(DenseVector(1d, 1d), DenseVector(-1d, -1d), DenseVector(1d, -1d), DenseVector(-1d, 1d))
    val randInstance: Random[(DV, Int)] = for {
                          y <- int(4)
                          center = means(y)
                          x <- centeredGaussian(diag(DenseVector(1d,1d)*0.1d)) + const(center)
                        } yield (x, y)
    val randData: Random[NEL[(DV, Int)]] = replicateA(n, randInstance)

    randData.map(nel2nev).map(nel => {
        (nel.map(_._1), nel.map(_._2))
    })
  }

  def nel2nev[T](nel: NEL[T]): NEV[T] = NonEmptyVector.of(nel.head, nel.tail :_*)
}
