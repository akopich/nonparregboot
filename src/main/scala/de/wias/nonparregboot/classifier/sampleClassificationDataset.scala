package de.wias.nonparregboot.classifier


import spire.syntax.field._
import breeze.linalg.{DenseVector, diag}
import cats.data.NonEmptyVector
import scalapurerandom._

object sampleClassificationDataset {
  val apply: ClassificationDataSampler[DV] = (n: PosInt) => {
    val means = Vector(DenseVector(1d, 1d), DenseVector(-1d, -1d), DenseVector(1d, -1d), DenseVector(-1d, 1d))
    val randInstance: Random[(DV, Int)] = for {
                          y <- int(4)
                          center = means(y)
                          x <- centeredGaussian(diag(DenseVector(0.3, 0.3))) + const(center)
                        } yield (x, y)
    val randData: Random[NEL[(DV, Int)]] = replicateA(n, randInstance)

    randData.map(nel2nev).map(nel => {
        (nel.map(_._1), nel.map(_._2))
    })
  }

  def nel2nev[T](nel: NEL[T]): NEV[T] = NonEmptyVector.of(nel.head, nel.tail :_*)
}
