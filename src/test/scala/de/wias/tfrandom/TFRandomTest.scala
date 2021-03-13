package de.wias.tfrandom

import org.scalatest.flatspec.AnyFlatSpec
import TFRandom._
import de.wias.nonparregboot.classifier.OutputEvaluates
import org.platanios.tensorflow.api.{Output, Shape, Tensor, tf}

import scala.reflect.ClassTag

class TFRandomTest extends AnyFlatSpec with OutputEvaluates {
  test("uniform01[Float]")(uniform01[Float])

  test("uniform01[Double]")(uniform01[Double])

  test("int[Int]")(intGen(5))

  test("int[Long]")(intGen(5L))

  test("gaussian[Float]")(gaussian[Float])

  test("gaussian[Double]")(gaussian[Double])

  "standard normals" should "be around 0" in {
    val r: Random[Float] = for {
      normals <- standardGaussian[Float](Shape(10000))
    } yield {
      normals.mean().evaluate.scalar
    }

    val empiricalMean = r.sample(13)

    assert(empiricalMean < 0.01 && empiricalMean > -0.01)
  }

  "multivariate normal" should "work" in {
    val mean = Tensor(1f, 1000f)
    val cov = Tensor(Tensor(1f, 0.2f), Tensor(0.2f, 2f))
    val root = chol(cov)

    val n = 10000
    val r = multivariateNormal(Shape(n), mean, root).map(samples => {
      val meanHat = samples.mean(Tensor(0))
      val centered = samples - meanHat
      val covHat = tf.matmul(centered, centered, transposeA = true) / n
      (meanHat, covHat)
    })
    val (meanHat, covHat) = r.sample(13)
    println(meanHat.evaluate.summarize())
    println(covHat.evaluate.summarize())
  }

  private def test[T: ClassTag](name: String)(generator: (Shape, Seed) => Output[T]) = {
    val SHAPE = Shape(100)
    name should "be reproducible" in {
      val r1 = generator(SHAPE, 0).evaluate
      val r2 = generator(SHAPE, 0).evaluate

      assert(r1.toArray === r2.toArray)
    }

    name should "depend on seed" in {
      val r1 = generator(SHAPE, 0).evaluate
      val r2 = generator(SHAPE, 13).evaluate

      assert(r1.toArray !== r2.toArray)
    }
  }
}
