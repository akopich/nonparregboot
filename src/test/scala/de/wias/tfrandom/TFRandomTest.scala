package de.wias.tfrandom

import org.scalatest.flatspec.AnyFlatSpec
import de.wias.nonparregboot.classifier.OutputEvaluates
import org.platanios.tensorflow.api.{Output, Shape, TF, Tensor, tf}

import scala.reflect.ClassTag

class TFRandomTest extends AnyFlatSpec with OutputEvaluates with TFRandom {
  val SHAPE = Shape(10000)

  test("uniform01[Float]")(uniform01Gen[Float](SHAPE))

  test("uniform01[Double]")(uniform01Gen[Double](SHAPE))

  test("int[Int]")(intGen(5, SHAPE))

  test("int[Long]")(intGen(5L, SHAPE))

  test("gaussian[Float]")(gaussianGen[Float](SHAPE))

  test("gaussian[Double]")(gaussianGen[Double](SHAPE))

  "uniform01" should "sample differently when called twice in a row" in {
    val rCheck: Random[Boolean] = for {
      first  <- uniform01[Float](SHAPE)
      second <- uniform01[Float](SHAPE)
    } yield areTensorsEqual(first, second, 1e-3f)
    assert(!rCheck.sample(13))
  }

  "uniform01" should "sample the same when called twice with the same seed" in {
    assert(areTensorsEqual(uniform01[Float](SHAPE).sample(13),
      uniform01[Float](SHAPE).sample(13), 1e-3f))
  }

  "standard normals" should "be around 0" in {
    val r: Random[Float] = for {
      normals <- standardGaussian[Float](SHAPE)
    } yield {
      normals.mean().evaluate.scalar
    }

    val empiricalMean = r.sample(13)

    assert(empiricalMean < 0.01 && empiricalMean > -0.01)
  }

  "multivariate normal" should "meet the first two moments" in {
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
    assert(areTensorsEqual(meanHat.evaluate, mean, 0.1f))
    assert(areTensorsEqual(covHat.evaluate, cov, 0.1f))
  }

  private def areTensorsEqual(a: Output[Float], b: Output[Float], eps: Float) = (a - b).abs.max().evaluate.scalar < eps

  private def test[T: ClassTag: TF](name: String)(generator: Seed => Output[T]) = {
    name should "be reproducible" in {
      val r1 = generator(0).evaluate
      val r2 = generator(0).evaluate

      assert(r1.toArray === r2.toArray)
    }

    name should "depend on seed" in {
      val r1 = generator(0).evaluate
      val r2 = generator(13).evaluate

      assert(r1.toArray !== r2.toArray)
    }
  }
}
