package de.wias.tfrandom

import cats.data.State
import org.platanios.tensorflow.api.Op
import org.platanios.tensorflow.api.Tensor
import org.platanios.tensorflow.api.Shape
import org.platanios.tensorflow.api.TF
import org.platanios.tensorflow.api.core.types.{IsHalfOrFloatOrDouble, IsIntOrLong}
import org.platanios.tensorflow.api.implicits.helpers.Zero
import org.platanios.tensorflow.api.ops.Op.OpInput
import org.platanios.tensorflow.api.tf
import org.platanios.tensorflow.api.ops.Output
import org.platanios.tensorflow.api.ops.math.Math

object TFRandom {
  private[tfrandom] lazy val seeder = State[SeedStream, Seed](seedStream => (seedStream.next(), seedStream.seed()))

  private[tfrandom] def wrapInState[T](f: Seed => T): Random[T] = seeder map f

  def uniform01[T: TF : IsHalfOrFloatOrDouble](shape: Shape): RandomOutput[T] = wrapInState(uniform01Gen(shape))

  def int(max: Int, shape: Shape): RandomOutput[Int] = wrapInState(intGen[Int](max, shape))

  def long(max: Long, shape: Shape): RandomOutput[Long] = wrapInState(intGen[Long](max, shape))

  def multivariateNormal[T: TF : IsHalfOrFloatOrDouble](batchSize: Shape, mean: Output[T], rootCov: Output[T]): RandomOutput[T] = {
    val shape: Shape = batchSize ++ mean.shape
    standardGaussian(shape).map(eps => tf.matmul(eps, rootCov, transposeB=true) + mean)
  }

  def gaussian[T: TF : IsHalfOrFloatOrDouble](mean: T, sd: T, shape: Shape): RandomOutput[T] =
    standardGaussian(shape).map(standard => standard * sd + mean)

  def standardGaussian[T: TF : IsHalfOrFloatOrDouble](shape: Shape): RandomOutput[T] = wrapInState(gaussianGen[T](shape))

  private[tfrandom] def gaussianGen[T: TF: IsHalfOrFloatOrDouble](shape: Shape)(seed: Seed) = {
    opBuilderHelper("RandomNormal",
      "StatelessRandomNormal",
      (shape: Output[Int], Output(seed, seed)))
  }

  private[tfrandom] def uniform01Gen[T: TF: IsHalfOrFloatOrDouble](shape: Shape)(seed: Seed) = {
    opBuilderHelper("RandomUniform",
      "StatelessRandomUniform",
      (shape: Output[Int], Output(seed, seed)))
  }

  private[tfrandom] def intGen[T: IsIntOrLong: TF](max: T, shape: Shape)(seed: Seed) = {
    val maxTensor : Output[T] = Tensor.ones[T](Shape()) * max    // there must be
    val zeroTensor: Output[T] = maxTensor - maxTensor           // a better way
    opBuilderHelper("RandomUniformInt",
                   "StatelessRandomUniformInt",
                   (shape: Output[Int], Output(seed, seed), zeroTensor, maxTensor))
  }

  private def opBuilderHelper[In: OpInput, T: TF](name: String, opType: String, input: In) = {
    Op.Builder[In, Output[T]](
      opType = opType,
      name = name,
      input = input
    ).setAttribute("dtype", TF[T].dataType)
      .build()
      .output
  }

  private[tfrandom] def chol[T: TF:  IsHalfOrFloatOrDouble](A: Output[T]): Output[T] = {
    Op.Builder[Output[T], Output[T]](
      opType = "Cholesky",
      name = "Cholesky",
      input = A
    ).build().output
  }
}
