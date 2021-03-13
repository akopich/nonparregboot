package de.wias.tfrandom

import algebra.ring.AdditiveSemigroup
import org.platanios.tensorflow.api._
import org.platanios.tensorflow.api.core.types.IsIntOrLongOrHalfOrFloatOrDouble
import org.platanios.tensorflow.api.ops.Output
import scalapurerandom.{Averageble, PosInt}

trait OutputAveragebleInstances {

  implicit def outputsAreAverageble[T: TF: IsIntOrLongOrHalfOrFloatOrDouble]: Averageble[Output[T]] = new Averageble[Output[T]] {
    override val semi: AdditiveSemigroup[Output[T]] = (x: Output[T], y: Output[T]) => x + y

    override def |/|(x: Output[T], cnt: PosInt): Output[T] = {
//      val denominator: Output[T] = Tensor.ones[T](Shape()) * cnt.toInt
//      x / cnt.toInt
      ???
    }
  }
}
