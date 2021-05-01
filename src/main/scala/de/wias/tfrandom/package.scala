package de.wias

import cats.data.State
import org.platanios.tensorflow.api.ops.Output

package object tfrandom {
  type Seed = Int

  type Random[T] = State[SeedStream, T]

  type RandomOutput[T] = Random[Output[T]]

  implicit class RichRandom[T](private val r: Random[T]) {
    def sample(seed: Int): T = r.runA(SeedStream(seed)).value
  }
}
