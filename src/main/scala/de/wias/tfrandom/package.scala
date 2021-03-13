package de.wias

import cats.data.State
import org.platanios.tensorflow.api.ops.Output

package object tfrandom {
  type Seed = Int

  type Random[T] = State[SeedStream, Output[T]]
}
