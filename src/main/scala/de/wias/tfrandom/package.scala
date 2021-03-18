package de.wias

import cats.Monad
import cats.data.{State, StateT}
import org.platanios.tensorflow.api.ops.Output

package object tfrandom {
  type Seed = Int

  type SeedStream = SeedStreamImpl

  val SeedStream = SeedStreamImpl

  type Random[T] = State[SeedStream, T]

  type RandomT[M[_], T] = StateT[M, SeedStream, T]

  type RandomOutput[T] = Random[Output[T]]

}
