package de.wias

import cats.Monad
import cats.data.{State, StateT}
import org.platanios.tensorflow.api.ops.Output
import scalapurerandom.{Averageble, AveragebleHelper, HasSizeHelper, NEL, PSReducible, PosInt, average, replicateA}

package object tfrandom extends AveragebleHelper with HasSizeHelper {
  type Seed = Int

  type Random[T] = State[SeedStream, T]

  type RandomT[M[_], T] = StateT[M, SeedStream, T]

  type RandomOutput[T] = Random[Output[T]]

  implicit class RichRandom[T](private val r: Random[T]) {
    def sample(seed: Int): T = r.runA(SeedStream(seed)).value
  }

  def sampleMean[M[_]: Monad, T: Averageble](random: RandomT[M, T], n : PosInt)
                                            (implicit nelReducer: PSReducible[NEL]): RandomT[M, T] =
    replicateA(n, random).map(average(_))

}
