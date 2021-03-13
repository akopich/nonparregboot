package de.wias.tfrandom

import cats.data.State

object TFRandom {
  private[tfrandom] lazy val seeder = State[SeedStream, Seed](seedStream => (seedStream.next(), seedStream.seed()))
}
