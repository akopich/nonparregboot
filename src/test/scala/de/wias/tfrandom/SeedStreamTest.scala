package de.wias.tfrandom

import org.scalatest.flatspec.AnyFlatSpec

class SeedStreamTest extends AnyFlatSpec {
  private lazy val seedStream = SeedStream(13)

  "SeedStream instantiated with the same initialSeed" should "give the same seed" in {
    val sameSeedStream = SeedStream(13)

    assert(seedStream.seed() === sameSeedStream.seed())
  }

  "SeedStream instantiated with different initialSeed" should "givea  different seed" in {
    val otherSeedStream = SeedStream(42)

    assert(seedStream.seed() !== otherSeedStream.seed())
  }

  "SeedStream" should "return the same seed" in {
    val firstSeed = seedStream.seed()
    val firstSeedAskedAgain = seedStream.seed()

    assert(firstSeed === firstSeedAskedAgain)
  }

  "SeedStream and SeedStream.next()" should "NOT return the same seed" in {
    val seedStreamNext = seedStream.next()

    assert(seedStream.seed() !== seedStreamNext.seed())
  }

  "seeder" should "not repeat seeds" in {
    import de.wias.tfrandom.TFRandom._
    val isEqual = for {
      seed1 <- seeder
      seed2 <- seeder
    } yield seed1 == seed2

    assert(!isEqual.runA(seedStream).value)
  }
}
