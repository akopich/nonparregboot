package de.wias.tfrandom

import java.math.BigInteger
import java.security.MessageDigest

/**
 * Here we mimic the behaviour of `tfp.util.SeedStream`
 */
class SeedStream(private val digest: MessageDigest,
                 private val initialSeed: Int,
                 private val counter: Int) {
  def seed(): Seed = {
    val str = (initialSeed, counter).toString()
    val hash = digest.digest(str.getBytes("UTF-8"))
    val bigSeed = new BigInteger(hash)
    val bigModulo = bigSeed.mod(SeedStream.BIG_INT_MAX)
    bigModulo.intValue()
  }

  def next(): SeedStream = new SeedStream(digest, initialSeed, counter + 1)
}

object SeedStream {
  private[tfrandom] lazy val BIG_INT_MAX = BigInteger.valueOf(Integer.MAX_VALUE)

  def apply(initialSeed: Int): SeedStream = {
    val digest = MessageDigest.getInstance("SHA-512")
    new SeedStream(digest, initialSeed, 0)
  }
}