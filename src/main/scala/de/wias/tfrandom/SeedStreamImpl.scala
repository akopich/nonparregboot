package de.wias.tfrandom

import java.math.BigInteger
import java.security.MessageDigest

/**
 * Here we mimic the behaviour of `tfp.util.SeedStream`
 */
private[tfrandom] class SeedStreamImpl(private val digest: MessageDigest,
                     private val initialSeed: Int,
                     private val counter: Int) {
  def seed(): Seed = {
    val str = (initialSeed, counter).toString()
    val hash = digest.digest(str.getBytes("UTF-8"))
    val bigSeed = new BigInteger(hash)
    val bigModulo = bigSeed.mod(SeedStreamImpl.BIG_INT_MAX)
    bigModulo.intValue()
  }

  def next(): SeedStreamImpl = new SeedStreamImpl(digest, initialSeed, counter + 1)
}

private[tfrandom] object SeedStreamImpl {
  private[tfrandom] lazy val BIG_INT_MAX = BigInteger.valueOf(Integer.MAX_VALUE)

  def apply(initialSeed: Int): SeedStreamImpl = {
    val digest = MessageDigest.getInstance("SHA-512")
    new SeedStreamImpl(digest, initialSeed, 0)
  }
}