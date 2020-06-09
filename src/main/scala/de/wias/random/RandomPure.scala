package de.wias.random

import breeze.numerics.log
import spire.random.Generator
import cats._
import cats.data._
import cats.implicits._
import de.wias.nonparregboot.Nat._
import de.wias.nonparregboot.Pos
import spire.random.rng.MersenneTwister64

import scala.util.Random

class MersenneTwisterImmutable(private val gen: MersenneTwister64) {
  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
    val newgen = gen.copyInit
    (new MersenneTwisterImmutable(newgen), action(newgen))
  }
}

object RandomPure {
  val multiplier = 0x5DEECE66DL
  val addend = 0xBL
  val mask = (1L << 48) - 1
  val DOUBLE_UNIT = 1.0 / (1L << 53)

  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]
  def Random[T]   = State[Gen, T] _

  def sample[T](random: Random[T], gen: Gen): T = random.run(gen).value._2

  def mixture[T](ra: Random[T], rb: Random[T]): Random[T] = for {
    a <- ra
    b <- rb
    p <- uniform01
  } yield if (p > 0.5d) a else b

  def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
    for {
      seed <- long
    } yield getGen(seed)).toList.sequence

  def randomSplit(n: Pos): Random[NonEmptyVector[Gen]] = (n times None).map {_ =>
    long.map(getGen)
  } sequence

  def next(bits: Int): Random[Int] = Random { gen =>
    gen(_.nextBits(bits))
  }

  def int: Random[Int] = Random { gen =>
    gen(_.nextInt())
  }

  def long: Random[Long] = Random { gen =>
    gen(_.nextLong())
  }

  def uniform01: Random[Double] = Random { gen =>
    gen(_.nextDouble())
  }

  def gaussian(mu: Double, sigma: Double): Random[Double] = Random { gen =>
    gen(_.nextGaussian(mu, sigma))
  }

  def laplace(location: Double, scale: Double): Random[Double] = uniform01.map { u =>
    // from numpy
    if (u < 0.5) location + scale * log(2 * u)
    else location - scale * log(2 * (1 - u))
  }

  def getGen(seed: Long) = new MersenneTwisterImmutable(MersenneTwister64.fromTime(time =  seed))
}
