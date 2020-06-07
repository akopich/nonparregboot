package de.wias.random

import spire.random.Generator
import cats._
import cats.data._
import cats.implicits._
import spire.random.rng.MersenneTwister64

import scala.util.Random

class MersenneTwisterImmutable(private val gen: MersenneTwister64) {
//  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
//    val newgen = gen.copyInit
//    (new MersenneTwisterImmutable(newgen), action(newgen))
//  }
  def apply[T](action : MersenneTwister64 => T): (MersenneTwisterImmutable, T) = {
    (this, action(gen))
  }
}

object RandomPure {
  val multiplier = 0x5DEECE66DL
  val addend = 0xBL
  val mask = (1L << 48) - 1
  val DOUBLE_UNIT = 1.0 / (1L << 53)

  type Gen = MersenneTwisterImmutable
  type Random[T] = State[Gen, T]

  def sample[T](random: Random[T], gen: Gen): T = random.run(gen).value._2

  def Random[T](r: Gen => (Gen, T)): Random[T] = State[Gen, T](r)

  def randomSplit(n: Int): Random[List[Gen]] = (0 until n).view.map(_ =>
    for {
      seed <- long
    } yield getGen(seed)).toList.sequence

  def next(bits: Int): Random[Int] = Random { gen =>
    gen(_.nextBits(bits))
  }

  def int: Random[Int] = Random {gen =>
    gen(_.nextInt())
  }

  def long: Random[Long] = Random {gen =>
    gen(_.nextLong())
  }

  def uniform01: Random[Double] = Random { gen =>
    gen(_.nextDouble())
  }

  def gaussian(mu: Double, sigma: Double): Random[Double] = Random { gen =>
    gen(_.nextGaussian(mu, sigma))
  }

  def getGen(seed: Long) = new MersenneTwisterImmutable(MersenneTwister64.fromTime(time =  seed))
}

object RandomPureExample extends App {
  import RandomPure._
  val gen = getGen(13L)
  for {
    x <- uniform01
    y <- uniform01
  } yield x + y
//  println((0 until 5).toList.map(_ => nextGaussian(0d, 1d)).sequence.run(gen).value)
//  println((0 until 5).toList.map(_ => nextGaussian(0d, 1d)).sequence.run(gen).value)
}