package de.wias.nonparregboot

import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian

import scala.reflect.ClassTag
import scala.collection.parallel.CollectionConverters._


object Times {
  implicit class TimesWrapper(val i : Int) {
    def times[T: ClassTag](f:  => T): IndexedSeq[T] = (0 until i) map (_ => f)

    def parTimes[T: ClassTag](f:  => T): Seq[T] = (0 until i).par map (_ => f) seq
  }
}

