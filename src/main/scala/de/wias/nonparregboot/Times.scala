package de.wias.nonparregboot

import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian

import scala.reflect.ClassTag


object Times {
  implicit class TimesWrapper(val i : Int) {
    def times[T: ClassTag](f:  => T): IndexedSeq[T] = (0 until i) map (_ => f)
  }
}

