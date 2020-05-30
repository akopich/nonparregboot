package de.wias.nonparregboot

import breeze.numerics.sqrt
import breeze.stats.distributions.Gaussian
import cats.data.NonEmptyVector
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Positive

import scala.reflect.ClassTag
import scala.collection.parallel.CollectionConverters._


object Times {
  implicit class TimesWrapper(val i : IRP) {
    def times[T: ClassTag](f:  => T): NEV[T] = toNEV ((0 until i.value) map (_ => f) )

    def parTimes[T: ClassTag](f:  => T): NEV[T] = toNEV ( (0 until i.value).par map (_ => f) seq )
  }
}

