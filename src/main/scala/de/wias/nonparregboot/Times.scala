package de.wias.nonparregboot

import scala.reflect.ClassTag
import scala.collection.parallel.CollectionConverters._
import NEV._


object Times {
  implicit class TimesWrapper(val i : IntP) {
    def times[T: ClassTag](f:  => T): NEV[T] = toNEV ((0 until i) map (_ => f) )

    def parTimes[T: ClassTag](f:  => T): NEV[T] = toNEV ( (0 until i).par map (_ => f) seq )
  }
}

