package de.wias.nonparregboot.classifier


import org.platanios.tensorflow.api._


trait OutputEvaluates {
  implicit class OutputWrapper[T](private val o: Output[T]) {
    def evaluate: Tensor[T] = Session().run(fetches = o, targets = Set(o))
  }

}
