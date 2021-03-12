package de.wias.nonparregboot.classifier


import org.platanios.tensorflow.api._

class OutputWrapper[T](private val o: Output[T]) {
  def evaluate: Tensor[T] = Session().run(fetches = o, targets = Set(o))
}

trait OutputEvaluates {
  implicit def wrapOutput[T](o: Output[T]) = new OutputWrapper(o)
}
