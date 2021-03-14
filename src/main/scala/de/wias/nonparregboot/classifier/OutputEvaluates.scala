package de.wias.nonparregboot.classifier


import org.platanios.tensorflow.api._

trait Evaluatable[T, E] {
  def evaluate(t: T): E
}

trait OutputEvaluates {
  implicit def outputIsEvaluatable[F: TF]: Evaluatable[Output[F], Tensor[F]] =
    (t: Output[F]) => Session().run(fetches = t, targets = Set(t))

  implicit def tupleIsEvaluatable[A, B, AE, BE](implicit aeval: Evaluatable[A, AE],
                                                beval: Evaluatable[B, BE]): Evaluatable[(A, B), (AE, BE)] =
    (t: (A, B)) => (aeval.evaluate(t._1), beval.evaluate(t._2))

  implicit class OutputWrapper[T, E](private val t: T)(implicit val eval: Evaluatable[T, E]) {
    def evaluate: E = eval.evaluate(t)
  }

}
