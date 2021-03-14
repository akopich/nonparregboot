package de.wias.nonparregboot.classifier

import breeze.numerics.sqrt
import org.platanios.tensorflow.api.core.Shape
import org.platanios.tensorflow.api.{Output, TF, tf}
import org.platanios.tensorflow.api.core.types.IsHalfOrFloatOrDouble

object Matern72 {
  def apply(scale: Float)(x: Covariates, y: Covariates): OFloat = {
    val d = dist(x, y) / scale * sqrt(7f)
    tf.exp(-d) * (1f/15f * d*d*d + 0.4f * d*d +  d + 1f )
  }
}


object dist {
  def apply(A: OFloat, B: OFloat): OFloat = {
    val na = tf.sum(tf.square(A), 1).reshape(Shape(-1, 1))
    val nb = tf.sum(tf.square(B), 1).reshape(Shape(1, -1))

    tf.sqrt(tf.maximum(na - 2*tf.matmul(A, B, transposeA = false, transposeB = true) + nb, 0f))
  }
}