package de.wias.random

import cats.data._
import Nat.{inc, mkNat}
import de.wias.random.Pos

trait HeadTailDecomposable[F[_], G[_]] {
  def decompose[A](f : F[A]): (A, G[A])
}

trait HasSize[F[_]] {
  def size[A](f: F[A]): Nat
}

object HeadTailDecomposable {
  implicit class Decomposer[F[_], G[_], A](f: F[A])(implicit val decomposer: HeadTailDecomposable[F, G]) {
    def decompose: (A, G[A]) = decomposer.decompose(f)
  }

  implicit val nelIsDecomposable = new HeadTailDecomposable[NonEmptyList, List] {
    override def decompose[A](f: NonEmptyList[A]): (A, List[A]) = (f.head, f.tail)
  }

  implicit val nevIsDecomposable = new HeadTailDecomposable[NonEmptyVector, Vector] {
    override def decompose[A](f: NonEmptyVector[A]): (A, Vector[A]) = (f.head, f.tail)
  }

  implicit val listHasSize: HasSize[List] = new HasSize[List] {
    override def size[A](f: List[A]): Nat = mkNat(f.size)
  }

  implicit val vecHasSize = new HasSize[Vector] {
    override def size[A](f: Vector[A]): Nat = mkNat(f.size)
  }

  def size[F[_], G[_], A](nel: F[A])(implicit d: HeadTailDecomposable[F, G], s: HasSize[G]): Pos = {
    inc(s.size(d.decompose(nel)._2))
  }
}
