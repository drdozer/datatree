package uk.co.turingatemyhamster
package typeclass

import simulacrum._

@typeclass trait Monoid[M] {
  def mzero: M
  def mappend(m1: M, m2: M): M
}

object Monoid {
  import ops._

  implicit def setMonoid[T]: Monoid[Set[T]] = new Monoid[Set[T]] {
    override def mzero = Set()
    override def mappend(m1: Set[T], m2: Set[T]) = m1 ++ m2
  }

  implicit def optionMonoid[T : Monoid]: Monoid[Option[T]] = new Monoid[Option[T]] {
    override def mzero = None
    override def mappend(m1: Option[T], m2: Option[T]) = (m1, m2) match {
      case (Some(t1), Some(t2)) => Some(t1 mappend t2)
      case (Some(_), None) => m1
      case (None, Some(_)) => m2
      case (None, None) => None
    }
  }
  
  implicit def mapMonoid[K, V : Monoid]: Monoid[Map[K, V]] = new Monoid[Map[K, V]] {
    override def mzero = Map()
    override def mappend(m1: Map[K, V], m2: Map[K, V]) = (
      for(k <- m1.keySet mappend m2.keySet) yield {
        k -> (m1.get(k) mappend m2.get(k)).get
      }
      ).toMap
  }

  def mzero[M : Monoid]: M = implicitly[Monoid[M]].mzero

  object Implicits {
    implicit class MReduce[M](val _c: Iterable[M]) {
      def mreduce(implicit monoid: Monoid[M]): M = _c.foldLeft(monoid.mzero)(monoid.mappend)
    }
  }
}