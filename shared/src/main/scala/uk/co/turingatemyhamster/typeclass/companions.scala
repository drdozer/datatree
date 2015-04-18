package uk.co.turingatemyhamster.typeclass

import simulacrum._

import scala.annotation.implicitNotFound

@implicitNotFound("Could not find constructor for ${T}")
trait Constructor1[T] {
  type A

  def apply(a: A): T
}

//object Constructor1 {
//  implicit def cov[S <: T, T](implicit sC: Constructor1[S]): Constructor1[T] = new Constructor1[T] {
//    override def apply(a: A) = sC.apply(a)
//
//    type A = sC.A
//  }
//}

@implicitNotFound("Could not find destructor for ${T}")
trait Destructor1[T] {
  type A

  def unapply(t: T): Option[A]
}

trait Companion1[T] extends Constructor1[T] with Destructor1[T] {
  type A

  def apply(a: A): T
  def unapply(t: T): Option[A]
}

trait Companion2[T] {
  type A
  type B

  def apply(a: A, b: B): T
  def unapply(t: T): Option[(A, B)]
}

trait Companion3[T] {
  type A
  type B
  type C

  def apply(a: A, b: B, c: C): T
  def unapply(t: T): Option[(A, B, C)]
}

trait Companion4[T] {
  type A
  type B
  type C
  type D

  def apply(a: A, b: B, c: C, d: D): T
  def unapply(t: T): Option[(A, B, C, D)]
}

@typeclass trait Companion_*[T[_]] {
  def apply[A](as: A*): T[A]
  def unapply[A](ta: T[A]): Option[Seq[A]]
}

trait Cstr[α] {
  type λ[β] = ConstructorChain[β, α]
}

@implicitNotFound("Could not build a constructor chain from ${S} to ${T}")
trait ConstructorChain[S, T] extends Function1[S, T] {
  def apply(s: S): T
}

object ConstructorChain {
  implicit def castS[S, T](implicit ev: S <:< T): ConstructorChain[S, T] = new ConstructorChain[S, T] {
    override def apply(a: S): T = a
  }

  implicit def fromChain[R, S, T](implicit
                                  cstr: Constructor1[T] { type A = S },
                                  chain: ConstructorChain[R, S]): ConstructorChain[R, T] =
    new ConstructorChain[R, T] {
      override def apply(r: R) = cstr(chain(r))
    }

  // if implicit, cause divergence, but used invarious work-arounds for SI5070
  def castT[S, TT, T](implicit ev: TT <:< T, chain: ConstructorChain[S, TT]): ConstructorChain[S, T] =
    new ConstructorChain[S, T] {
      override def apply(s: S) = chain(s)
    }
}
