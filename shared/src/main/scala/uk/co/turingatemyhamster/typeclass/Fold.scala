package uk.co.turingatemyhamster
package typeclass

import simulacrum._

trait Fold1[T, A1] {
  def fold[X](t: T)(
    f1: A1 => X): X
}

object Fold1 {
  def apply[T, A1](implicit fold: Fold1[T, A1]): Fold1[T, A1] = fold

  trait Ops[T, A1] {
    def typeClassInstance: Fold1[T, A1]
    def self: T
    def fold[X](
        f1: A1 => X): X = typeClassInstance.fold(self)(f1)
  }

  trait ToFold1Ops {
    implicit def toFold1Ops[T, A1](target: T)(implicit tc: Fold1[T, A1]): Ops[T, A1] = new Ops[T, A1] {
      val self = target
      val typeclassInstance = tc
    }
  }

  object ops extends ToFold1Ops
}


trait Fold2[T, A1, A2] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X): X
}

object Fold2 {
  def apply[T, A1, A2](implicit fold: Fold2[T, A1, A2]): Fold2[T, A1, A2] = fold

  trait Ops[T, A1, A2] {
    def typeClassInstance: Fold2[T, A1, A2]
    def self: T
    def fold[X](
        f1: A1 => X,
        f2: A2 => X): X = typeClassInstance.fold(self)(f1, f2)
  }

  trait ToFold2Ops {
    implicit def toFold2Ops[T, A1, A2](target: T)(implicit tc: Fold2[T, A1, A2]): Ops[T, A1, A2] = new Ops[T, A1, A2] {
      val self = target
      val typeclassInstance = tc
    }
  }

  object ops extends ToFold2Ops
}


trait Fold3[T, A1, A2, A3] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X): X
}

trait Fold4[T, A1, A2, A3, A4] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X,
    f4: A4 => X): X
}

trait Fold5[T, A1, A2, A3, A4, A5] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X,
    f4: A4 => X,
    f5: A5 => X): X
}

trait Fold6[T, A1, A2, A3, A4, A5, A6] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X,
    f4: A4 => X,
    f5: A5 => X,
    f6: A6 => X): X
}