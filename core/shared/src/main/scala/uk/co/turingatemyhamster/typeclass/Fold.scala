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
    def fold[X](f1: A1 => X): X = typeClassInstance.fold(self)(f1)
  }

  trait ToFold1Ops {
    implicit def toFold1Ops[T, A1](target: T)(implicit tc: Fold1[T, A1]): Ops[T, A1] = new Ops[T, A1] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object ops extends ToFold1Ops
}


trait Fold2[T, A1, A2] {
  def fold[X](t: T)(f1: A1 => X,
                    f2: A2 => X): X
}

object Fold2 {
  def apply[T, A1, A2](implicit fold: Fold2[T, A1, A2]): Fold2[T, A1, A2] = fold

  trait Ops[T, A1, A2] {
    def typeClassInstance: Fold2[T, A1, A2]
    def self: T
    def fold[X](f1: A1 => X,
                f2: A2 => X): X = typeClassInstance.fold(self)(f1, f2)
  }

  trait ToFold2Ops {
    implicit def toFold2Ops[T, A1, A2](target: T)(implicit tc: Fold2[T, A1, A2]): Ops[T, A1, A2] = new Ops[T, A1, A2] {
      val self = target
      val typeClassInstance = tc
    }
  }

  object ops extends ToFold2Ops
}


trait Fold3[T, A1, A2, A3] {
  def fold[X](t: T)(f1: A1 => X,
                    f2: A2 => X,
                    f3: A3 => X): X
}

object Fold3 {
  def apply[T, A1, A2, A3](implicit fold: Fold3[T, A1, A2, A3]): Fold3[T, A1, A2, A3] = fold

  trait Ops[T, A1, A2, A3] {
    def typeClassInstance: Fold3[T, A1, A2, A3]
    def self: T
    def fold[X](f1: A1 => X,
                f2: A2 => X,
                f3: A3 => X): X = typeClassInstance.fold(self)(f1, f2, f3)
  }

  trait ToFold3Ops {
    implicit def toFold3Ops[T, A1, A2, A3](target: T)(implicit tc: Fold3[T, A1, A2, A3])
    : Ops[T, A1, A2, A3] = new Ops[T, A1, A2, A3]
    {
      val self = target
      val typeClassInstance = tc
    }
  }
}


trait Fold4[T, A1, A2, A3, A4] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X,
    f4: A4 => X): X
}

object Fold4 {
  def apply[T, A1, A2, A3, A4](implicit fold: Fold4[T, A1, A2, A3, A4]): Fold4[T, A1, A2, A3, A4] = fold

  trait Ops[T, A1, A2, A3, A4] {
    def typeClassInstance: Fold4[T, A1, A2, A3, A4]
    def self: T
    def fold[X](f1: A1 => X,
                f2: A2 => X,
                f3: A3 => X,
                f4: A4 => X): X = typeClassInstance.fold(self)(f1, f2, f3, f4)
  }

  trait ToFold4Ops {
    implicit def toFold4Ops[T, A1, A2, A3, A4](target: T)(implicit tc: Fold4[T, A1, A2, A3, A4])
    : Ops[T, A1, A2, A3, A4] = new Ops[T, A1, A2, A3, A4]
    {
      val self = target
      val typeClassInstance = tc
    }
  }
}


trait Fold5[T, A1, A2, A3, A4, A5] {
  def fold[X](t: T)(
    f1: A1 => X,
    f2: A2 => X,
    f3: A3 => X,
    f4: A4 => X,
    f5: A5 => X): X
}

object Fold5 {
  def apply[T, A1, A2, A3, A4, A5](implicit fold: Fold5[T, A1, A2, A3, A4, A5]): Fold5[T, A1, A2, A3, A4, A5] = fold

  trait Ops[T, A1, A2, A3, A4, A5] {
    def typeClassInstance: Fold5[T, A1, A2, A3, A4, A5]
    def self: T
    def fold[X](f1: A1 => X,
                f2: A2 => X,
                f3: A3 => X,
                f4: A4 => X,
                f5: A5 => X): X = typeClassInstance.fold(self)(f1, f2, f3, f4, f5)
  }

  trait ToFold5Ops {
    implicit def toFold5Ops[T, A1, A2, A3, A4, A5](target: T)(implicit tc: Fold5[T, A1, A2, A3, A4, A5])
    : Ops[T, A1, A2, A3, A4, A5] = new Ops[T, A1, A2, A3, A4, A5]
    {
      val self = target
      val typeClassInstance = tc
    }
  }
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

object Fold6 {
  def apply[T, A1, A2, A3, A4, A5, A6](implicit fold: Fold6[T, A1, A2, A3, A4, A5, A6]): Fold6[T, A1, A2, A3, A4, A5, A6] = fold

  trait Ops[T, A1, A2, A3, A4, A5, A6] {
    def typeClassInstance: Fold6[T, A1, A2, A3, A4, A5, A6]
    def self: T
    def fold[X](f1: A1 => X,
                f2: A2 => X,
                f3: A3 => X,
                f4: A4 => X,
                f5: A5 => X,
                f6: A6 => X): X = typeClassInstance.fold(self)(f1, f2, f3, f4, f5, f6)
  }

  trait ToFold6Ops {
    implicit def toFold6Ops[T, A1, A2, A3, A4, A5, A6](target: T)(implicit tc: Fold6[T, A1, A2, A3, A4, A5, A6])
    : Ops[T, A1, A2, A3, A4, A5, A6] = new Ops[T, A1, A2, A3, A4, A5, A6]
    {
      val self = target
      val typeClassInstance = tc
    }
  }
}
