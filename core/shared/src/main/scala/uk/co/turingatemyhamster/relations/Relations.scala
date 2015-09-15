package uk.co.turingatemyhamster.relations

import scala.language.{higherKinds, implicitConversions}

/**
 * Created by caroline on 23/08/2014.
 */
trait Relations {

  type ZeroOne[+T]
  type One[+T]
  type ZeroMany[+T]
  type OneMany[+T]
  type TwoMany[+T]

}

object Relations2Relations {

  def ops[R1 <: Relations : RelationsDSL, R2 <: Relations : RelationsDSL]: Relations2Relations[R1, R2] =
    new Relations2Relations[R1, R2] {
      override final val r1: RelationsDSL[R1] = implicitly[RelationsDSL[R1]]
      override final val r2: RelationsDSL[R2] = implicitly[RelationsDSL[R2]]
    }

}

trait Relations2Relations[R1 <: Relations, R2 <: Relations] {
  val r1: RelationsDSL[R1]
  val r2: RelationsDSL[R2]


  implicit def zeroOne12[S, T](implicit st: S => T): R1#ZeroOne[S] => R2#ZeroOne[T] =
  {
    case r1.ZeroOne(s) => r2.ZeroOne(s.toSeq map st : _*)
  }

  implicit def one12[S, T](implicit st: S => T): R1#One[S] => R2#One[T] =
  {
    case r1.One(s) => r2.One(s.toSeq map st: _*)
  }

  implicit def zeroMany12[S, T](implicit st: S => T): R1#ZeroMany[S] => R2#ZeroMany[T] =
  {
    case r1.ZeroMany(s) => r2.ZeroMany(s map st : _*)
  }

  implicit def oneMany12[S, T](implicit st: S => T): R1#OneMany[S] => R2#OneMany[T] =
  {
    case r1.OneMany(s) => r2.OneMany(s map st : _*)
  }

  implicit def twoMany12[S, T](implicit st: S => T): R1#TwoMany[S] => R2#TwoMany[T] =
  {
    case r1.TwoMany(s) => r2.TwoMany(s map st : _*)
  }
}