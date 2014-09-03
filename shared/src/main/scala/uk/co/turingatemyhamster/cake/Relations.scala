package uk.co.turingatemyhamster.cake

/**
 * Created by caroline on 23/08/2014.
 */
trait Relations {

  type ZeroOne[T]
  type One[T]
  type ZeroMany[T]
  type OneMany[T]
  type TwoMany[T]

  def ZeroOne[T](): ZeroOne[T]
  def ZeroOne[T](t: T): ZeroOne[T]
  def ZeroOne[T](ts: T*): ZeroOne[T]

  def One[T](t: T): One[T]
  def ZeroMany[T](ts: T*): ZeroMany[T]
  def OneMany[T](ts: T*): OneMany[T]
  def TwoMany[T](ts: T*): TwoMany[T]

  implicit def zeroOneOps[T]: ZeroOneOps[T]

  trait ZeroOneOps[T] {
    def seq(zo: ZeroOne[T]): Seq[T]
  }

  implicit class ZeroOneSyntax[T](val _zo: ZeroOne[T])(implicit ops: ZeroOneOps[T]) {
    def seq = ops.seq(_zo)
  }

  implicit def oneOps[T]: OneOps[T]

  trait OneOps[T] {
    def theOne(ot: One[T]): T
    def seq(ot: One[T]): Seq[T]
  }

  implicit class OneSyntax[T](val _ot: One[T])(implicit ops: OneOps[T]) {
    def theOne: T = ops.theOne(_ot)
    def seq: Seq[T] = ops.seq(_ot)
  }

  implicit def zeroManyOps[T]: ZeroManyOps[T]

  trait ZeroManyOps[T] {
    def seq(zm: ZeroMany[T]): Seq[T]
    def +: (zm: ZeroMany[T], t: T): ZeroMany[T]
    def :+ (zm: ZeroMany[T], t: T): ZeroMany[T]
  }

  implicit class ZeroManySyntax[T](val _zmt: ZeroMany[T])(implicit ops: ZeroManyOps[T]) {
    def seq = ops.seq(_zmt)
    def +: (t: T) = ops.+:(_zmt, t)
    def :+ (t: T) = ops.:+(_zmt, t)
  }

  implicit def oneManyOps[T]: OneManyOps[T]

  trait OneManyOps[T] {
    def seq(om: OneMany[T]): Seq[T]
  }

  implicit class OneManySyntax[T](val _omt: OneMany[T])(implicit ops: OneManyOps[T]) {
    def seq = ops.seq(_omt)
  }

  implicit def twoManyOps[T]: TwoManyOps[T]

  trait TwoManyOps[T] {
    def seq(tm: TwoMany[T]): Seq[T]
  }

  implicit class TwoManySyntax[T](val _tmt: TwoMany[T])(implicit ops: TwoManyOps[T]) {
    def seq = ops.seq(_tmt)
  }
}

object Relations {

  def implicits[R1 <: Relations, R2 <: Relations](r1: R1, r2: R2) = new Implicits(r1, r2)
//    implicit def one[T](o: R1#One[T]): R2#One[T]
//    implicit def zeroMany[T](zm: R1#ZeroMany[T]): R2#ZeroMany[T]
//    implicit def oneMany[T](om: R1#OneMany[T]): R2#OneMany[T]
//    implicit def twoMany[T](tm: R1#TwoMany[T]): R2#TwoMany[T]


  class Implicits[R1 <: Relations, R2 <: Relations](r1: R1, val r2: R2) {
    import r1._
    implicit def zeroOne[T](zo: r1.ZeroOne[T]): r2.ZeroOne[T] = r2.ZeroOne(zo.seq :_*)
  }
}
