package uk.co.turingatemyhamster.relations

/**
 * Created by caroline on 23/08/2014.
 */
trait Relations {

  type ZeroOne[T]
  type One[T]
  type ZeroMany[T]
  type OneMany[T]
  type TwoMany[T]

}

trait RelationsOps {
  importedPackages : Relations =>

  val ZeroOne : ZeroOneApi

  trait ZeroOneApi {
    def fromOption[T](ot: Option[T]): ZeroOne[T]

    def apply[T](ts: T*): ZeroOne[T]

    def unapply[T](zo: ZeroOne[T]): Option[Option[T]]
  }

  implicit def zeroOneOps[T]: ZeroOneOps[T]

  trait ZeroOneOps[T] {
    def seq(zo: ZeroOne[T]): Seq[T]
  }

  implicit class ZeroOneSyntax[T](val _zo: ZeroOne[T])(implicit ops: ZeroOneOps[T]) {
    def seq = ops.seq(_zo)
  }



  val One : OneApi

  trait OneApi {
    def apply[T](t: T*): One[T]
    def unapply[T](o: One[T]): Option[Option[T]]
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



  val ZeroMany : ZeroManyApi

  trait ZeroManyApi {
    def apply[T](ts: T*): ZeroMany[T]
    def unapply[T](zm: ZeroMany[T]): Option[Seq[T]]
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



  val OneMany : OneManyApi

  trait OneManyApi {
    def apply[T](ts: T*): OneMany[T]
    def unapply[T](om: OneMany[T]): Option[Seq[T]]
  }

  implicit def oneManyOps[T]: OneManyOps[T]

  trait OneManyOps[T] {
    def seq(om: OneMany[T]): Seq[T]
  }

  implicit class OneManySyntax[T](val _omt: OneMany[T])(implicit ops: OneManyOps[T]) {
    def seq = ops.seq(_omt)
  }



  val TwoMany : TwoManyApi

  trait TwoManyApi {
    def apply[T](ts: T*): TwoMany[T]
    def unapply[T](tm: TwoMany[T]): Option[Seq[T]]
  }

  implicit def twoManyOps[T]: TwoManyOps[T]

  trait TwoManyOps[T] {
    def seq(tm: TwoMany[T]): Seq[T]
  }

  implicit class TwoManySyntax[T](val _tmt: TwoMany[T])(implicit ops: TwoManyOps[T]) {
    def seq = ops.seq(_tmt)
  }
}

trait RelationsOpsScalaImpl extends RelationsOps {
  importedPackages : Relations =>

  override type ZeroOne[T] = Option[T]
  override type One[T] = T
  override type ZeroMany[T] = Seq[T]
  override type OneMany[T] = Seq[T]
  override type TwoMany[T] = Seq[T]


  override val ZeroOne: ZeroOneApi = new ZeroOneApi {
    override def fromOption[T](ot: Option[T]) = ot

    override def unapply[T](zo: ZeroOne[T]) = Some(zo)

    override def apply[T](ts: T*) = if(ts.length > 1)
      throw new IllegalArgumentException("Can only build a ZeroOne from zero or one elements, but given " + ts.length)
    else
      ts.headOption
  }

  override implicit def zeroOneOps[T]: ZeroOneOps[T] = new ZeroOneOps[T] {
    override def seq(zo: ZeroOne[T]): Seq[T] = zo.toSeq
  }

  override val One: OneApi = new OneApi {
    override def apply[T](t: T*): One[T] = t.head

    override def unapply[T](t: T) = Some(Some(t))
  }

  override implicit def oneOps[T]: OneOps[T] = new OneOps[T] {
    override def theOne(ot: One[T]) = ot

    override def seq(ot: One[T]): Seq[T] = List(ot)
  }

  override val ZeroMany: ZeroManyApi = new ZeroManyApi {
    override def apply[T](ts: T*) = ts

    override def unapply[T](zm: ZeroMany[T]) = Some(zm)
  }

  override implicit def zeroManyOps[T]: ZeroManyOps[T] = new ZeroManyOps[T] {
    override def seq(zm: ZeroMany[T]): Seq[T] = zm

    override def :+(zm: ZeroMany[T], t: T): ZeroMany[T] = zm :+ t

    override def +:(zm: ZeroMany[T], t: T): ZeroMany[T] = t +: zm
  }

  override val OneMany: OneManyApi = new OneManyApi {
    override def apply[T](ts: T*) = if(ts.isEmpty)
      throw new IllegalArgumentException("Can only build a OneMany from one or more elements")
    else
      ts

    override def unapply[T](om: OneMany[T]) = Some(om)
  }

  override implicit def oneManyOps[T]: OneManyOps[T] = new OneManyOps[T] {
    override def seq(om: OneMany[T]) = om
  }

  override val TwoMany: TwoManyApi = new TwoManyApi {
    override def apply[T](ts: T*) = if(ts.length < 2)
      throw new IllegalArgumentException("Can only build TwoMany from two or more elements")
    else
      ts

    override def unapply[T](tm: TwoMany[T]) = Some(tm)
  }

  override implicit def twoManyOps[T]: TwoManyOps[T] = new TwoManyOps[T] {
    override def seq(tm: TwoMany[T]) = tm
  }
}

object Relations2Relations {
  def apply[R1 <: Relations with RelationsOps, R2 <: Relations with RelationsOps](r1: R1, r2: R2) = new Object {
    implicit def zeroOne12[S, T](implicit st: S => T): r1.ZeroOne[S] => r2.ZeroOne[T] =
    { case r1.ZeroOne(s) => r2.ZeroOne((s.toSeq map st) : _*)}

    implicit def one12[S, T](implicit st: S => T): r1.One[S] => r2.One[T] =
    { case r1.One(s) => r2.One((s.toSeq map st) : _*)}

    implicit def zeroMany12[S, T](implicit st: S => T): r1.ZeroMany[S] => r2.ZeroMany[T] =
    { case r1.ZeroMany(s) => r2.ZeroMany((s map st) : _*)}

    implicit def oneMany12[S, T](implicit st: S => T): r1.OneMany[S] => r2.OneMany[T] =
    { case r1.OneMany(s) => r2.OneMany((s map st) : _*)}

    implicit def twoMany12[S, T](implicit st: S => T): r1.TwoMany[S] => r2.TwoMany[T] =
    { case r1.TwoMany(s) => r2.TwoMany((s map st) : _*)}
  }
}