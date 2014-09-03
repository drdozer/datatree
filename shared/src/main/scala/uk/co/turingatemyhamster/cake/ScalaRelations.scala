package uk.co.turingatemyhamster.cake

/**
 * Default scala type bindings for Relations.
 *
 * @author Matthew Pocock
 */
trait ScalaRelations extends Relations {

  override type ZeroOne[T] = Option[T]
  override type One[T] = T
  override type ZeroMany[T] = Seq[T]
  override type TwoMany[T] = Seq[T]
  override type OneMany[T] = Seq[T]

  override def ZeroOne[T]() = None

  override def ZeroOne[T](t: T) = Some(t)

  override def ZeroOne[T](ts: T*) = ts.headOption

  override def One[T](t: T) = t

  override def ZeroMany[T](ts: T*) = ts

  override def TwoMany[T](ts: T*) = ts

  override def OneMany[T](ts: T*) = ts

  override implicit def zeroOneOps[T] = new ZeroOneOps[T] {
    override def seq(zo: ZeroOne[T]) = zo.to[List]
  }

  implicit def oneOps[T] = new OneOps[T] {
    override def theOne(ot: One[T]) = ot
    override def seq(ot: One[T]) = ot::Nil
  }

  override implicit def zeroManyOps[T] = new ZeroManyOps[T] {
    override def seq(zm: ZeroMany[T]) = zm
  }

  override implicit def oneManyOps[T] = new OneManyOps[T] {
    override def seq(om: OneMany[T]) = om
  }

  override implicit def twoManyOps[T] = new TwoManyOps[T] {
    override def seq(tm: TwoMany[T]) = tm
  }
}
