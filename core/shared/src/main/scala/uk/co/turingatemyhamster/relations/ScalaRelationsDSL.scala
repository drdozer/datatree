package uk.co.turingatemyhamster
package relations

import typeclass._

/**
 *
 *
 * @author Matthew Pocock
 */
trait ScalaRelations extends Relations {

  override type ZeroOne[+T] = Option[T]
  override type One[+T] = T
  override type ZeroMany[+T] = Seq[T]
  override type OneMany[+T] = Seq[T]
  override type TwoMany[+T] = Seq[T]

}

object ScalaRelations {
  implicit def dsl[R <: ScalaRelations]: RelationsDSL[R] = new ScalaRelationsDSL[R] {}
}

trait ScalaRelationsDSL[R <: ScalaRelations] extends RelationsDSL[R] {

  object ZeroOne extends ZeroOneCompanion {
    override def apply[A](as: A*) = as.headOption // fixme: validate
    override def unapply[A](ta: Option[A]) = ta map (Seq apply _)
  }

  object ZeroOneBinding extends Companion_*[Option] {
    override def unapply[T](zo: Option[T]) = Some(zo.toSeq)

    override def apply[T](ts: T*) = if(ts.length > 1)
      throw new IllegalArgumentException("Can only build a ZeroOne from zero or one elements, but given " + ts.length)
    else
      ts.headOption
  }

  object One extends Companion_*[R#One] {
    override def apply[T](t: T*): R#One[T] = t.head
    override def unapply[T](t: T) = Some(Seq(t))
  }

  implicit object oneMethods extends OneMethods[R#One] {
    override def get[T](ot: R#One[T]) = ot
  }

  object ZeroMany extends Companion_*[R#ZeroMany] {
    override def apply[T](ts: T*) = ts
    override def unapply[T](zm: R#ZeroMany[T]) = Some(zm)
  }

  object OneMany extends Companion_*[R#OneMany] {
    override def apply[T](ts: T*) = if(ts.isEmpty)
      throw new IllegalArgumentException("Can only build a OneMany from one or more elements")
    else
      ts

    override def unapply[T](om: R#OneMany[T]) = Some(om)
  }

  object TwoMany extends Companion_*[R#TwoMany] {
    override def apply[T](ts: T*) = if(ts.length < 2)
      throw new IllegalArgumentException("Can only build TwoMany from two or more elements")
    else
      ts

    override def unapply[T](tm: R#TwoMany[T]) = Some(tm)
  }


  implicit object zeroManyMethods extends ZeroManyMethods[Seq] {
    override def :+[T](zm: Seq[T], t: T): Seq[T] = zm :+ t
    override def +:[T](zm: Seq[T], t: T): Seq[T] = t +: zm
  }


  implicit object zeroOneAsSeq extends AsSeq[Option] {
    override def seq[T](s: Option[T]) = s.toSeq
  }

  implicit object zeroManyAsSeq extends AsSeq[Seq] {
    override def seq[T](s: Seq[T]) = s
  }

  implicit object oneAsSeq extends AsSeq[R#One] {
    override def seq[T](s: R#One[T]) = s::Nil
  }

  implicit object oneManyAsSeq extends AsSeq[Seq] {
    override def seq[T](s: Seq[T]) = s
  }

  implicit object twoManyAsSeq extends AsSeq[Seq] {
    override def seq[T](s: Seq[T]) = s
  }
}
