package uk.co.turingatemyhamster
package relations

import typeclass._
import simulacrum._

object RelationsDSL {
  def apply[R <: Relations : RelationsDSL]: RelationsDSL[R] = implicitly[RelationsDSL[R]]
}

/**
 *
 *
 * @author Matthew Pocock
 */
trait RelationsDSL[R <: Relations] {

  trait ZeroOneCompanion extends Companion_*[R#ZeroOne] {
    override def apply[A](as: A*): R#ZeroOne[A]
    def fromOption[A](oa: Option[A]): R#ZeroOne[A] = oa.fold[R#ZeroOne[A]](apply())(a => apply(a))
  }
  val ZeroOne: ZeroOneCompanion

  val One : Companion_*[R#One]
  val ZeroMany : Companion_*[R#ZeroMany]
  val OneMany: Companion_*[R#OneMany]
  val TwoMany: Companion_*[R#TwoMany]

  implicit val oneMethods: OneMethods[R#One]
  @typeclass trait OneMethods[R_One[_]] {
    def get[T](ot: R_One[T]): T
  }

  implicit val zeroManyMethods: ZeroManyMethods[R#ZeroMany]
  @typeclass trait ZeroManyMethods[R_ZeroMany[_]] {
    def +: [T](zm: R_ZeroMany[T], t: T): R_ZeroMany[T]
    def :+ [T](zm: R_ZeroMany[T], t: T): R_ZeroMany[T]
  }

  @typeclass trait AsSeq[S[+_]] {
    def seq[T](s: S[T]): Seq[T]
  }

  implicit val zeroOneAsSeq: AsSeq[R#ZeroOne]
  implicit val zeroManyAsSeq: AsSeq[R#ZeroMany]
  implicit val oneAsSeq: AsSeq[R#One]
  implicit val oneManyAsSeq: AsSeq[R#OneMany]
  implicit val twoManyAsSeq: AsSeq[R#TwoMany]


  implicit def liftZeroOne1[S, T](implicit ev: ConstructorChain[S, T]): ConstructorChain[S, R#ZeroOne[T]] =
    new ConstructorChain[S, R#ZeroOne[T]] {
      override def apply(s: S) = ZeroOne(s)
    }

  implicit def liftOne[S, T](implicit ev: ConstructorChain[S, T]): ConstructorChain[S, R#One[T]] =
    new ConstructorChain[S, R#One[T]] {
      override def apply(s: S) = One(s)
    }


  object Methods
    extends OneMethods.ToOneMethodsOps
            with ZeroManyMethods.ToZeroManyMethodsOps
            with AsSeq.ToAsSeqOps
}