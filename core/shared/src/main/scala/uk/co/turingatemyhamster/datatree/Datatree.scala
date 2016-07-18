package uk.co.turingatemyhamster
package datatree

import typeclass._
import relations._
import web._

import simulacrum._

/**
 * The SBOL-data datatree language.
 *
 * @author Matthew Pocock
 */
trait Datatree extends Web with Relations {

  type WithBindings
  type PropertyValue
  type Document <: WithBindings
  type DocumentRoot <: WithBindings
  type TopLevelDocument <: Document
  type NestedDocument <: Document with PropertyValue
  type NamedProperty <: WithBindings
  type Literal <: PropertyValue
  type LongLiteral <: Literal
  type DoubleLiteral <: Literal
  type BooleanLiteral <: Literal
  type UriLiteral <: Literal
  type TypedLiteral <: Literal

}

trait DatatreeDSL[DT <: Datatree] {
  protected val relationsDSL: RelationsDSL[DT]
  import relationsDSL._


  trait WithBindingsDestructor[DT_WithBindings] extends Destructor1[DT_WithBindings] {
    type A = DT#ZeroMany[DT#NamespaceBinding]
  }
  implicit def WithBindings[DT_WithBindings <: DT#WithBindings]: WithBindingsDestructor[DT_WithBindings]

  implicit def withBindingsMembers[DT_WithBindings <: DT#WithBindings]: WithBindingsMembers[DT_WithBindings]
  @typeclass trait WithBindingsMembers[DT_WithBindings] {
    def bindings(wb: DT_WithBindings): DT#ZeroMany[DT#NamespaceBinding]
  }

  implicit def withBindingsFold: Fold3[DT#WithBindings,
    DT#Document, DT#DocumentRoot, DT#NamedProperty]


  implicit def propertyValueFold: Fold2[DT#PropertyValue,
    DT#NestedDocument, DT#Literal]


  implicit def documentMembers[DT_Document <: DT#Document]: DocumentMembers[DT_Document]
  @typeclass trait DocumentMembers[DT_Document] {
    def identity(document: DT_Document): DT#ZeroOne[DT#Uri]
    def `type`(document: DT_Document): DT#One[DT#QName]
    def properties(document: DT_Document): DT#ZeroMany[DT#NamedProperty]
  }

  implicit def documentFold: Fold2[DT#Document,
    DT#TopLevelDocument, DT#NestedDocument]


  trait DocumentRootCompanion extends Companion2[
    DT#DocumentRoot] {
    type A = DT#ZeroMany[DT#NamespaceBinding]
    type B = DT#ZeroMany[DT#TopLevelDocument]

    override def apply(bindings: DT#ZeroMany[DT#NamespaceBinding] = ZeroMany(),
                       documents: DT#ZeroMany[DT#TopLevelDocument]): DT#DocumentRoot
  }
  implicit def DocumentRoot: DocumentRootCompanion

  implicit def documentRootMembers: DocumentRootMembers[DT#DocumentRoot]
  @typeclass trait DocumentRootMembers[DT_DocumentRoot] {
    def documents(documentRoot: DT_DocumentRoot): DT#ZeroMany[DT#TopLevelDocument]
  }


  trait DocumentCompanion[DocumentType] extends Companion4[DocumentType] {
    type A = DT#ZeroMany[DT#NamespaceBinding]
    type B = DT#ZeroOne[DT#Uri]
    type C = DT#One[DT#QName]
    type D = DT#ZeroMany[DT#NamedProperty]

    override def apply(bindings: DT#ZeroMany[DT#NamespaceBinding],
                       identity: DT#ZeroOne[DT#Uri],
                       `type`: DT#One[DT#QName],
                       properties: DT#ZeroMany[DT#NamedProperty]): DocumentType
//
//    def apply[
//    I : Cstr[DT#ZeroOne[DT#Uri]]#λ,
//    T : Cstr[DT#One[DT#QName]]#λ](bindings: DT#ZeroMany[DT#NamespaceBinding] = ZeroMany(),
//                                  identity: I,
//                                  `type`: T,
//                                  properties: DT#ZeroMany[DT#NamedProperty] = ZeroMany()
//                                   ): DocumentType =
//      apply(bindings, identity : DT#ZeroOne[DT#Uri], `type` : DT#One[DT#QName], properties)
//
//    def apply[T : Cstr[DT#One[DT#QName]]#λ](`type`: T,
//                                            properties: DT#ZeroMany[DT#NamedProperty]
//                                             ): DocumentType =
//      apply(ZeroMany(), ZeroOne(), `type` : DT#One[DT#QName], properties)
  }


  implicit def TopLevelDocument: DocumentCompanion[DT#TopLevelDocument]

  implicit def NestedDocument: DocumentCompanion[DT#NestedDocument]

  trait NamedPropertyCompanion extends Companion3[DT#NamedProperty] {
    type A = DT#ZeroMany[DT#NamespaceBinding]
    type B = DT#One[DT#QName]
    type C = DT#One[DT#PropertyValue]

    def apply(bindings: DT#ZeroMany[DT#NamespaceBinding],
              name: DT#One[DT#QName],
              value: DT#One[DT#PropertyValue]): DT#NamedProperty
    def apply[N, V](bindings: DT#ZeroMany[DT#NamespaceBinding] = relationsDSL.ZeroMany(),
                    name: N,
                    value: V)(implicit
                              nN: ConstructorChain[N, DT#One[DT#QName]],
                              vV: ConstructorChain[V, DT#One[DT#PropertyValue]]): DT#NamedProperty =
      apply(bindings, name: DT#One[DT#QName], value: DT#One[DT#PropertyValue])
  }

  implicit def NamedProperty: NamedPropertyCompanion

  implicit def namedPropertyMembers: NamedPropertyMembers[DT#NamedProperty]
  @typeclass trait NamedPropertyMembers[DT_NamedProperty] {
    def name(np: DT_NamedProperty): DT#One[DT#QName]
    def value(np: DT_NamedProperty): DT#One[DT#PropertyValue]
  }

//
//  implicit def Literal[DT_Literal <: DT#Literal]: LiteralDestructor[DT_Literal]
//  trait LiteralDestructor[DT_Literal] extends Destructor1[DT_Literal] { type A = Any }

  implicit def literalFold: Fold5[DT#Literal,
    DT#LongLiteral, DT#DoubleLiteral, DT#BooleanLiteral, DT#UriLiteral, DT#TypedLiteral]


  trait LongLiteralCompanion extends Companion1[DT#LongLiteral] {
    type A = Long
    override def apply(value: A): DT#LongLiteral
  }
  implicit def LongLiteral: LongLiteralCompanion

  implicit def longLiteralMembers: LongLiteralMembers[DT#LongLiteral]
  @typeclass trait LongLiteralMembers[DT_LongLiteral] {
    def value(ll: DT_LongLiteral): Long
  }


  trait DoubleLiteralCompanion extends Companion1[DT#DoubleLiteral] {
    type A = Double
    override def apply(value: A): DT#DoubleLiteral
  }
  implicit def DoubleLiteral: DoubleLiteralCompanion

  implicit def doubleLiteralMembers: DoubleLiteralMembers[DT#DoubleLiteral]
  @typeclass trait DoubleLiteralMembers[DT_DoubleLiteral] {
    def value(dl: DT_DoubleLiteral): Double
  }


  trait BooleanLiteralCompanion extends Companion1[DT#BooleanLiteral] {
    type A = Boolean
  }
  implicit def BooleanLiteral: BooleanLiteralCompanion

  implicit def booleanLiteralMembers: BooleanLiteralMembers[DT#BooleanLiteral]
  @typeclass trait BooleanLiteralMembers[DT_BooleanLiteral] {
    def value(bl: DT_BooleanLiteral): Boolean
  }


  trait UriLiteralCompanion extends Companion1[DT#UriLiteral] {
    type A = DT#Uri

    override def apply(value: DT#Uri): DT#UriLiteral
    def apply[V : Cstr[DT#Uri]#λ](value: V): DT#UriLiteral = apply(value : DT#Uri)
  }
  implicit def UriLiteral: UriLiteralCompanion

  implicit def uriLiteralMembers: UriLiteralMembers[DT#UriLiteral]
  @typeclass trait UriLiteralMembers[DT_UriLiteral] {
    def value(ul: DT_UriLiteral): DT#Uri
  }


  trait TypedLiteralCompanion extends Companion3[DT#TypedLiteral] {
    type A = DT#One[String]
    type B = DT#ZeroOne[DT#Uri]
    type C = DT#ZeroOne[String]

    override def apply(value: A, valueType: B, lang: C): DT#TypedLiteral
  }
  implicit def TypedLiteral: TypedLiteralCompanion

  implicit def typedLiteralMembers: TypedLiteralMembers[DT#TypedLiteral]
  @typeclass trait TypedLiteralMembers[DT_TypedLiteral] {
    def value(tl: DT_TypedLiteral): DT#One[String]
    def valueType(tl: DT_TypedLiteral): DT#ZeroOne[DT#Uri]
    def lang(tl: DT_TypedLiteral): DT#ZeroOne[String]
  }

  trait TypedLiteralSCompanion extends Companion1[DT#TypedLiteral] {
    type A = String
  }
  implicit def TypedLiteralS: TypedLiteralSCompanion

  object Syntax {

    implicit class NPSyntax[N](val _n: N) {
      def := [V](v: V)(implicit nN: ConstructorChain[N, DT#One[DT#QName]], vV: ConstructorChain[V, DT#One[DT#PropertyValue]]) =
        NamedProperty(name = _n, value = v)
    }

  }

  object Members extends WithBindingsMembers.ToWithBindingsMembersOps
                         with DocumentMembers.ToDocumentMembersOps
                         with DocumentRootMembers.ToDocumentRootMembersOps
                         with NamedPropertyMembers.ToNamedPropertyMembersOps
                         with LongLiteralMembers.ToLongLiteralMembersOps
                         with DoubleLiteralMembers.ToDoubleLiteralMembersOps
                         with BooleanLiteralMembers.ToBooleanLiteralMembersOps
                         with UriLiteralMembers.ToUriLiteralMembersOps
                         with TypedLiteralMembers.ToTypedLiteralMembersOps

  object SI5070 {
    implicit val propertyValueFromString: ConstructorChain[String, DT#PropertyValue] =
      ConstructorChain.castT[String, DT#TypedLiteral, DT#PropertyValue]

    implicit val propertyValueFromUri: ConstructorChain[DT#Uri, DT#PropertyValue] =
      ConstructorChain.castT[DT#Uri, DT#UriLiteral, DT#PropertyValue]

    implicit val propertyValueFromLong: ConstructorChain[Long, DT#PropertyValue] =
      ConstructorChain.castT[Long, DT#LongLiteral, DT#PropertyValue]
  }
}


// fixme: is this important for something?
/*
class rdfType(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
class rdfProperty(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
*/