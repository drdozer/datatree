package uk.co.turingatemyhamster
package datatree
package ast

import typeclass._
import uk.co.turingatemyhamster.relations.{RelationsDSL, ScalaRelations}
import uk.co.turingatemyhamster.web.ast.{Uri, AstWeb}

import scala.language.higherKinds

/**
 *
 *
 * @author Matthew Pocock
 */
trait AstDatatree extends Datatree with AstWeb with ScalaRelations {

  override final type WithBindings = ast.WithBindings
  override final type PropertyValue = ast.PropertyValue
  override final type Document = ast.Document
  override final type DocumentRoot = ast.DocumentRoot
  override final type TopLevelDocument = ast.TopLevelDocument
  override final type NestedDocument = ast.NestedDocument
  override final type NamedProperty = ast.NamedProperty
  override final type Literal = ast.Literal
  override final type StringLiteral = ast.StringLiteral
  override final type LongLiteral = ast.LongLiteral
  override final type DoubleLiteral = ast.DoubleLiteral
  override final type BooleanLiteral = ast.BooleanLiteral
  override final type UriLiteral = ast.UriLiteral[Uri]
  override final type TypedLiteral = ast.TypedLiteral

}


object AstDatatree {
  implicit object dsl extends DatatreeDSL[AstDatatree]{

    override protected val relationsDSL: RelationsDSL[AstDatatree] = implicitly[RelationsDSL[AstDatatree]]


    override implicit def WithBindings[DT_WithBindings <: AstDatatree#WithBindings]: WithBindingsDestructor[DT_WithBindings] =
      new WithBindingsDestructor[DT_WithBindings] {
        override def unapply(t: DT_WithBindings) = Some(t.bindings)
      }

    implicit def withBindingsMembers[Ast_WithBindings <: AstDatatree#WithBindings]: WithBindingsMembers[Ast_WithBindings] =
      new WithBindingsMembers[Ast_WithBindings] {
        override def bindings(wb: Ast_WithBindings) = wb.bindings
      }

    implicit object withBindingsFold extends Fold3[AstDatatree#WithBindings, AstDatatree#Document, AstDatatree#DocumentRoot, AstDatatree#NamedProperty] {
      override def fold[A](wb: AstDatatree#WithBindings)
                          (doc: (AstDatatree#Document) => A,
                           root: (AstDatatree#DocumentRoot) => A,
                           prop: (AstDatatree#NamedProperty) => A): A = wb match {
        case d: ast.Document =>
          doc(d)
        case r : ast.DocumentRoot =>
          root(r)
        case p : ast.NamedProperty =>
          prop(p)
      }
    }

    implicit object propertyValueFold extends Fold2[AstDatatree#PropertyValue, AstDatatree#NestedDocument, AstDatatree#Literal] {
    override def fold[A](pv: AstDatatree#PropertyValue)(
      nd: (AstDatatree#NestedDocument) => A,
      lit: (AstDatatree#Literal) => A): A = pv match {
        case n : ast.NestedDocument =>
          nd(n)
        case l : ast.Literal =>
          lit(l)
      }
    }


    implicit def documentMembers[Ast_Document <: AstDatatree#Document]: DocumentMembers[Ast_Document] =
      new DocumentMembers[Ast_Document] {
        override def identity(document: Ast_Document) = document.identity
        override def `type`(document: Ast_Document) = document.`type`
        override def properties(document: Ast_Document) = document.properties
      }

    implicit object documentFold extends Fold2[AstDatatree#Document, AstDatatree#TopLevelDocument, AstDatatree#NestedDocument] {
      override def fold[A](doc: AstDatatree#Document)
                          (topLevel: (AstDatatree#TopLevelDocument) => A,
                           nested: (AstDatatree#NestedDocument) => A): A =
      {
        doc match {
          case tld : ast.TopLevelDocument =>
            topLevel(tld)
          case nd : ast.NestedDocument =>
            nested(nd)
        }
      }
    }


    implicit object DocumentRoot extends DocumentRootCompanion {
      override def apply(a: AstDatatree#ZeroMany[AstDatatree#NamespaceBinding],
                         b: AstDatatree#ZeroMany[AstDatatree#TopLevelDocument]) =
        ast.DocumentRoot.apply(a, b)

      override def unapply(t: AstDatatree#DocumentRoot) =
        ast.DocumentRoot.unapply(t)
    }

    implicit object documentRootMembers extends DocumentRootMembers[AstDatatree#DocumentRoot] {
      override def documents(documentRoot: AstDatatree#DocumentRoot) = documentRoot.documents
    }


    implicit object TopLevelDocument extends DocumentCompanion[AstDatatree#TopLevelDocument]
    {
      override def apply(a: AstDatatree#ZeroMany[AstDatatree#NamespaceBinding],
                         b: AstDatatree#ZeroOne[AstDatatree#Uri],
                         c: AstDatatree#One[AstDatatree#QName],
                         d: AstDatatree#ZeroMany[AstDatatree#NamedProperty]) =
        ast.TopLevelDocument.apply(a, b, c, d)

      override def unapply(t: AstDatatree#TopLevelDocument) =
        ast.TopLevelDocument.unapply(t)
    }

    implicit object NestedDocument extends DocumentCompanion[AstDatatree#NestedDocument]
    {
      override def apply(a: AstDatatree#ZeroMany[AstDatatree#NamespaceBinding],
                         b: AstDatatree#ZeroOne[AstDatatree#Uri],
                         c: AstDatatree#One[AstDatatree#QName],
                         d: AstDatatree#ZeroMany[AstDatatree#NamedProperty]) =
        ast.NestedDocument.apply(a, b, c, d)

      override def unapply(t: AstDatatree#NestedDocument) =
        ast.NestedDocument.unapply(t)
    }

    
    implicit object NamedProperty extends NamedPropertyCompanion
    {
      override def apply(a: AstDatatree#ZeroMany[AstDatatree#NamespaceBinding],
                        b: AstDatatree#One[AstDatatree#QName],
                        c: AstDatatree#One[AstDatatree#PropertyValue]) =
        ast.NamedProperty.apply(a, b, c)

      override def unapply(t: AstDatatree#NamedProperty) =
        ast.NamedProperty.unapply(t)
    }
    
    implicit object namedPropertyMembers extends NamedPropertyMembers[AstDatatree#NamedProperty] {
      override def name(np: AstDatatree#NamedProperty) = np.name

      override def value(np: AstDatatree#NamedProperty) = np.value
    }


    implicit override def literalFold: Fold6[AstDatatree#Literal, AstDatatree#StringLiteral, AstDatatree#LongLiteral, AstDatatree#DoubleLiteral, AstDatatree#BooleanLiteral, AstDatatree#UriLiteral, AstDatatree#TypedLiteral] =
      new Fold6[AstDatatree#Literal, AstDatatree#StringLiteral, AstDatatree#LongLiteral, AstDatatree#DoubleLiteral, AstDatatree#BooleanLiteral, AstDatatree#UriLiteral, AstDatatree#TypedLiteral] {
        override def fold[A](lit: AstDatatree#Literal)
                            (sLit: (AstDatatree#StringLiteral) => A,
                             lLit: (AstDatatree#LongLiteral) => A,
                             dLit: (AstDatatree#DoubleLiteral) => A,
                             bLit: (AstDatatree#BooleanLiteral) => A,
                             uLit: (AstDatatree#UriLiteral) => A,
                             tLit: (AstDatatree#TypedLiteral) => A): A = lit match {
          case s : StringLiteral =>
            sLit(s)
          case l : LongLiteral =>
            lLit(l)
          case d : DoubleLiteral =>
            dLit(d)
          case b : BooleanLiteral =>
            bLit(b)
          case u : UriLiteral[AstDatatree#Uri] =>
            uLit(u)
          case t : TypedLiteral =>
            tLit(t)
        }
      }


    implicit object StringLiteral extends StringLiteralCompanion {
      override def apply(a: String) = ast.StringLiteral.apply(a)
      override def unapply(t: AstDatatree#StringLiteral) = ast.StringLiteral.unapply(t)
    }

    implicit object stringLiteralMembers extends StringLiteralMembers[AstDatatree#StringLiteral] {
      override def value(sl: StringLiteral) = sl.value
    }


    implicit object LongLiteral extends LongLiteralCompanion {
      override def apply(a: Long) = ast.LongLiteral.apply(a)
      override def unapply(t: AstDatatree#LongLiteral) = ast.LongLiteral.unapply(t)
    }

    implicit object longLiteralMembers extends LongLiteralMembers[AstDatatree#LongLiteral] {
      override def value(ll: LongLiteral) = ll.value
    }


    implicit object DoubleLiteral extends DoubleLiteralCompanion {
      override def apply(a: Double) = ast.DoubleLiteral.apply(a)
      override def unapply(t: AstDatatree#DoubleLiteral) = ast.DoubleLiteral.unapply(t)
    }

    implicit object doubleLiteralMembers extends DoubleLiteralMembers[AstDatatree#DoubleLiteral] {
      override def value(dl: DoubleLiteral) = dl.value
    }


    implicit object BooleanLiteral extends BooleanLiteralCompanion {
      override def apply(a: Boolean) = ast.BooleanLiteral.apply(a)
      override def unapply(t: AstDatatree#BooleanLiteral) = ast.BooleanLiteral.unapply(t)
    }

    implicit object booleanLiteralMembers extends BooleanLiteralMembers[AstDatatree#BooleanLiteral] {
      override def value(bl: BooleanLiteral) = bl.value
    }


    implicit object UriLiteral extends UriLiteralCompanion {
      override def apply(a: AstDatatree#Uri) = ast.UriLiteral.apply(a)
      override def unapply(t: AstDatatree#UriLiteral) = ast.UriLiteral.unapply(t)
    }

    implicit object uriLiteralMembers extends UriLiteralMembers[AstDatatree#UriLiteral] {
      override def value(ul: UriLiteral[Uri]) = ul.value
    }


    implicit object TypedLiteral extends TypedLiteralCompanion {
      override def apply(a: String, b: Uri) = ast.TypedLiteral.apply(a, b)
      override def unapply(t: AstDatatree#TypedLiteral) = ast.TypedLiteral.unapply(t)
    }

    implicit object typedLiteralMembers extends TypedLiteralMembers[AstDatatree#TypedLiteral] {
      override def value(tl: TypedLiteral) = tl.value

      override def valueType(tl: TypedLiteral) = tl.xsdType
    }
  }
}


trait WithBindings {
  def bindings: Seq[AstDatatree#NamespaceBinding]
}


sealed trait PropertyValue


trait Document
  extends WithBindings
{
  def identity: Option[AstDatatree#Uri]
  def `type`: AstDatatree#QName
  def properties: Seq[NamedProperty]
}


case class DocumentRoot(bindings: Seq[AstDatatree#NamespaceBinding],
                        documents: Seq[TopLevelDocument])
  extends WithBindings


case class TopLevelDocument(bindings: Seq[AstDatatree#NamespaceBinding],
                            identity: Option[AstDatatree#Uri],
                            `type`: AstDatatree#QName,
                            properties: Seq[NamedProperty])
  extends Document


case class NestedDocument(bindings: Seq[AstDatatree#NamespaceBinding],
                          identity: Option[AstDatatree#Uri],
                          `type`: AstDatatree#QName,
                          properties: Seq[NamedProperty])
  extends Document with PropertyValue


case class NamedProperty(bindings: Seq[AstDatatree#NamespaceBinding],
                         name: AstDatatree#QName,
                         value: PropertyValue)
  extends WithBindings


sealed trait Literal extends PropertyValue {
  type Value
  def value: Value
}

case class StringLiteral(value: String) extends Literal {
  type Value = String
}

case class LongLiteral(value: Long) extends Literal {
  type Value = Long
}

case class DoubleLiteral(value: Double) extends Literal {
  type Value = Double
}

case class BooleanLiteral(value: Boolean) extends Literal {
  type Value = Boolean
}

case class UriLiteral[Uri](value: Uri) extends Literal {
  type Value = Uri
}

case class TypedLiteral(value: String, xsdType: Uri) extends Literal {
  type Value = String
}


/*
trait DatatreeOps {
  importedPackage : Datatree
    with WebOps
    with RelationsOps =>

  implicit class DocumentRootSyntax(_dr: DocumentRoot) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      (b.prefix == Prefix("rdf")) ||
      _dr.documents.seq.exists(_.usedBinding(b))
  }

  implicit class DocumentSyntax(_doc: Document) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      (b.prefix == Prefix("rdf")) ||
      (QNameSyntax(_doc.`type`.theOne).prefix == b.prefix) ||
      _doc.properties.seq.exists(_.usedBinding(b))
  }

  implicit class NamedPropertySyntax(_np: NamedProperty) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      (b.prefix == Prefix("rdf")) ||
      _np.name.theOne.prefix == b.prefix || _np.propertyValue.theOne.usedBinding(b)
  }

  implicit class PropertyValueSyntax(_pv: PropertyValue) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean = _pv match {
      case nd : NestedDocument =>
        (nd: Document).usedBinding(b)
      case _ => false
    }
  }

 */