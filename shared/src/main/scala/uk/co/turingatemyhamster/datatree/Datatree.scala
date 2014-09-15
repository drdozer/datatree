package uk.co.turingatemyhamster.datatree

import uk.co.turingatemyhamster.relations.{RelationsOps, Relations}
import uk.co.turingatemyhamster.web.{WebOps, Web}

import scala.annotation.{StaticAnnotation, implicitNotFound}
import language.experimental.macros

/**
 * The SBOL-data datatree language.
 *
 * @author Matthew Pocock
 */
abstract class Datatree extends Web with Relations {
  importedPackages : RelationsOps =>

  trait WithBindings {
    def bindings: ZeroMany[NamespaceBinding]
  }

  trait Document extends WithBindings {
    def identity: One[Uri]
    def `type`: One[QName]
    def properties: ZeroMany[NamedProperty]
  }

  case class DocumentRoot(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                          documents: ZeroMany[TopLevelDocument] = ZeroMany())
    extends WithBindings

  case class TopLevelDocument(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                              identity: One[Uri],
                              `type`: One[QName],
                              properties: ZeroMany[NamedProperty] = ZeroMany())
    extends Document

  case class NestedDocument(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                            identity: One[Uri],
                            `type`: One[QName],
                            properties: ZeroMany[NamedProperty] = ZeroMany())
    extends Document with PropertyValue

  case class NamedProperty(name: One[QName],
                           propertyValue: One[PropertyValue])

  sealed trait PropertyValue

  sealed trait Literal extends PropertyValue {
    type Value
    def value: Value
  }

  case class StringLiteral(value: String) extends Literal {
    type Value = String
  }

  case class IntegerLiteral(value: Int) extends Literal {
    type Value = Int
  }

  case class DoubleLiteral(value: Double) extends Literal {
    type Value = Double
  }

  case class BooleanLiteral(value: Boolean) extends Literal {
    type Value = Boolean
  }

  case class UriLiteral(value: Uri) extends Literal {
    type Value = Uri
  }

  case class TypedLiteral(value: String, xsdType: String) extends Literal {
    type Value = String
  }
}

@implicitNotFound(
"Datatree does not know how to build [${D}]s from [${T}]s; define an implicit Builder[${T}, ${D}] to teach it how")
  trait DatatreeBuilder[T, D] {
  def build(t: T): D
}

trait DatatreeOps {
  importedPackage : Datatree
    with WebOps
    with RelationsOps =>

  trait UriApi {
    def apply(uri: String): Uri
    def unapply(uri: Uri): Option[String]
  }

  implicit class DocumentRootSyntax(_dr: DocumentRoot) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      (b.prefix == Prefix("rdf")) ||
      _dr.documents.seq.exists(_.usedBinding(b))
  }

  implicit class DocumentSyntax(_doc: Document) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      (QNameSyntax(_doc.`type`.theOne).prefix == b.prefix) ||
      _doc.properties.seq.exists(_.usedBinding(b))
  }

  implicit class NamedPropertySyntax(_np: NamedProperty) {
    def unusedBinding(b: NamespaceBinding): Boolean = ! usedBinding(b)
    def usedBinding(b: NamespaceBinding): Boolean =
      _np.name.theOne.prefix == b.prefix
  }
}


class rdfType(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
class rdfProperty(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
