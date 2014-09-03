package uk.co.turingatemyhamster.datatree

import javax.xml.namespace.QName

import uk.co.turingatemyhamster.cake.{ScalaRelations, Relations}

import scala.annotation.{StaticAnnotation, implicitNotFound}
import language.experimental.macros

/**
 * The SBOL-data datatree language.
 *
 * @author Matthew Pocock
 */
trait Datatree {
  importedPackage : Relations =>

  type Name
  type URI

  trait WithBindings {
    def bindings: ZeroMany[NamespaceBinding]
  }

  trait Document extends WithBindings {
    def identity: One[URI]
    def `type`: One[Name]
    def properties: ZeroMany[NamedProperty]
  }

  case class DocumentRoot(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                          documents: ZeroMany[TopLevelDocument] = ZeroMany())
    extends WithBindings

  case class TopLevelDocument(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                              identity: One[URI],
                              `type`: One[Name],
                              properties: ZeroMany[NamedProperty] = ZeroMany())
    extends Document

  case class NestedDocument(bindings: ZeroMany[NamespaceBinding] = ZeroMany(),
                            identity: One[URI],
                            `type`: One[Name],
                            properties: ZeroMany[NamedProperty] = ZeroMany())
    extends Document with PropertyValue

  case class NamedProperty(name: Name, propertyValue: PropertyValue)

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

  case class UriLiteral(value: URI) extends Literal {
    type Value = URI
  }

  case class TypedLiteral(value: String, xsdType: String) extends Literal {
    type Value = String
  }

  case class NamespaceBinding(prefix: String, namespaceURI: URI) {
    def withLocalName(localPart: String): Name = Name(prefix, namespaceURI, localPart)
    def uri(localPart: String): URI = URI(namespaceURI + localPart)
  }

  def URI(uri: String): URI
  def Name(prefix: String, namespaceURI: URI, localPart: String): Name
}

@implicitNotFound(
"Datatree does not know how to build [${D}]s from [${T}]s; define an implicit Builder[${T}, ${D}] to teach it how")
  trait DatatreeBuilder[T, D] {
  def build(t: T): D
}

object Datatree extends ScalaRelations with Datatree {
  override type Name = QName
  override type URI = java.net.URI

  override def URI(uri: String) = java.net.URI.create(uri)

  override def Name(prefix: String, namespaceURI: URI, localName: String) =
    new QName(namespaceURI.toString, localName, prefix)
}

class rdfType(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
class rdfProperty(prefix: String, namespaceURI: String, localPart: String) extends StaticAnnotation
