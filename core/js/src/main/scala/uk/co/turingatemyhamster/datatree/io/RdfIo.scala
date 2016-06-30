package uk.co.turingatemyhamster
package datatree
package io

import datatree.{Datatree, DatatreeDSL}
import relations.RelationsDSL
import web.{JQName, RdfConstants, WebDSL}
import typeclass._

import scalatags.{generic => sg}
import scalatags.text.Builder

/**
  *
  *
  * @author Matthew Pocock
  */
object RdfIo {
  implicit def rdfIo[DT <: Datatree](implicit
                                     _ioConstants: RdfConstants[DT],
                                     _webDSL: WebDSL[DT],
                                     _jQName: JQName[DT],
                                     _relationsDSL: RelationsDSL[DT],
                                     _datatreeDSL: DatatreeDSL[DT]) = new RdfIo[DT]
  {
    override protected val ioConstants: RdfConstants[DT] = _ioConstants
    override protected val jQName: JQName[DT] = _jQName
    override protected val webDSL: WebDSL[DT] = _webDSL
    override protected val relationsDSL: RelationsDSL[DT] = _relationsDSL
    override protected val datatreeDSL: DatatreeDSL[DT] = _datatreeDSL

  }

  def write[DT <: Datatree](doc: DT#DocumentRoot)(implicit rdfIo: RdfIo[DT]): XML.Element =
    rdfIo.write(doc)

  object XML {

    type Element = scalatags.Text.TypedTag[String]
    import scalatags.Text.all.makeAbstractTypedTag

    implicit class NamespaceNamespaceOps[DT <: Datatree](val _n: DT#Namespace) extends AnyVal {
      def toNamespace(implicit webDSL: WebDSL[DT]): sg.Namespace = new sg.Namespace {
        import webDSL._
        import webDSL.Members._
        def uri = webDSL.NamespaceMembers.ops.toAllNamespaceMembersOps(_n).uri.get
      }
    }

    implicit def namespacetoNamespace[DT <: Datatree](n: DT#Namespace)(implicit webDSL: WebDSL[DT]): sg.Namespace =
      n.toNamespace

    implicit class QNameTagOps[DT <: Datatree](val _q: DT#QName) extends AnyVal {
      def tag(implicit webDSL: WebDSL[DT]): Element = {
        import webDSL._
        import webDSL.Members._
        makeAbstractTypedTag(_q.prefix.get + ":" + _q.localName.get, false, _q.namespace)
      }

      def attr(implicit webDSL: WebDSL[DT]): sg.Attr = {
        import webDSL._
        import webDSL.Members._
        sg.Attr(_q.prefix.get + ":" + _q.localName.get, None)
      }
    }

    implicit def genericAttr[T]: scalatags.Text.GenericAttr[T] = new scalatags.Text.GenericAttr[T]

    implicit class NamespaceBindingAttrOps[DT <: Datatree](val _nsb: DT#NamespaceBinding) extends AnyVal {
      def xmlns(implicit webDSL: WebDSL[DT]): sg.AttrPair[Builder, String] = {
        import webDSL._
        import webDSL.Members._
        sg.Attr("xmlns:" + _nsb.prefix.get) := _nsb.namespace.uri
      }
    }
  }
}

trait RdfIo[DT <: Datatree] {
  protected implicit val ioConstants: RdfConstants[DT]
  protected implicit val jQName: JQName[DT]
  protected implicit val webDSL: WebDSL[DT]
  protected implicit val relationsDSL: RelationsDSL[DT]
  protected implicit val datatreeDSL: DatatreeDSL[DT]

  import ioConstants._
  import jQName._
  import relationsDSL._
  import datatreeDSL._
  import webDSL._

  import relationsDSL.Methods._
  import webDSL.Members._
  import webDSL.Methods._
  import datatreeDSL.Members._

  object write {
    import RdfIo.XML._
    import scalatags.Text.all.stringFrag

    def makeBindings(lnToNs: Map[String, String], bindings: DT#ZeroMany[DT#NamespaceBinding])
    : (Map[String, String], Seq[sg.AttrPair[Builder, String]]) = {
      val pairs = bindings.seq map { nsb => nsb.prefix.get -> nsb.namespace.uri }
      val newMap = lnToNs ++ pairs
      val bndgs = bindings.seq filter {
        nsb => lnToNs.get(nsb.prefix.get).contains(nsb.namespace.uri) } map {
        nsb => nsb.xmlns
      }

      (newMap, bndgs)
    }

    def apply(doc: DT#DocumentRoot): Element =
      writeRoot(doc, Map(rdf.prefix.get -> rdf.namespace.uri))

    def writeRoot(doc: DT#DocumentRoot, lnToNs: Map[String, String]): Element =
    {
      val (newMap, bindings) = makeBindings(lnToNs, doc.bindings)
      val children = doc.documents.seq map (d => writeDocument(d, newMap))
      rdf_rdf.tag.apply(rdf.xmlns)(bindings :_*)(children :_*)
    }

    def writeDocument(doc: DT#Document, lnToNs: Map[String, String]): Element =
    {
      val (newMap, bindings) = makeBindings(lnToNs, doc.bindings)
      val id = doc.identity.seq map (rdf_about.attr := _.get)
      val children = doc.properties.seq map (p => writeProperty(p, newMap))
      doc.`type`.get.tag.apply(bindings :_*)(id :_*)(children :_*)
    }

    def writeProperty(prop: DT#NamedProperty, lnToNs: Map[String, String]): Element =
    {
      val (newMap, bindings) = makeBindings(lnToNs, prop.bindings)
      val el = prop.name.get.tag
      prop.value.get.fold(
        doc => el(writeDocument(doc, newMap)),
        lit => lit.fold(
          l => el(l.value),
          l => el(l.value.toString()),
          l => el(l.value.toString()),
          l => el(l.value.toString()),
          l => el(rdf_resource.attr := l.value.get),
          l => el(rdf_datatype.attr := l.valueType)(l.value)
        )
      )
    }
  }
}