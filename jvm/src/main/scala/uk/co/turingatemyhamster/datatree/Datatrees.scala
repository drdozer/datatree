package uk.co.turingatemyhamster.datatree

import javax.xml.namespace.{QName => jQName}

import uk.co.turingatemyhamster.relations.{Relations, RelationsOpsScalaImpl, RelationsOps}
import uk.co.turingatemyhamster.web.{WebOpsImpl, WebOps, Web}

/**
 *
 *
 * @author Matthew Pocock
 */
object Datatrees
  extends Datatree
  with WebOpsImpl
  with RelationsOpsScalaImpl
  with DatatreeOps
  with RdfIo
{
  override implicit val RDF: RDF = new RDF {
    override implicit val qname2name: (jQName) => Datatrees.QName =
      (q: jQName) => QName(
        namespace = Namespace(Uri(q.getNamespaceURI)),
        localName = LocalName(q.getLocalPart),
        prefix = Prefix(q.getPrefix))

    override implicit val name2qname: (Datatrees.QName) => jQName =
      (n: QName) => new jQName(
        n.namespace.uri.raw,
        n.localName.ln,
        n.prefix.pfx)
  }
}
