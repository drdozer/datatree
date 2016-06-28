package uk.co.turingatemyhamster
package datatree
package io

import datatree.{Datatree, DatatreeDSL}
import relations.RelationsDSL
import web.{JQName, RdfConstants, WebDSL}
import typeclass._
import org.scalajs.{dom => jsDom}
import jsDom._

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
                                     _datatreeDSL: DatatreeDSL[DT]): RdfIo[DT] = new RdfIo[DT]
  {
    override protected val ioConstants: RdfConstants[DT] = _ioConstants
    override protected val jQName: JQName[DT] = _jQName
    override protected val webDSL: WebDSL[DT] = _webDSL
    override protected val relationsDSL: RelationsDSL[DT] = _relationsDSL
    override protected val datatreeDSL: DatatreeDSL[DT] = _datatreeDSL

    def createDocument: jsDom.Document = {
      import ioConstants._
      import webDSL._
      import webDSL.Members._
      jsDom.document.implementation.createDocument(rdf_rdf.namespace.uri.get, rdf_rdf.localName.get, null)
    }
  }

  def write[DT <: Datatree](writer: jsDom.Document, doc: DT#DocumentRoot)(implicit rdfIo: RdfIo[DT]) =
    rdfIo.write(writer, doc)
}

trait RdfIo[DT <: Datatree] {
  protected val ioConstants: RdfConstants[DT]
  protected val jQName: JQName[DT]
  protected val webDSL: WebDSL[DT]
  protected val relationsDSL: RelationsDSL[DT]
  protected val datatreeDSL: DatatreeDSL[DT]

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
    def apply(writer: jsDom.Document, doc: DT#DocumentRoot): Unit = {
      writeRoot(writer, Map.empty[String, String], writer.documentElement, doc)
    }

    def writeRoot(writer: jsDom.Document,
                  lnToNs: Map[String, String],
                  at: jsDom.Element,
                  doc: DT#DocumentRoot): Unit =
    {
      val withBindings = addBindings(lnToNs, doc.bindings)
      for(c <- doc.documents.seq) {
        writeDocument(writer, withBindings, at, c)
      }
    }

    def writeDocument(writer: jsDom.Document,
                      lnToNs: Map[String, String],
                      at: jsDom.Element,
                      doc: DT#Document): Unit =
    {
      val `type` = doc.`type`.get
      val el = writer.createElementNS(`type`.namespace.uri.get, `type`.localName.get)
      val withBindings = addBindings(lnToNs, doc.bindings)
      addIdentity(writer, withBindings, el, doc.identity)
      addProperties(writer, withBindings, el, doc.properties)
      at.appendChild(el)
    }

    def addBindings(lnToNs: Map[String, String],
                    bindings: DT#ZeroMany[DT#NamespaceBinding]): Map[String, String] =
      lnToNs ++ bindings.seq.map(b => b.prefix.get -> b.namespace.uri.get)

    def addIdentity(writer: jsDom.Document,
                    lnToNs: Map[String, String],
                    at: jsDom.Element,
                    ident: DT#ZeroOne[DT#Uri]): Unit =
      for(i <- ident.seq) {
        at.setAttributeNS(rdf_about.namespace.uri.get, rdf_about.localName.get, i.get)
      }

    def addProperties(writer: jsDom.Document,
                      lnToNs: Map[String, String],
                      at: jsDom.Element,
                      props: DT#ZeroMany[DT#NamedProperty]): Unit =
      for(p <- props.seq) {
        val nm = p.name.get
        val pEl = writer.createElementNS(nm.namespace.uri.get, nm.localName.get)
        at.appendChild(pEl)
        p.value.get.fold(
          doc => writeDocument(writer, lnToNs, pEl, doc),
          lit => lit.fold(
            l => pEl.textContent = l.value,
            l => pEl.textContent = l.value.toString(),
            l => pEl.textContent = l.value.toString(),
            l => pEl.textContent = l.value.toString(),
            l => pEl.setAttributeNS(rdf_resource.namespace.uri.get, rdf_resource.localName.get, l.value.get),
            l => {
              pEl.setAttributeNS(rdf_datatype.namespace.uri.get, rdf_datatype.localName.get, l.valueType)
              pEl.textContent = l.value
            }
          )
        )
      }
  }
}