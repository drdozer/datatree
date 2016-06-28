package uk.co.turingatemyhamster.datatree.io

import uk.co.turingatemyhamster.datatree.{Datatree, DatatreeDSL}
import uk.co.turingatemyhamster.graphvizs.dsl.{AttributeList, NodeStatement, Statement, _}
import uk.co.turingatemyhamster.relations.RelationsDSL
import uk.co.turingatemyhamster.web.WebDSL

/**
 *
 *
 * @author Matthew Pocock
 */
trait GraphvizIO[DT <: Datatree] {
  implicit val Graphviz : Graphviz
  implicit val webDSL: WebDSL[DT]
  implicit val relationsDSL: RelationsDSL[DT]
  implicit val datatreeDSL: DatatreeDSL[DT]

  import datatreeDSL._
  import datatreeDSL.Members._
  import webDSL._
  import webDSL.Members._
  import relationsDSL._
  import relationsDSL.Methods._

  trait Graphviz {

    type Styler[T] = T => Map[String, String]

    def documentStyler: Styler[DT#Document]
    def literalStyler: Styler[DT#Literal]
    def edgeStyler: Styler[(DT#Document, DT#QName, DT#PropertyValue)]
    def linkStyler: Styler[(DT#Document, DT#QName, DT#Uri)]

    def write(documentRoot: DT#DocumentRoot): Graph = {

      val lrRanks = AssignmentStatement("rank", "LR") +:
        documentRoot.documents.seq.flatMap(writeDocument) :+ Subgraph(
        AssignmentStatement("rank", "same") +:
          documentRoot.documents.seq.flatMap(writeDocumentIdentity) :_*)

      StrictDigraph("g", (lrRanks :_*))
    }

    def writeDocument(doc: DT#Document): Seq[Statement] = {
      Seq(doc.identity.seq.head.get :| documentStyler(doc))
//      :+
//             doc.properties.map { p => writeProperty(doc, p) } +:
//               Subgraph(None, ,
//                 ("rank" := "same") :+
//                   documentRoot.documents.seq.map(writeDocumentIdentity))
    }

    def writeDocumentIdentity(doc: DT#Document) = doc.identity.seq.map(_.get : NodeStatement)
  }

  implicit class NWithMapAttributes[N](n: N)(implicit asNs: N => NodeStatement) {
    def :| (as: Map[String, String]) = {
      val ns = asNs(n)
      val ass = as.to[Seq].map { case (k, v) => k := v }
      ns.copy(attributes = Some(ns.attributes.getOrElse(AttributeList()) ++ ass))
    }
  }
}
