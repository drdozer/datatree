package uk.co.turingatemyhamster
package web

object RdfConstants {
  implicit def constants[W <: Web](implicit dsl: WebDSL[W]): RdfConstants[W] = new RdfConstants[W] {
    override protected implicit val webDSL: WebDSL[W] = dsl
  }
}

trait RdfConstants[W <: Web] {
  protected implicit val webDSL: WebDSL[W]

  import webDSL._
  import webDSL.Syntax._

  lazy val rdf = "rdf" -:- "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  lazy val rdf_rdf = rdf :- "rdf"
  lazy val rdf_about = rdf :- "about"
  lazy val rdf_resource = rdf :- "resource"
  lazy val rdf_datatype = rdf :- "datatype"
}
