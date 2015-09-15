package uk.co.turingatemyhamster
package datatree
package io

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLInputFactory, XMLOutputFactory}

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import relations._
import typeclass._
import web._


object RdfTest {

  def main(args: Array[String]): Unit = doTest[datatree.ast.AstDatatree]

  def doTest[DT <: Datatree](implicit
                             datatreeDSL: DatatreeDSL[DT],
                             relationsDSL: RelationsDSL[DT],
                             webDSL: WebDSL[DT],
                             rdfIo: RdfIo[DT]): Unit =
  {
    import datatreeDSL._
    import relationsDSL._
    import webDSL._

    import webDSL.Methods._
    import webDSL.SI5070._

    import datatreeDSL.Syntax._
    import datatreeDSL.SI5070._

    def readFromString(output: String): DT#DocumentRoot = {
      val reader = XMLInputFactory.newInstance.createXMLStreamReader(new StringReader(output))
      val read = rdfIo.read(reader)
      read
    }

    def writeToString(doc: DT#DocumentRoot): String = {
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))

      rdfIo.write(xmlWriter, doc)
      writer.toString
    }

    val sbol2 = NamespaceBinding(prefix=Prefix("sbol2"), namespace = Namespace(Uri("http://sbol.org/v2#")))
    val testSpace = NamespaceBinding(prefix=Prefix("test"), namespace = Namespace(Uri("http://turingatemyhamster.co.uk/test#")))
    val sbo = NamespaceBinding(prefix=Prefix("sbo"), namespace = Namespace(Uri("http://www.ebi.ac.uk/sbo/main/SBO:")))

    val doc = DocumentRoot(
      bindings = ZeroMany(sbol2, testSpace),
      documents = ZeroMany(
        TopLevelDocument(
          identity = testSpace uri "tld1",
          `type` = sbol2 withLocalName "Model",
          properties = ZeroMany(
            sbol2 withLocalName "language" := "SBML",
            sbol2 withLocalName "source" := Uri("http://www.async.ece.utah.edu/LacI_Inverter.xml"),
            sbol2 withLocalName "role" := (sbo uri "0000062"),
            testSpace withLocalName "counter" := 5L,
            testSpace withLocalName "iMadeThis" := NestedDocument(
              `type` = testSpace withLocalName "unicorn",
              properties = ZeroMany(
                testSpace.withLocalName("status") := "critically endangered")
            )
          )
        )
      )
    )


    println(doc)

    val output = writeToString(doc)
    println(output)

    val read = readFromString(output)
    println(read)

    val output2 = writeToString(read)
    println(output2)

  }

}
