package uk.co.turingatemyhamster.datatree

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLStreamReader, XMLInputFactory, XMLOutputFactory, XMLStreamWriter}

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter


object RdfTest {
  import Datatrees._

  def main(args: Array[String]): Unit = {
    val sbol2 = NamespaceBinding(prefix=Prefix("sbol2"), namespace = Namespace(Uri("http://sbol.org/v2#")))
    val testSpace = NamespaceBinding(prefix=Prefix("test"), namespace = Namespace(Uri("http://turingatemyhamster.co.uk/test#")))
    val sbo = NamespaceBinding(prefix=Prefix("sbo"), namespace = Namespace(Uri("http://www.ebi.ac.uk/sbo/main/SBO:")))

    val doc = DocumentRoot(
      bindings = Vector(sbol2, testSpace),
      documents = Vector(
        TopLevelDocument(
          bindings = Vector(),
          identity = Some(testSpace uri "tld1"),
          `type` = sbol2 qName "Model",
          properties = Vector(
            NamedProperty(name = sbol2 qName "language", propertyValue = StringLiteral("SBML")),
            NamedProperty(name = sbol2 qName "source", propertyValue = UriLiteral(Uri("http://www.async.ece.utah.edu/LacI_Inverter.xml"))),
            NamedProperty(name = sbol2 qName "role", propertyValue = UriLiteral(sbo.uri("0000062"))),
            NamedProperty(name = testSpace qName "iMadeThis", propertyValue = NestedDocument(
              bindings = Vector(),
              identity = Some(testSpace uri "unicorn1"),
              `type` = testSpace qName "unicorn",
              properties = Vector(
                NamedProperty(name = testSpace qName "status", propertyValue = StringLiteral("endangered"))
              )
            ))
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


  def readFromString(output: String): DocumentRoot = {
    val reader = XMLInputFactory.newInstance.createXMLStreamReader(new StringReader(output))
    val read = RDF.read(reader)
    read
  }

  def writeToString(doc: DocumentRoot): String = {
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))

    RDF.write(xmlWriter, doc)
    writer.toString
  }
}
