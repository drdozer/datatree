package uk.co.turingatemyhamster.datatree

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLStreamReader, XMLInputFactory, XMLOutputFactory, XMLStreamWriter}

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import uk.co.turingatemyhamster.datatree.Datatree.Name


object RdfTest {
  def main(args: Array[String]): Unit = {
    import Datatree._

    val sbol2 = NamespaceBinding(prefix="sbol2", namespaceURI = URI("http://sbol.org/v2#"))
    val testSpace = NamespaceBinding(prefix="test", namespaceURI = URI("http://turingatemyhamster.co.uk/test#"))
    val sbo = NamespaceBinding(prefix="sbo", namespaceURI = URI("http://www.ebi.ac.uk/sbo/main/SBO:"))

    val doc = DocumentRoot(
      bindings = Vector(sbol2, testSpace),
      documents = Vector(
        TopLevelDocument(
          bindings = Vector(),
          identity = testSpace.uri("tld1"),
          `type` = sbol2.withLocalName("Model"),
          properties = Vector(
            NamedProperty(sbol2.withLocalName("language"), StringLiteral("SBML")),
            NamedProperty(sbol2.withLocalName("source"), UriLiteral(URI("http://www.async.ece.utah.edu/LacI_Inverter.xml"))),
            NamedProperty(sbol2.withLocalName("role"), UriLiteral(sbo.uri("0000062"))),
            NamedProperty(testSpace.withLocalName("iMadeThis"), NestedDocument(
              bindings = Vector(),
              identity = testSpace.uri("unicorn1"),
              `type` = testSpace.withLocalName("unicorn"),
              properties = Vector(
                NamedProperty(testSpace.withLocalName("status"), StringLiteral("endangered"))
              )
            ))
          )
        )
      )
    )

    println(doc)

    val rdfIo = RdfIo(Datatree)

    val output = writeToString(doc)
    println(output)

    val read = readFromString(output)
    println(read)

    val output2 = writeToString(read)
    println(output2)
  }


  def readFromString(output: String): Datatree.DocumentRoot = {
    val reader = XMLInputFactory.newInstance.createXMLStreamReader(new StringReader(output))
    val read = RdfIo(Datatree).read(reader)
    read
  }

  def writeToString(doc: Datatree.DocumentRoot): String = {
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))

    RdfIo(Datatree).write(xmlWriter, doc)
    writer.toString
  }
}
