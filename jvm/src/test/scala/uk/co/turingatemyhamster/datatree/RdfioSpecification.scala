package uk.co.turingatemyhamster.datatree

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLInputFactory, XMLOutputFactory}

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import org.scalacheck._

import scala.util.Try

/**
 *
 *
 * @author Matthew Pocock
 */
object RdfioSpecification extends Properties("RdfIO") {
  import Datatrees._

  import Prop.{_}
  import Gen._
  import Shrink._
  import Arbitrary.arbitrary

  def nonEmptyStringN(n: Int) = {
    val chars = new Array[Char](n)

    def fillI(i: Int): Gen[Array[Char]] = for {
      c <- Gen.alphaChar
      _ = chars(i) = c
      cs <- if(i == 0) Gen.const(chars) else fillI(i-1)
    } yield cs

    for {
      cs <- fillI(n-1)
    } yield new String(cs)
  }

  val nonEmptyString = for {
    n <- Gen.choose(4, 10)
    s <- nonEmptyStringN(n)
  } yield s

  implicit val genUri: Gen[Uri] = for {
    uri <- nonEmptyString
  } yield Uri("http://tests.com/" + uri)

  implicit val arbUri = Arbitrary { genUri }

  implicit val genName: Gen[QName] = for {
    prefix <- nonEmptyString
    namespaceUri <- arbitrary[Uri]
    localName <- nonEmptyString
  } yield QName(prefix = Prefix(prefix), namespace = Namespace(namespaceUri), localName = LocalName(localName))

  implicit val arbName = Arbitrary { genName }

  implicit val genNamespaceBinding = for {
    prefix <- nonEmptyString
    namespaceUri <- arbitrary[Uri]
  } yield NamespaceBinding(prefix = Prefix(prefix), namespace = Namespace(namespaceUri))

  implicit val arbNamespaceBinding = Arbitrary { genNamespaceBinding }

  implicit val genStringLiteral = for {
    value <- nonEmptyString
  } yield StringLiteral(value)

  implicit val genIntegerLiteral = for {
    value <- arbitrary[Int]
  } yield IntegerLiteral(value)

  implicit val genPropertyValue = Gen.oneOf[PropertyValue](genStringLiteral, genIntegerLiteral)

  implicit val arbPropertyValue = Arbitrary { genPropertyValue }

  class NamespaceBindings(nsBindings: Seq[NamespaceBinding]) {

    implicit val genName = for {
      nb <- Gen.oneOf(nsBindings)
      localName <- nonEmptyString
    } yield QName(namespace = nb.namespace, localName = LocalName(localName), prefix = nb.prefix)

    implicit val arbName = Arbitrary { genName }

    implicit val genNamedProperty = for {
      name <- arbitrary[QName]
      propertyValue <- arbitrary[PropertyValue]
    } yield NamedProperty(name, propertyValue)

    implicit val arbNamedProperty = Arbitrary { genNamedProperty }

    implicit val genTopLevelDocument = for {
      bindings <- Gen.listOfN(2, arbitrary[NamespaceBinding]).map(_.to[Vector].toSeq)
      nestedBindings = new NamespaceBindings(nsBindings ++ bindings)
      identity <- arbitrary[One[Uri]]
      tpe <- arbitrary[One[QName]]
      properties <- Gen.listOf(nestedBindings.genNamedProperty)
    } yield TopLevelDocument(bindings, identity, tpe, properties)

    implicit val arbTopLevelDocument = Arbitrary { genTopLevelDocument }
  }

  implicit val genDocumentRoot = for {
    bindings <- Gen.listOfN(5, arbitrary[NamespaceBinding]).map(_.to[Vector].toSeq)
    fullBindings = bindings :+
      NamespaceBinding(prefix = Prefix("rdf"), namespace = Namespace(Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#")))
    namespaceBindings = new NamespaceBindings(fullBindings)
    documents <- Gen.listOf(namespaceBindings.genTopLevelDocument)
  } yield {
    DocumentRoot(
      fullBindings,
      documents)
  }

  implicit val arbDocumentRoot = Arbitrary { genDocumentRoot }

  implicit def shrinkDocumentRoot: Shrink[DocumentRoot] = Shrink { case dr@DocumentRoot(bindings, documents) =>
    val fewerNamespaces = for {
      b <- bindings.toStream if dr.unusedBinding(b)
    } yield dr.copy(bindings = bindings filterNot (_ == b))

    val fewerDocs = for {
      ds <- shrink(documents)
    } yield dr.copy(documents = ds)

    val shrunkDocs = for {
      d <- documents.toStream
      sd <- shrink(d)
    } yield dr.copy(documents = documents.filterNot ( (_: TopLevelDocument) == d) :+ sd)

    fewerNamespaces ++ fewerDocs ++ shrunkDocs
  }

  implicit def shrinkTopLevelDocument: Shrink[TopLevelDocument] = Shrink { case tld@TopLevelDocument(bindings, identity, tpe, props) =>
    val fewerNamespaces = for {
      b <- bindings.toStream if tld.unusedBinding(b)
    } yield tld.copy(bindings = bindings filterNot (_ == b))

    val fewerProps = for {
      p <- shrink(props)
    } yield tld.copy(properties = p)

    fewerNamespaces ++ fewerProps
  }

  property("write_read") = forAll { (docRoot: DocumentRoot) =>
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, docRoot)).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

    val read = Try { RDF.read.apply(reader) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    } get

    (docRoot == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("nameApi") = forAll { (n: QName) =>
    val n2 = n match { case QName(prefix, namespaceUri, localPart) => QName(prefix, namespaceUri, localPart) }

    (n == n2) :| f"name destructor/constructor should commute: $n2"
  }
}
