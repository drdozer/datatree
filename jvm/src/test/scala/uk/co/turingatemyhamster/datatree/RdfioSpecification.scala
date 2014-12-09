package uk.co.turingatemyhamster.datatree

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLInputFactory, XMLOutputFactory, XMLStreamWriter, XMLStreamReader}

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

  implicit val genStringLiteral: Gen[StringLiteral] = for {
    value <- nonEmptyString
  } yield StringLiteral(value)

  implicit val arbStringLiteral = Arbitrary { genStringLiteral }

  implicit val genIntegerLiteral: Gen[IntegerLiteral] = for {
    value <- arbitrary[Int]
  } yield IntegerLiteral(value)

  implicit val arbIntegerLiteral = Arbitrary { genIntegerLiteral }

  implicit val genUriLiteral: Gen[UriLiteral] = for {
    uri <- arbitrary[Uri]
  } yield UriLiteral(uri)

  implicit val arbUriLiteral = Arbitrary { genUriLiteral }

  implicit def genLiteral = Gen.oneOf[Literal](genStringLiteral, genIntegerLiteral, genUriLiteral)

  implicit def arbLiteral = Arbitrary { genLiteral }

  implicit def genPropertyValue(sz: Int)(implicit genNestedDoc: Gen[NestedDocument]) =
    if(sz == 0)
      genLiteral
    else
      Gen.frequency(5 -> genLiteral, 1 -> genNestedDoc)

  class NamespaceBindings(nsBindings: Seq[NamespaceBinding]) {

    implicit val genName: Gen[QName] = for {
      nb <- Gen.oneOf(nsBindings)
      localName <- nonEmptyString
    } yield QName(namespace = nb.namespace, localName = LocalName(localName), prefix = nb.prefix)

    implicit val arbName = Arbitrary { genName }

    implicit def genNamedProperty(sz: Int) = for {
      name <- arbitrary[QName]
      propertyValue <- genPropertyValue(sz)(genNestedDocument(sz - 1))
    } yield NamedProperty(name = name, propertyValue = propertyValue)

    implicit def genNestedDocument(sz: Int, bs: Seq[NamespaceBinding] = Seq()): Gen[NestedDocument] = for {
      bindings <- Gen.listOfN(2, arbitrary[NamespaceBinding]).map(_.to[Vector].toSeq ++ bs)
      nestedBindings = new NamespaceBindings(nsBindings ++ bindings)
      identity <- arbitrary[One[Uri]]
      tpe <- nestedBindings.genName
      properties <- Gen.listOf(nestedBindings.genNamedProperty(sz))
    } yield NestedDocument(bindings, Some(identity), tpe, properties)

    implicit def genTopLevelDocument(bs: Seq[NamespaceBinding] = Seq()): Gen[TopLevelDocument] = for {
      bindings <- Gen.listOfN(2, arbitrary[NamespaceBinding]).map(_.to[Vector].toSeq ++ bs)
      nestedBindings = new NamespaceBindings(nsBindings ++ bindings)
      identity <- arbitrary[One[Uri]]
      tpe <- arbitrary[One[QName]]
      properties <- Gen.listOf(nestedBindings.genNamedProperty(2))
    } yield TopLevelDocument(bindings, Some(identity), tpe, properties)

    implicit val arbTopLevelDocument = Arbitrary { genTopLevelDocument() }
  }

  implicit val genDocumentRoot = for {
    bindings <- Gen.listOfN(5, arbitrary[NamespaceBinding]).map(_.to[Vector].toSeq)
    fullBindings = bindings :+
      NamespaceBinding(prefix = Prefix("rdf"), namespace = Namespace(Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#")))
    namespaceBindings = new NamespaceBindings(fullBindings)
    documents <- Gen.listOf(namespaceBindings.genTopLevelDocument())
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

  implicit def shrinkNestedDocument: Shrink[NestedDocument] = Shrink { case nd@NestedDocument(bindings, identity, tpe, props) =>
      val fewerNamespaces = for {
        b <- bindings.toStream if (nd: Document).unusedBinding(b)
      } yield nd.copy(bindings = bindings filterNot (_ == b))

      val fewerProps = for {
        p <- shrink(props)
      } yield nd.copy(properties = p)

      fewerNamespaces ++ fewerProps
    }

  property("write/read whole document") = forAll { (docRoot: DocumentRoot) =>
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

  property("write/read string attribute") = forAll { (attr: StringLiteral) =>
    val prop = NamedProperty(ZeroMany(RDF.rdf), One(RDF.rdf_rdf), One(attr))
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, prop, (np: NamedProperty, w: XMLStreamWriter) => RDF.write.property(np)(w))).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

    val read = (Try { RDF.read.apply(reader, (r: XMLStreamReader) => RDF.read.property(r)) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    }).get.propertyValue.theOne

    (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("write/read uri attribute") = forAll { (attr: UriLiteral) =>
    val prop = NamedProperty(ZeroMany(RDF.rdf), One(RDF.rdf_rdf), One(attr))
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, prop, (np: NamedProperty, w: XMLStreamWriter) => RDF.write.property(np)(w))).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

    val read = (Try { RDF.read.apply(reader, (r: XMLStreamReader) => RDF.read.property(r)) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    }).get.propertyValue.theOne

    (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("write/read integer attribute") = forAll { (attr: IntegerLiteral) =>
    val prop = NamedProperty(ZeroMany(RDF.rdf), One(RDF.rdf_rdf), One(attr))
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, prop, (np: NamedProperty, w: XMLStreamWriter) => RDF.write.property(np)(w))).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

    val read = (Try { RDF.read.apply(reader, (r: XMLStreamReader) => RDF.read.property(r)) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    }).get.propertyValue.theOne

    (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("write/read nested document") = forAll(new NamespaceBindings(Seq()).genNestedDocument(2, Seq(RDF.rdf))) { (doc: NestedDocument) =>
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, doc, (nd: NestedDocument, w: XMLStreamWriter) => RDF.write.document(nd)(w))).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

//    println("Reading")
    val read = (Try { RDF.read.apply(reader, (r: XMLStreamReader) => RDF.read.nestedDocument(r)) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    }).get
//    println("Read")

    (doc == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("write/read flat nested document") = forAll(new NamespaceBindings(Seq()).genNestedDocument(0, Seq(RDF.rdf))) { (doc: NestedDocument) =>
    val writer = new StringWriter
    val xmlWriter = new IndentingXMLStreamWriter(
      XMLOutputFactory.newInstance.createXMLStreamWriter(
        writer))
    Try(RDF.write(xmlWriter, doc, (nd: NestedDocument, w: XMLStreamWriter) => RDF.write.document(nd)(w))).recover { case e =>
      e.printStackTrace()
      throw e
    }
    val written = writer.toString

    val reader = XMLInputFactory.newInstance.createXMLStreamReader(
      new StringReader(written))

//    println("Reading")
    val read = (Try { RDF.read.apply(reader, (r: XMLStreamReader) => RDF.read.nestedDocument(r)) } recover { case e =>
      e.printStackTrace()
      println(written)
      throw e
    }).get
//    println("Read")

    (doc == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
  }

  property("nameApi") = forAll { (n: QName) =>
    val n2 = n match { case QName(prefix, namespaceUri, localPart) => QName(prefix, namespaceUri, localPart) }

    (n == n2) :| f"name destructor/constructor should commute: $n2"
  }
}
