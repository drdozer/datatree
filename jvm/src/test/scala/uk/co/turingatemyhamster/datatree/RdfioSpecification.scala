package uk.co.turingatemyhamster
package datatree

import io._
import uk.co.turingatemyhamster.datatree.ast.AstDatatree
import web._
import relations._

import java.io.{StringReader, StringWriter}
import javax.xml.stream.{XMLInputFactory, XMLOutputFactory, XMLStreamWriter, XMLStreamReader}

import com.sun.xml.internal.txw2.output.IndentingXMLStreamWriter
import org.scalacheck._
import Prop._
import Arbitrary._
import Shrink._

import scala.util.Try

object StringGenerators {
  implicit object stringGenerators extends StringGenerators
}

trait StringGenerators {

  val nonEmptyString = for {
    n <- Gen.choose(4, 10)
    s <- nonEmptyStringN(n)
  } yield s

  protected def nonEmptyStringN(n: Int) = {
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

}

object WebGenerators {
  implicit def webGenerators[W <: Web](implicit
                                        _stringGenerators: StringGenerators,
                                        _webDSL: WebDSL[W]): WebGenerators[W] = new WebGenerators[W] {
    override protected val stringGenerators: StringGenerators = _stringGenerators
    override protected val webDSL: WebDSL[W] = _webDSL

    override protected def generatedPrefix = webDSL.Uri("http://www.test.com/")
  }
}

trait WebGenerators[W <: Web] {
  protected val stringGenerators: StringGenerators
  protected val webDSL: WebDSL[W]

  import stringGenerators._
  import webDSL._
  import webDSL.Methods._

  protected def generatedPrefix: W#Uri

  implicit lazy val genUri: Gen[W#Uri] = for {
    path <- nonEmptyString
  } yield generatedPrefix extendWith path


  implicit lazy val arbUri = Arbitrary { genUri }

  implicit lazy val genName: Gen[W#QName] = for {
    prefix <- nonEmptyString
    namespaceUri <- arbitrary[W#Uri]
    localName <- nonEmptyString
  } yield QName(prefix = Prefix(prefix), namespace = Namespace(namespaceUri), localName = LocalName(localName))

  implicit lazy val arbName = Arbitrary { genName }

  implicit lazy val genNamespaceBinding = for {
    prefix <- nonEmptyString
    namespaceUri <- arbitrary[W#Uri]
  } yield NamespaceBinding(prefix = Prefix(prefix), namespace = Namespace(namespaceUri))

  implicit lazy val arbNamespaceBinding = Arbitrary { genNamespaceBinding }
}

object DatatreeGenerators {

  implicit def datatreeGenerators[DT <: Datatree](implicit
                                                  _datatreeDSL: DatatreeDSL[DT],
                                                  _webGenerators: WebGenerators[DT],
                                                  _relationsDSL: RelationsDSL[DT],
                                                  _stringGenerators: StringGenerators,
                                                  _declarationOps: DeclarationOps[DT],
                                                  _rdfConstants: WebDSL[DT],
                                                  _webDSL: WebDSL[DT]): DatatreeGenerators[DT] = new DatatreeGenerators[DT] {
    override protected val datatreeDSL: DatatreeDSL[DT] = _datatreeDSL
    override protected val webGenerators: WebGenerators[DT] = _webGenerators
    override protected val relationsDSL: RelationsDSL[DT] = _relationsDSL
    override protected val stringGenerators: StringGenerators = _stringGenerators
    override protected val declarationOps: DeclarationOps[DT] = _declarationOps
    override implicit protected val rdfConstants: WebDSL[DT] = _rdfConstants
    override protected val webDSL: WebDSL[DT] = _webDSL
  }
}

trait DatatreeGenerators[DT <: Datatree] {
  import Arbitrary.arbitrary

  protected val webDSL: WebDSL[DT]
  implicit protected val rdfConstants: WebDSL[DT]
  protected val relationsDSL: RelationsDSL[DT]
  protected val datatreeDSL: DatatreeDSL[DT]
  protected val declarationOps: DeclarationOps[DT]

  protected val stringGenerators: StringGenerators
  protected val webGenerators: WebGenerators[DT]

  import webDSL._
  import webDSL.Members._
  import relationsDSL._
  import relationsDSL.Methods._
  import datatreeDSL._
  import datatreeDSL.Members._
  import declarationOps._
  import declarationOps.Syntax._

  import stringGenerators._
  import webGenerators._

  implicit lazy val genStringLiteral: Gen[DT#StringLiteral] = for {
    value <- nonEmptyString
  } yield StringLiteral(value)

  implicit lazy val arbStringLiteral = Arbitrary { genStringLiteral }

  implicit lazy val genIntegerLiteral: Gen[DT#LongLiteral] = for {
    value <- arbitrary[Long]
  } yield LongLiteral(value)

  implicit lazy val arbIntegerLiteral = Arbitrary { genIntegerLiteral }

  implicit lazy val genUriLiteral: Gen[DT#UriLiteral] = for {
    uri <- arbitrary[DT#Uri]
  } yield UriLiteral(uri)

  implicit lazy val arbUriLiteral = Arbitrary { genUriLiteral }

  implicit def genLiteral = Gen.oneOf[DT#Literal](genStringLiteral, genIntegerLiteral, genUriLiteral)

  implicit def arbLiteral = Arbitrary { genLiteral }

  implicit def genPropertyValue(sz: Int)(implicit genNestedDoc: Gen[DT#NestedDocument]) =
    if(sz == 0)
      genLiteral
    else
      Gen.frequency(5 -> genLiteral, 1 -> genNestedDoc)


  class NamespaceBindings(nsBindings: Seq[DT#NamespaceBinding]) {

    implicit lazy val genName: Gen[DT#QName] = for {
      nb <- Gen.oneOf(nsBindings)
      localName <- nonEmptyString
    } yield QName(namespace = nb.namespace, localName = LocalName(localName), prefix = nb.prefix)

    implicit lazy val arbName = Arbitrary { genName }

    implicit def genNamedProperty(sz: Int) = for {
      name <- arbitrary[DT#QName]
      propertyValue <- genPropertyValue(sz)(genNestedDocument(sz - 1))
    } yield NamedProperty(name = name, value = propertyValue)

    implicit def genNestedDocument(sz: Int, bs: Seq[DT#NamespaceBinding] = Seq()): Gen[DT#NestedDocument] = for {
      bindings <- Gen.listOfN(2, arbitrary[DT#NamespaceBinding]).map(_ ++ bs)
      nestedBindings = new NamespaceBindings(nsBindings ++ bindings)
      identity <- arbitrary[DT#Uri]
      tpe <- nestedBindings.genName
      properties <- Gen.listOf(nestedBindings.genNamedProperty(sz))
    } yield NestedDocument(ZeroMany(bindings :_*), identity, tpe, ZeroMany(properties :_*))

    implicit def genTopLevelDocument(bs: Seq[DT#NamespaceBinding] = Seq()): Gen[DT#TopLevelDocument] = for {
      bindings <- Gen.listOfN(2, arbitrary[DT#NamespaceBinding]).map(_ ++ bs)
      nestedBindings = new NamespaceBindings(nsBindings ++ bindings)
      identity <- arbitrary[DT#Uri]
      tpe <- arbitrary[DT#QName]
      childCount <- Gen.chooseNum(0, 5)
      properties <- Gen.listOfN(childCount, nestedBindings.genNamedProperty(2))
    } yield TopLevelDocument(ZeroMany(bindings :_*), identity, tpe, ZeroMany(properties :_*))

    implicit lazy val arbTopLevelDocument = Arbitrary { genTopLevelDocument() }
  }

  implicit lazy val genDocumentRoot = for {
    bindings <- Gen.listOfN(5, arbitrary[DT#NamespaceBinding]).map(_.to[Seq])
    fullBindings = bindings :+
      NamespaceBinding(prefix = Prefix("rdf"), namespace = Namespace(Uri("http://www.w3.org/1999/02/22-rdf-syntax-ns#")))
    namespaceBindings = new NamespaceBindings(fullBindings)
    documents <- Gen.listOf(namespaceBindings.genTopLevelDocument())
  } yield {
    DocumentRoot(
      ZeroMany(fullBindings :_*),
      ZeroMany(documents :_*))
  }

  implicit lazy val arbDocumentRoot = Arbitrary { genDocumentRoot }

  implicit def shrinkDocumentRoot: Shrink[DT#DocumentRoot] = Shrink { case dr@datatreeDSL.DocumentRoot(bindings, documents) =>
    val collected = dr.collectBindings

    val fewerNamespaces = for {
      b <- bindings.seq.to[Stream] if collected isUnused b
    } yield DocumentRoot(
        bindings = ZeroMany(bindings.seq filterNot (_ == b) :_*),
        documents = dr.documents)

    val fewerDocs = for {
      ds <- shrink(documents)
    } yield DocumentRoot(
        bindings = dr.bindings,
        documents = ds)

    val shrunkDocs = for {
      d <- documents.seq.to[Stream]
      sd <- shrink(d)
    } yield DocumentRoot(
        bindings = dr.bindings,
        documents = ZeroMany(documents.seq.filterNot ( (_: DT#TopLevelDocument) == d) :+ sd :_*))

    fewerNamespaces ++ fewerDocs ++ shrunkDocs
  }

  implicit def shrinkTopLevelDocument: Shrink[DT#TopLevelDocument] = Shrink { case tld@datatreeDSL.TopLevelDocument(bindings, identity, tpe, props) =>
    val collected = tld.collectBindings

    val fewerNamespaces = for {
      b <- bindings.seq.to[Stream] if collected isUnused b
    } yield TopLevelDocument(
        bindings = ZeroMany(bindings.seq filterNot (_ == b) :_*),
        identity= tld.identity,
        `type` = tld.`type`,
        properties = tld.properties)

    val fewerProps = for {
      p <- shrink(props)
    } yield TopLevelDocument(
      bindings = tld.bindings,
      identity = tld.identity,
      `type` = tld.`type`,
      properties = p)

    fewerNamespaces ++ fewerProps
  }

  implicit def shrinkNestedDocument: Shrink[DT#NestedDocument] = Shrink { case nd@datatreeDSL.NestedDocument(bindings, identity, tpe, props) =>
    val collected = nd.collectBindings

    val fewerNamespaces = for {
      b <- bindings.seq.toStream if collected isUnused b
    } yield NestedDocument(
        bindings = ZeroMany(bindings.seq filterNot (_ == b) :_*),
        identity = nd.identity,
        `type` = nd.`type`,
        properties = nd.properties)

    val fewerProps = for {
      p <- shrink(props)
    } yield NestedDocument(
        bindings = nd.bindings,
        identity = nd.identity,
        `type` = nd.`type`,
        properties = p)

    fewerNamespaces ++ fewerProps
  }
}

object RdfIoSpecification extends Properties("RdfIO") {

  def rdfIoSpecification[DT <: Datatree](impl: String)(implicit
                                                       _ioConstants: RdfConstants[DT],
                                                       _datatreeDSL: DatatreeDSL[DT],
                                                       _webGenerators: WebGenerators[DT],
                                                       _datatreeGenerators: DatatreeGenerators[DT],
                                                       _relationsDSL: RelationsDSL[DT],
                                                       _webDSL: WebDSL[DT],
                                                       _rdfIo: RdfIo[DT]
    ): RdfIoSpecification[DT] = new RdfIoSpecification[DT](s"RdfIO-$impl") {
    override val ioConstants: RdfConstants[DT] = _ioConstants
    override val datatreeDSL: DatatreeDSL[DT] = _datatreeDSL
    override val webGenerators: WebGenerators[DT] = _webGenerators
    override val datatreeGenerators: DatatreeGenerators[DT] = _datatreeGenerators
    override val relationsDSL: RelationsDSL[DT] = _relationsDSL
    override val webDSL: WebDSL[DT] = _webDSL
    override val rdfIo: RdfIo[DT] = _rdfIo
  }

  lazy val astIoSpec = rdfIoSpecification[AstDatatree]("AstDatatree")

  property("AstDatatree can be manipulated by RdfIo") = astIoSpec

}

/**
 *
 *
 * @author Matthew Pocock
 */
abstract class RdfIoSpecification[DT <: Datatree](name: String) extends Properties(name) {
  val ioConstants: RdfConstants[DT]
  val rdfIo: RdfIo[DT]
  val relationsDSL: RelationsDSL[DT]
  val webDSL: WebDSL[DT]
  val datatreeDSL: DatatreeDSL[DT]
  val webGenerators: WebGenerators[DT]
  val datatreeGenerators: DatatreeGenerators[DT]

  import relationsDSL._
  import relationsDSL.Methods._
  import webDSL._
  import webDSL.Members._
  import datatreeDSL._
  import datatreeDSL.Members._
  import webGenerators._
  import datatreeGenerators._

  def is = {
    property("write/read whole document") = forAll { (docRoot: DT#DocumentRoot) =>
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, docRoot)).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      val read = Try {
        rdfIo.read.apply(reader)
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      } get

      (docRoot == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("write/read string attribute") = forAll { (attr: DT#StringLiteral) =>
      val prop = NamedProperty(ZeroMany(ioConstants.rdf), One(ioConstants.rdf_rdf), One(attr))
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, prop, (np: DT#NamedProperty,
                                        w: XMLStreamWriter) => rdfIo.write.property(np)(w))).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      val read = (Try {
        rdfIo.read.apply(reader, (r: XMLStreamReader) => rdfIo.read.property(r))
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      }).get.value.get

      (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("write/read uri attribute") = forAll { (attr: DT#UriLiteral) =>
      val prop = NamedProperty(ZeroMany(ioConstants.rdf), One(ioConstants.rdf_rdf), One(attr))
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, prop, (np: DT#NamedProperty,
                                        w: XMLStreamWriter) => rdfIo.write.property(np)(w))).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      val read = (Try {
        rdfIo.read.apply(reader, (r: XMLStreamReader) => rdfIo.read.property(r))
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      }).get.value.get

      (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("write/read integer attribute") = forAll { (attr: DT#LongLiteral) =>
      val prop = NamedProperty(ZeroMany(ioConstants.rdf), One(ioConstants.rdf_rdf), One(attr))
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, prop, (np: DT#NamedProperty,
                                        w: XMLStreamWriter) => rdfIo.write.property(np)(w))).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      val read = (Try {
        rdfIo.read.apply(reader, (r: XMLStreamReader) => rdfIo.read.property(r))
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      }).get.value.get

      (attr == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("write/read nested document") = forAll(new NamespaceBindings(Seq()).genNestedDocument(2, Seq(ioConstants.rdf))) { (doc: DT#NestedDocument) =>

      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, doc, (nd: DT#NestedDocument,
                                       w: XMLStreamWriter) => rdfIo.write.document(nd)(w))).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      //    println("Reading")
      val read = (Try {
        rdfIo.read.apply(reader, (r: XMLStreamReader) => rdfIo.read.nestedDocument(r))
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      }).get
      //    println("Read")

      (doc == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("write/read flat nested document") = forAll(new NamespaceBindings(Seq()).genNestedDocument(0, Seq(ioConstants.rdf))) { (doc: DT#NestedDocument) =>
      val writer = new StringWriter
      val xmlWriter = new IndentingXMLStreamWriter(
        XMLOutputFactory.newInstance.createXMLStreamWriter(
          writer))
      Try(rdfIo.write(xmlWriter, doc, (nd: DT#NestedDocument,
                                       w: XMLStreamWriter) => rdfIo.write.document(nd)(w))).recover { case e =>
        e.printStackTrace()
        throw e
      }
      val written = writer.toString

      val reader = XMLInputFactory.newInstance.createXMLStreamReader(
        new StringReader(written))

      //    println("Reading")
      val read = (Try {
        rdfIo.read.apply(reader, (r: XMLStreamReader) => rdfIo.read.nestedDocument(r))
      } recover { case e =>
        e.printStackTrace()
        println(written)
        throw e
      }).get
      //    println("Read")

      (doc == read) :| f"original value should be equal to the value after write/read, which is:\n$read\n$written"
    }

    property("nameApi") = forAll { (n: DT#QName) =>
      val n2 = n match {
        case webDSL.QName(prefix, namespaceUri, localPart) => QName(prefix, namespaceUri, localPart)
      }

      (n == n2) :| f"name destructor/constructor should commute: $n2"
    }
  }
}
