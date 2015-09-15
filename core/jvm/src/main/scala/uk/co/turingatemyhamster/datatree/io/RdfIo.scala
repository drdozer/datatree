package uk.co.turingatemyhamster
package datatree
package io

import javax.xml.namespace.{QName => jQName}
import javax.xml.stream.{XMLStreamConstants, XMLStreamReader, XMLStreamWriter}

import relations._
import web._
import typeclass._


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
  }
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

  object read {
    val DoubleR = """([+-]?\d+\.\d*(e[+-]\d*)?)""".r
    val LongR = """([+-]?\d+)""".r
    val BooleanR = """((true)|(false))""".r
    val TypedR = """([^^]*)^^(.*)""".r

    implicit class EnhancedReader(val _reader: XMLStreamReader) {
      import javax.xml.stream.XMLStreamConstants._
      def pretty(et: Int): String = et match {
        case START_ELEMENT => "START_ELEMENT"
        case END_ELEMENT => "END_ELELEMENT"
        case PROCESSING_INSTRUCTION => "PROCESSING_INSTRUCTION"
        case CHARACTERS => "CHARACTERS"
        case COMMENT => "COMMENT"
        case SPACE => "SPACE"
        case START_DOCUMENT => "START_DOCUMENT"
        case END_DOCUMENT => "END_DOCUMENT"
        case ENTITY_REFERENCE => "ENTITY_REFERENCE"
        case ATTRIBUTE => "ATTRIBUTE"
        case DTD => "DTD"
        case CDATA => "CDATA"
        case NAMESPACE => "NAMESPACE"
        case NOTATION_DECLARATION => "NOTATIONAL_DECLARATION"
        case ENTITY_DECLARATION => "ENTITY_DECLARATION"
      }

      def context = _reader.getEventType match {
        case START_ELEMENT => s"<${_reader.getName}>"
        case END_ELEMENT => s"</${_reader.getName}>"
        case PROCESSING_INSTRUCTION => s"&${_reader.getPITarget};"
        case CHARACTERS => s"`${_reader.getText}'"
        case COMMENT => "COMMENT"
        case SPACE => "SPACE"
        case START_DOCUMENT => "START_DOCUMENT"
        case END_DOCUMENT => "END_DOCUMENT"
        case ENTITY_REFERENCE => "ENTITY_REFERENCE"
        case ATTRIBUTE => "ATTRIBUTE"
        case DTD => "DTD"
        case CDATA => "CDATA"
        case NAMESPACE => "NAMESPACE"
        case NOTATION_DECLARATION => "NOTATIONAL_DECLARATION"
        case ENTITY_DECLARATION => "ENTITY_DECLARATION"
      }

      def nextContext = {
        if(_reader.hasNext) {
          _reader.next()
          context
        } else {
          "__EOF__"
        }
      }

      def checkState(eventType: Int, msg: => String): Unit = {
        if(_reader.getEventType != eventType)
          throw new IllegalStateException(
            s"$msg\nExpected ${pretty(eventType)} but got ${pretty(_reader.getEventType)} at ${context} followed by ${nextContext}")
      }

      def checkClosing(tn: jQName): Unit = {
        val realTn = _reader.getName

        if(tn != realTn)
          throw new IllegalStateException(
            s"Expected ${tn} but got ${realTn}")
      }

      def getAttribute(attrName: DT#QName): Seq[String] = for {
        i <- 0 until _reader.getAttributeCount if (_reader.getAttributeName(i) : DT#QName) == attrName
      } yield _reader.getAttributeValue(i)

      def nextSkippingWhitespace(): Int = {
        _reader.next()
        skipWhitespace()
      }

      def skipWhitespace(): Int = {
        while(_reader.getEventType() == XMLStreamConstants.CHARACTERS && _reader.getText.trim.length == 0) {
          _reader.next()
        }

        _reader.getEventType
      }

      def parseElement[T](f: (DT#QName, DT#ZeroMany[DT#NamespaceBinding], Map[DT#QName, String]) => T): T = {
        checkState(XMLStreamConstants.START_ELEMENT, "Starting to parse an element")

        val tagName = _reader.getName()
        val bindings = for(i <- 0 until _reader.getNamespaceCount) yield
          NamespaceBinding(Namespace(Uri(_reader.getNamespaceURI(i))), Prefix(_reader.getNamespacePrefix(i)))
        val attrs = for(i <- 0 until _reader.getAttributeCount) yield
          (_reader.getAttributeName(i) : DT#QName) -> _reader.getAttributeValue(i)

        _reader.next()

        val t = f(
          tagName,
          ZeroMany(bindings :_*),
          attrs.toMap)

        skipWhitespace()
        checkState(XMLStreamConstants.END_ELEMENT, s"Ending parse of element $tagName")
        checkClosing(tagName)
        _reader.nextSkippingWhitespace()

        t
      }

      def parseDocument[T](f: =>T): T = {
        checkState(XMLStreamConstants.START_DOCUMENT, "Starting to parse document")
        _reader.next()

        val t = f

        checkState(XMLStreamConstants.END_DOCUMENT, "Ending parse of document")

        t
      }
    }

    def property(implicit reader: XMLStreamReader): DT#NamedProperty = reader.parseElement { (qname, bindings, attrs) =>
      def makeLiteral(text: String): DT#Literal = text match {
        case DoubleR(d) => DoubleLiteral(d.toDouble)
        case LongR(i) => LongLiteral(i.toLong)
        case BooleanR(b) => BooleanLiteral(b.toBoolean)
        case s => StringLiteral(s)
      }

      //        println(s"Reading property at ${reader.context}")
      val v = attrs.get(rdf_resource).headOption match {
        case Some(resource) =>
          //            println("Found resource")
          val lit = UriLiteral(Uri(resource))
          lit
        case None =>
          reader.getEventType() match {
            case XMLStreamConstants.CHARACTERS if reader.getText.trim.length != 0 =>
              val text = reader.getText
              val lit = makeLiteral(text)
              reader.next()
              //                println(s"Read literal $lit} and advanced to ${reader.context}")
              lit
            case XMLStreamConstants.START_ELEMENT =>
              //                println(s"Found nested document at ${reader.context}")
              val nd = nestedDocument
              //                println(s"Read nested document at ${reader.context}")
              nd
            case XMLStreamConstants.CHARACTERS =>
              reader.nextSkippingWhitespace()
              //                println(s"Guessing nested document at ${reader.context}")
              val nd = nestedDocument
              //                println(s"Read guessed nested document at ${reader.context}")
              nd
          }
      }

      NamedProperty(
        bindings = bindings,
        name = One(qname),
        value = One(v))
    }

    def properties(implicit reader: XMLStreamReader): DT#ZeroMany[DT#NamedProperty] = reader.getEventType match {
      case XMLStreamConstants.START_ELEMENT =>
        //          println(s"Found a new property at ${reader.context}")
        property +: properties
      case XMLStreamConstants.END_ELEMENT =>
        //          println(s"Found end of properties at ${reader.context}")
        ZeroMany()
      case XMLStreamConstants.CHARACTERS if reader.getText.trim.length == 0 =>
        //          println(s"Skipping whitespace in properties at ${reader.context}")
        reader.next()
        properties
    }

    def document(implicit reader: XMLStreamReader): (
      DT#ZeroMany[DT#NamespaceBinding],
        DT#ZeroOne[DT#Uri],
        DT#One[DT#QName],
        DT#ZeroMany[DT#NamedProperty]) =
      reader.parseElement { (qname, bindings, attrs) =>
        val id = ZeroOne.fromOption(attrs get rdf_about map (Uri apply))
        val ps = properties
        (bindings, id, One(qname), ps)
      }

    def nestedDocument(implicit reader: XMLStreamReader): DT#NestedDocument = {
      reader.checkState(XMLStreamConstants.START_ELEMENT, "Wrong state to start reading a nested document")
      val (nsb, id, tp, np) = document
      NestedDocument(
        bindings = nsb,
        identity = id,
        `type` = tp,
        properties = np)
    }

    def topLevelDocument(implicit reader: XMLStreamReader): DT#TopLevelDocument = {
      reader.checkState(XMLStreamConstants.START_ELEMENT, "Wrong state to start reading top level document")
      val (nsb, id, tp, np) = document
      TopLevelDocument(
        nsb,
        id,
        tp,
        np)
    }

    def topLevelDocuments(implicit reader: XMLStreamReader): DT#ZeroMany[DT#TopLevelDocument] = reader.getEventType match {
      case XMLStreamConstants.START_ELEMENT =>
        topLevelDocument +: topLevelDocuments
      case XMLStreamConstants.END_ELEMENT =>
        ZeroMany()
      case XMLStreamConstants.CHARACTERS if reader.getText.trim.length == 0 =>
        reader.next()
        topLevelDocuments
    }

    def documentRoot(implicit reader: XMLStreamReader): DT#DocumentRoot = reader.parseElement { (qname, bindings, attrs) =>
      if(qname != rdf_rdf) throw new IllegalStateException("Expecting rdf:rdf but got $qname")

      DocumentRoot(
        bindings,
        topLevelDocuments)
    }

    def apply[T](reader: XMLStreamReader, readFunc: (XMLStreamReader) => T): T = {
      reader.parseDocument {
        readFunc(reader)
      }
    }

    def apply(reader: XMLStreamReader): DT#DocumentRoot =
      apply(reader, documentRoot(_))
  }

  object write {

    implicit class EnhancedWriter(val _writer: XMLStreamWriter) {

      def writeStartElement(tag: DT#QName): Unit =
      {
        _writer.writeStartElement(tag.prefix.get, tag.localName.get, tag.namespace.uri.get)
      }

      def writeAttribute(attrName: DT#QName, value: DT#Uri): Unit =
        writeAttribute(attrName, value.get)

      def writeAttribute(attrName: DT#QName, value: String): Unit =
        _writer.writeAttribute(
          attrName.prefix.get, attrName.namespace.uri.get, attrName.localName.get, value)
    }

    def bindings(bs: Seq[DT#NamespaceBinding])(implicit writer: XMLStreamWriter): Unit = {
      for(b <- bs) {
        writer.writeNamespace(b.prefix.get, b.namespace.uri.get)
      }
    }

    def document(doc: DT#Document)(implicit writer: XMLStreamWriter): Unit = {
      writer.writeStartElement(doc.`type`.get : DT#QName)
      bindings(doc.bindings.seq)
      for(id <- doc.identity.seq) writer.writeAttribute(rdf_about : DT#QName, id)
      properties(doc.properties.seq)
      writer.writeEndElement()
    }

    def property(prop: DT#NamedProperty)(implicit writer: XMLStreamWriter): Unit = {
      writer.writeStartElement(prop.name.get : DT#QName)
      bindings(prop.bindings.seq)
      prop.value.get.fold(document,
        lit => lit.fold(
          l => writer.writeCharacters(l.value),
          l => writer.writeCharacters(l.value.toString),
          l => writer.writeCharacters(l.value.toString),
          l => writer.writeCharacters(l.value.toString),
          l => writer.writeAttribute(rdf_resource, l.value),
          l => {
            writer.writeAttribute(rdf_datatype : DT#QName, l.valueType)
            writer.writeCharacters(l.value)
          }))
      writer.writeEndElement()
    }

    def properties(props: Seq[DT#NamedProperty])(implicit writer: XMLStreamWriter): Unit = {
      for(p <- props) {
        property(p)
      }
    }

    def documentRoot(docRoot: DT#DocumentRoot)(implicit writer: XMLStreamWriter): Unit = {
      writer.writeStartElement(rdf_rdf : DT#QName)

      val bs = if(docRoot.bindings.seq.contains(rdf)) docRoot.bindings else docRoot.bindings :+ rdf
      bindings(bs.seq)

      for(d <- docRoot.documents.seq) {
        document(d)
      }

      writer.writeEndElement()
    }

    def apply[T](writer: XMLStreamWriter, t: T, writeFunc: (T, XMLStreamWriter) => Unit): Unit = {
      writer.writeStartDocument()

      writeFunc(t, writer)

      writer.writeEndDocument()
      writer.flush()
    }

    def apply(writer: XMLStreamWriter, docRoot: DT#DocumentRoot): Unit =
      apply(writer, docRoot, (dr: DT#DocumentRoot, w: XMLStreamWriter) => documentRoot(dr)(w))
  }
}
