package uk.co.turingatemyhamster.datatree

import java.lang
import javax.xml.namespace.{QName => jQName}
import javax.xml.stream.{XMLStreamConstants, XMLStreamReader, XMLStreamWriter}

import uk.co.turingatemyhamster.relations.{Relations, RelationsOps}
import uk.co.turingatemyhamster.web.{Web, WebOps}

trait RdfIo extends Datatree {
  importedPackage : WebOps
    with RelationsOps
    with DatatreeOps =>

  implicit val RDF: RDF

  trait RDF {

    implicit val qname2name: jQName => QName
    implicit val name2qname: QName => jQName

    val rdf = NamespaceBinding(prefix = Prefix("rdf"), namespace = Namespace(Uri(("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))))
    val rdf_rdf = rdf.qName(LocalName("rdf"))
    val rdf_about = rdf.qName(LocalName("about"))
    val rdf_resource = rdf.qName(LocalName("resource"))
    val rdf_datatype = rdf.qName(LocalName("datatype"))

    object read {
      val DoubleR = """([+-]?\d+\.\d*(e[+-]\d*)?)""".r
      val IntegerR = """([+-]?\d+)""".r
      val BooleanR = """((true)|(false))""".r
      val TypedR = """([^^]*)^^(.*)""".r

      implicit class EnhancedReader(val _reader: XMLStreamReader) {
        import XMLStreamConstants._
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

        def getAttribute(attrName: QName): Seq[String] = for {
          i <- 0 until _reader.getAttributeCount if (_reader.getAttributeName(i) : QName) == attrName
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

        def parseElement[T](f: (QName, ZeroMany[NamespaceBinding], Map[QName, String]) => T): T = {
          checkState(XMLStreamConstants.START_ELEMENT, "Starting to parse an element")

          val tagName = _reader.getName()
          val bindings = for(i <- 0 until _reader.getNamespaceCount) yield
            NamespaceBinding(namespace = Namespace(Uri(_reader.getNamespaceURI(i))), prefix = Prefix(_reader.getNamespacePrefix(i)))
          val attrs = for(i <- 0 until _reader.getAttributeCount) yield
            (_reader.getAttributeName(i) : QName) -> _reader.getAttributeValue(i)

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

      def property(implicit reader: XMLStreamReader): NamedProperty = reader.parseElement { (qname, bindings, attrs) =>
        def makeLiteral(text: String): Literal = text match {
          case DoubleR(d) => DoubleLiteral(d.toDouble)
          case IntegerR(i) => IntegerLiteral(i.toInt)
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
          propertyValue = One(v))
      }

      def properties(implicit reader: XMLStreamReader): ZeroMany[NamedProperty] = reader.getEventType match {
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

      def document(implicit reader: XMLStreamReader): (ZeroMany[NamespaceBinding], ZeroOne[Uri], One[QName], ZeroMany[NamedProperty]) =
      reader.parseElement { (qname, bindings, attrs) =>
        val id = ZeroOne.fromOption(attrs get rdf_about map (Uri apply))
        val ps = properties
        (bindings, id, One(qname), ps)
      }

      def nestedDocument(implicit reader: XMLStreamReader): NestedDocument = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Wrong state to start reading a nested document")
        val (nsb, id, tp, np) = document
        NestedDocument(
          bindings = nsb,
          identity = id,
          `type` = tp,
          properties = np)
      }

      def topLevelDocument(implicit reader: XMLStreamReader): TopLevelDocument = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Wrong state to start reading top level document")
        val (nsb, id, tp, np) = document
        TopLevelDocument(
          bindings = nsb,
          identity = id,
          `type` = tp,
          properties = np)
      }

      def topLevelDocuments(implicit reader: XMLStreamReader): ZeroMany[TopLevelDocument] = reader.getEventType match {
        case XMLStreamConstants.START_ELEMENT =>
          topLevelDocument +: topLevelDocuments
        case XMLStreamConstants.END_ELEMENT =>
          ZeroMany()
        case XMLStreamConstants.CHARACTERS if reader.getText.trim.length == 0 =>
          reader.next()
          topLevelDocuments
      }

      def documentRoot(implicit reader: XMLStreamReader): DocumentRoot = reader.parseElement { (qname, bindings, attrs) =>
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

      def apply(reader: XMLStreamReader): DocumentRoot =
        apply(reader, documentRoot(_))
    }

    object write {

      implicit class EnhancedWriter(val _writer: XMLStreamWriter) {

        def writeStartElement(tag: QName): Unit =
        {
          _writer.writeStartElement(tag.prefix.raw, tag.localName.raw, tag.namespace.uri.raw)
        }

        def writeAttribute(attrName: QName, value: Uri): Unit =
          writeAttribute(attrName, value.raw)

        def writeAttribute(attrName: QName, value: String): Unit =
          _writer.writeAttribute(
            attrName.prefix.raw, attrName.namespace.uri.raw, attrName.localName.raw, value)
      }

      def bindings(bs: Seq[NamespaceBinding])(implicit writer: XMLStreamWriter): Unit = {
        for(b <- bs) {
          writer.writeNamespace(b.prefix.raw, b.namespace.uri.raw)
        }
      }

      def document(doc: Document)(implicit writer: XMLStreamWriter): Unit = {
        writer.writeStartElement(doc.`type`.theOne : QName)
        bindings(doc.bindings.seq)
        for(id <- doc.identity.seq) writer.writeAttribute(rdf_about : QName, id)
        properties(doc.properties.seq)
        writer.writeEndElement()
      }

      def property(prop: NamedProperty)(implicit writer: XMLStreamWriter): Unit = {
        writer.writeStartElement(prop.name.theOne : QName)
        bindings(prop.bindings.seq)
        prop.propertyValue match {
          case nd : NestedDocument =>
            document(nd)
          case ul : UriLiteral =>
            writer.writeAttribute(rdf_resource : QName, ul.value)
          case tl : TypedLiteral =>
            writer.writeAttribute(rdf_datatype : QName, tl.xsdType)
            writer.writeCharacters(tl.value)
          case l : Literal =>
            writer.writeCharacters(l.value.toString)
        }
        writer.writeEndElement()      }

      def properties(props: Seq[NamedProperty])(implicit writer: XMLStreamWriter): Unit = {
        for(p <- props) {
          property(p)
        }
      }

      def documentRoot(docRoot: DocumentRoot)(implicit writer: XMLStreamWriter): Unit = {
        writer.writeStartElement(rdf_rdf : QName)

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

      def apply(writer: XMLStreamWriter, docRoot: DocumentRoot): Unit =
        apply(writer, docRoot, (dr: DocumentRoot, w: XMLStreamWriter) => documentRoot(dr)(w))
    }
  }
}
