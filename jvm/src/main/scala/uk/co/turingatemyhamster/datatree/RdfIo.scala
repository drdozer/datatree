package uk.co.turingatemyhamster.datatree

import javax.xml.namespace.QName
import javax.xml.stream.{XMLStreamConstants, XMLStreamReader, XMLStreamWriter}

import uk.co.turingatemyhamster.relations.{Relations, RelationsOps}
import uk.co.turingatemyhamster.web.{Web, WebOps}

trait RdfIo extends Datatree {
  importedPackage : WebOps
    with RelationsOps
    with DatatreeOps =>

  implicit val RDF: RDF

  trait RDF {

    implicit val qname2name: javax.xml.namespace.QName => QName
    implicit val name2qname: QName => javax.xml.namespace.QName

    private val rdf = NamespaceBinding(prefix = Prefix("rdf"), namespace = Namespace(Uri(("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))))
    private val rdf_rdf = rdf.qName(LocalName("rdf"))
    private val rdf_about = rdf.qName(LocalName("about"))
    private val rdf_resource = rdf.qName(LocalName("resource"))
    private val rdf_datatype = rdf.qName(LocalName("datatype"))

    object read {
      val DoubleR = """([+-]?\d+\.\d*(e[+-]\d*)?)""".r
      val IntegerR = """([+-]?\d+)""".r
      val BooleanR = """((true)|(false))""".r
      val TypedR = """([^^]*)^^(.*)""".r

      implicit class EnhancedReader(val _reader: XMLStreamReader) {

        def checkState(eventType: Int, msg: => String): Unit = {
          if(_reader.getEventType != eventType)
            throw new IllegalStateException(msg)
        }

        def getAttribute(attrName: QName): Seq[String] = for {
          i <- 0 until _reader.getAttributeCount if (_reader.getAttributeName(i) : QName) == attrName
        } yield _reader.getAttributeValue(i)

      }

      def value(implicit reader: XMLStreamReader): One[PropertyValue] = {

        def makeLiteral(text: String): Literal = text match {
          case DoubleR(d) => DoubleLiteral(d.toDouble)
          case IntegerR(i) => IntegerLiteral(i.toInt)
          case BooleanR(b) => BooleanLiteral(b.toBoolean)
          case s => StringLiteral(s)
        }

        val lit = reader.getAttribute(rdf_resource).headOption match {
          case Some(resource) =>
            UriLiteral(Uri(resource))
          case None =>
            reader.next() match {
              case XMLStreamConstants.START_ELEMENT =>
                nestedDocument
              case XMLStreamConstants.CHARACTERS =>
                val text = reader.getText
                makeLiteral(text)
            }
        }
        reader.next() match {
          case XMLStreamConstants.END_ELEMENT =>
            One(lit)
          case XMLStreamConstants.START_ELEMENT =>
            One(nestedDocument(reader))
        }
      }

      def tagName(implicit reader: XMLStreamReader): One[QName] =
        One(
          QName(
            Namespace(Uri(reader.getNamespaceURI)),
            LocalName(reader.getLocalName),
            Prefix(reader.getPrefix)))

      def identity(implicit reader: XMLStreamReader): One[Uri] =
        One(
          Uri(reader.getAttribute(rdf_about).headOption.getOrElse(
            throw new IllegalStateException("Expecting rdf:about at " + reader.getName + " searching with " + rdf_about))))

      def property(implicit reader: XMLStreamReader): NamedProperty = NamedProperty(
        name = tagName,
        propertyValue = value)

      def properties(implicit reader: XMLStreamReader): ZeroMany[NamedProperty] = reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          property +: properties
        case XMLStreamConstants.END_ELEMENT =>
          ZeroMany()
        case XMLStreamConstants.CHARACTERS =>
          properties
      }

      def document(implicit reader: XMLStreamReader): (ZeroMany[NamespaceBinding], One[Uri], One[QName], ZeroMany[NamedProperty]) = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Expecting state START_ELEMENT while reading a document")
        val tp = tagName
        val bs = bindings
        val id = identity
        val ps = properties
        (bs, id, tp, ps)
      }

      def nestedDocument(implicit reader: XMLStreamReader): NestedDocument = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Expecting state START_ELEMENT while reading a nested document")
        val (nsb, id, tp, np) = document
        NestedDocument(
          bindings = nsb,
          identity = id,
          `type` = tp,
          properties = np)
      }

      def topLevelDocument(implicit reader: XMLStreamReader): TopLevelDocument = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Expecting state START_ELEMENT while reading a top-level document")
        val (nsb, id, tp, np) = document
        TopLevelDocument(
          bindings = nsb,
          identity = id,
          `type` = tp,
          properties = np)
      }

      def topLevelDocuments(implicit reader: XMLStreamReader): ZeroMany[TopLevelDocument] = reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          topLevelDocument +: topLevelDocuments
        case XMLStreamConstants.END_ELEMENT =>
          ZeroMany()
        case XMLStreamConstants.CHARACTERS =>
          topLevelDocuments
      }

      def bindings(implicit reader: XMLStreamReader): ZeroMany[NamespaceBinding] = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Expected to be in START_ELEMENT state when reading bindings")
        ZeroMany(
          (for (i <- 0 until reader.getNamespaceCount) yield
            NamespaceBinding(
              prefix = Prefix(reader.getNamespacePrefix(i)),
              namespace = Namespace(Uri(reader.getNamespaceURI(i))))): _*)
      }

      def documentRoot(implicit reader: XMLStreamReader): DocumentRoot = {
        reader.checkState(XMLStreamConstants.START_ELEMENT, "Expecting start of document")

        DocumentRoot(
          bindings,
          topLevelDocuments)
      }

      def apply[T](reader: XMLStreamReader, readFunc: (XMLStreamReader) => T): T = {
        reader.checkState(XMLStreamConstants.START_DOCUMENT, "Expecting start of document")
        reader.next()

        val rd = readFunc(reader)
        reader.next()
        reader.checkState(XMLStreamConstants.END_DOCUMENT, "Expecting end of document")
        rd
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
        writer.writeAttribute(rdf_about : QName, doc.identity.theOne)
        properties(doc.properties.seq)
        writer.writeEndElement()
      }

      def properties(props: Seq[NamedProperty])(implicit writer: XMLStreamWriter): Unit = {
        for(p <- props) {
          writer.writeStartElement(p.name.theOne : QName)
          p.propertyValue match {
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
          writer.writeEndElement()
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
