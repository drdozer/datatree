package uk.co.turingatemyhamster.datatree

import javax.xml.namespace.QName
import javax.xml.stream.{XMLStreamConstants, XMLStreamReader, XMLStreamWriter}

import uk.co.turingatemyhamster.cake.{Relations, ScalaRelations}

object RdfIo
{
  private val DoubleR = """([+-]?\d+\.\d*(e[+-]\d*)?)""".r
  private val IntegerR = """([+-]?\d+)""".r
  private val BooleanR = """((true)|(false))""".r
  private val TypedR = """([^^]*)^^(.*)""".r

  def apply[DT <: Datatree with Relations { type Name = QName; type URI = java.net.URI} ](dt: DT) = new
  {
    import dt._

    private val rdf = NamespaceBinding(prefix = "rdf", namespaceURI = URI("http://www.w3.org/1999/02/22-rdf-syntax-ns#"))
    private val rdf_rdf = rdf.withLocalName("rdf")
    private val rdf_about = rdf.withLocalName("about")
    private val rdf_resource = rdf.withLocalName("resource")
    private val rdf_datatype = rdf.withLocalName("datatype")

    def read(reader: XMLStreamReader): DocumentRoot = {

      def readBindings(): ZeroMany[NamespaceBinding] =
        ZeroMany(
          (for(i <- 0 until reader.getNamespaceCount) yield
            NamespaceBinding(reader.getNamespacePrefix(i), URI(reader.getNamespaceURI(i)))) :_*)

      def readTopLevelDocuments(): ZeroMany[TopLevelDocument] = reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          readTopLevelDocument() +: readTopLevelDocuments()
        case XMLStreamConstants.END_ELEMENT =>
          ZeroMany()
        case XMLStreamConstants.CHARACTERS =>
          readTopLevelDocuments()
      }

      def readTopLevelDocument(): TopLevelDocument = {
        val (nsb, id, tp, np) = readDocument()
        TopLevelDocument(nsb, id, tp, np)
      }
      def readNestedDocument(): NestedDocument = {
        val (nsb, id, tp, np) = readDocument()
        NestedDocument(nsb, id, tp, np)
      }

      def readDocument(): (ZeroMany[NamespaceBinding], One[URI], One[Name], ZeroMany[NamedProperty]) = {
        val rdfType = One(
          Name(reader.getNamespaceURI, URI(reader.getLocalName), reader.getPrefix))
        val bindings = readBindings()
        val identity = readIdentity()
        val properties = readProperties()
        (bindings, identity, rdfType, properties)
      }

      def readProperties(): ZeroMany[NamedProperty] = reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          readProperty() +: readProperties()
        case XMLStreamConstants.END_ELEMENT =>
          ZeroMany()
        case XMLStreamConstants.CHARACTERS =>
          readProperties()
      }

      def readProperty(): NamedProperty = NamedProperty(
        readTagName(),
        readValue())

      def readIdentity(): One[URI] =
        One(
          URI(reader.getAttribute(rdf_about).headOption.getOrElse(
          throw new IllegalStateException("Expecting rdf:about at " + reader.getName))))

      def readTagName(): QName = new QName(reader.getNamespaceURI, reader.getLocalName, reader.getPrefix)

      def readValue(): PropertyValue = {
        val lit = reader.getAttribute(rdf_resource).headOption match {
          case Some(resource) =>
            UriLiteral(URI(resource))
          case None =>
            reader.next() match {
              case XMLStreamConstants.START_ELEMENT =>
                readNestedDocument()
              case XMLStreamConstants.CHARACTERS =>
                val text = reader.getText
                makeLiteral(text)
            }
        }
        reader.next() match {
          case XMLStreamConstants.END_ELEMENT =>
            lit
          case XMLStreamConstants.START_ELEMENT =>
            readNestedDocument()
        }
      }

      def makeLiteral(text: String): Literal = text match {
        case DoubleR(d) => DoubleLiteral(d.toDouble)
        case IntegerR(i) => IntegerLiteral(i.toInt)
        case BooleanR(b) => BooleanLiteral(b.toBoolean)
        case s => StringLiteral(s)
      }

      val rd = reader.next() match {
        case XMLStreamConstants.START_ELEMENT =>
          DocumentRoot(
            readBindings(),
            readTopLevelDocuments())
      }

      while(reader.hasNext) {
        reader.next match {
          case XMLStreamConstants.START_ELEMENT =>
            println("Starting: " + reader.getName)
          case XMLStreamConstants.END_ELEMENT =>
            println("Ending: " + reader.getName)
          case XMLStreamConstants.CHARACTERS =>
            println("Characters: " + reader.getText)
          case XMLStreamConstants.END_DOCUMENT =>
            println("End document")
        }
      }

      rd
    }

    def write(writer: XMLStreamWriter, docRoot: DocumentRoot): Unit = {

      def writeBindings(bs: Seq[NamespaceBinding]): Unit = {
        for(b <- bs) {
          writer.writeNamespace(b.prefix, b.namespaceURI.toString)
        }
      }

      def writeDocument(doc: Document): Unit = {
        writer.writeStartElement(doc.`type`.theOne)
        writeBindings(doc.bindings.seq)
        writer.writeAttribute(rdf_about, doc.identity.theOne)
        writeProperties(doc.properties.seq)
        writer.writeEndElement()
      }

      def writeProperties(props: Seq[NamedProperty]): Unit = {
        for(p <- props) {
          writer.writeStartElement(p.name)
          p.propertyValue match {
            case nd : NestedDocument =>
              writeDocument(nd)
            case ul : UriLiteral =>
              writer.writeAttribute(rdf_resource, ul.value)
            case tl : TypedLiteral =>
              writer.writeAttribute(rdf_datatype, tl.xsdType)
              writer.writeCharacters(tl.value)
            case l : Literal =>
              writer.writeCharacters(l.value.toString)
          }
          writer.writeEndElement()
        }
      }

      writer.writeStartDocument()

      writer.writeStartElement(rdf_rdf)

      val bindings = if(docRoot.bindings.seq.contains(rdf)) docRoot.bindings else docRoot.bindings :+ rdf
      writeBindings(bindings.seq)
      
      for(d <- docRoot.documents.seq) {
        writeDocument(d)
      }

      writer.writeEndDocument()
      writer.flush()
    }
  }

  implicit class EnhancedReader(val _reader: XMLStreamReader) extends AnyVal {

    def getAttribute(attrName: QName): Seq[String] = for {
      i <- 0 until _reader.getAttributeCount if _reader.getAttributeName(i) == attrName
    } yield _reader.getAttributeValue(i)

  }

  implicit class EnhancedWriter(val _writer: XMLStreamWriter) extends AnyVal {

    def writeStartElement(tag: QName): Unit =
      _writer.writeStartElement(tag.getPrefix, tag.getLocalPart, tag.getNamespaceURI)
    
    def writeAttribute(attrName: QName, value: java.net.URI): Unit =
      writeAttribute(attrName, value.toString)

    def writeAttribute(attrName: QName, value: String): Unit =
      _writer.writeAttribute(attrName.getPrefix, attrName.getNamespaceURI, attrName.getLocalPart, value)
  }
}
