package uk.co.turingatemyhamster
package datatree
package io

import javax.xml.namespace.{QName => jQName}
import jawn.ast._

import typeclass._
import relations._
import web._


/**
 *
 *
 * @author Matthew Pocock
 */
trait JsonIO[DT <: Datatree] {
  protected val ioConstants: RdfConstants[DT]
  protected val jQName: JQName[DT]
  protected val webDSL: WebDSL[DT]
  protected val relationsDSL: RelationsDSL[DT]
  protected val datatreeDSL: DatatreeDSL[DT]

  import jQName._
  import relationsDSL._
  import datatreeDSL._
  import webDSL._

  import relationsDSL.Methods._
  import webDSL.Members._
  import webDSL.Methods._
  import datatreeDSL.Members._
//  import datatreeDSL.Methods._

  def qname2string(qname: jQName): String
  def string2qname(string: String): jQName

  val RDF_ABOUT = qname2string(ioConstants.rdf_about)
  val RDF_RESOURCE = qname2string(ioConstants.rdf_resource)

  object write {

    import scala.collection.mutable.{Map => MMap}

    def apply(docRoot: DT#DocumentRoot): JValue = JArray.fromSeq(
      docRoot.documents.seq.map(writeDocument))

    def writeDocument(doc: DT#Document): JObject = {
      val id = doc.identity.seq map { i => RDF_ABOUT -> JString(i.get) }
      val props = doc.properties.seq.groupBy(_.name).map { case (name, values) =>
        qname2string(name.get) -> JArray.fromSeq(values.map { np =>
          writeValue(np.value.get) }) }

      JObject(MMap(
        qname2string(doc.`type`.get) -> JObject.fromSeq(
          (props ++ id).to[Seq])))
    }

    def writeValue(value: DT#PropertyValue): JValue = value.fold(
      writeDocument _,
      writeLiteral _)

    def writeLiteral(l: DT#Literal): JValue = l.fold(
      sLit => JString(sLit.value),
      lLit => JNum(lLit.value),
      dLit => JNum(dLit.value),
      bLit => JBool(bLit.value),
      uLit => JObject.fromSeq(Seq(RDF_RESOURCE -> JString(uLit.value.get))),
      tLit => JString(s"${tLit.value}^^${tLit.valueType}t"))
  }

  object read {

    def apply(jValue: JValue): DT#DocumentRoot = {
      jValue match {
        case jArray : JArray =>
          DocumentRoot(
            documents = readTLDs(jArray))
      }
    }

    def readTLDs(jArray: JArray): DT#ZeroMany[DT#TopLevelDocument] = {
      ZeroMany(jArray.vs map {
        case jObject: JObject => readTLD(jObject)
      } :_*)
    }

    def readD[D <: DT#Document](jObject: JObject, f: (DT#One[DT#QName], DT#ZeroOne[DT#Uri], DT#ZeroMany[DT#NamedProperty]) => D): D = {
      case class State(`type`: DT#QName,
                       identity: Option[DT#Uri] = None,
                       properties: Seq[DT#NamedProperty] = Seq.empty)

      val state = jObject.vs match {
        case Seq((JString(tp), jV)) =>
          val st0 = State(`type` = string2qname(tp))
          jV match {
            case jO : JObject =>
              jO.vs.foldLeft(st0) { case (st, (k, v)) =>
                (k, v) match {
                  case (RDF_ABOUT, JString(a)) =>
                    st.copy(identity = Some(Uri(a)))
                  case (_, jA: JArray) =>
                    val kName = string2qname(k) : DT#QName
                    st.copy(properties =
                      jA.vs map { av => NamedProperty(name = kName, value = One(readPV(jA))) })
                }
              }
          }
      }

      f(One(state.`type`),
        ZeroOne.fromOption(state.identity),
        ZeroMany(state.properties :_*))
    }

    def readTLD(jObject: JObject): DT#TopLevelDocument =
      readD(jObject, (t, i, ps) => TopLevelDocument.apply(ZeroMany(), `type` = t, identity = i, properties = ps))

    def readND(jObject: JObject): DT#NestedDocument =
      readD(jObject, (t, i, ps) => NestedDocument.apply(ZeroMany(), `type` = t, identity = i, properties = ps))

    def readPV(jValue: JValue): DT#PropertyValue = jValue match {
      case JArray(_) =>
        throw new IllegalArgumentException("Can't process array at this position")
      case JTrue =>
        BooleanLiteral(true)
      case JFalse =>
        BooleanLiteral(false)
      case LongNum(l) =>
        LongLiteral(l)
      case DoubleNum(d) =>
        DoubleLiteral(d)
      case JString(str) =>
        StringLiteral(str)
      case JNull =>
        throw new IllegalArgumentException("Can't process NULL at this position")
      case jO : JObject =>
        readNdOrUri(jO)
    }

    def readNdOrUri(jObject: JObject): DT#PropertyValue =
      jObject.vs.get(RDF_RESOURCE).map {
        case JString(uri) => UriLiteral(Uri(uri))
      } getOrElse readND(jObject)
  }

}
