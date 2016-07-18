package uk.co.turingatemyhamster.datatree.io

import javax.xml.namespace.{QName => jQName}

import uk.co.turingatemyhamster.datatree.{Datatree, DatatreeDSL}
import uk.co.turingatemyhamster.relations.RelationsDSL
import uk.co.turingatemyhamster.web.{JQName, RdfConstants, WebDSL}

import upickle._

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

  import uk.co.turingatemyhamster.typeclass._

  import jQName._
  import relationsDSL._
  import datatreeDSL._
  import webDSL._
  import datatreeDSL.Members._
  import webDSL.Members._
  import relationsDSL.Methods._

  def qname2string(qname: jQName): String
  def string2qname(string: String): jQName

  val RDF_ABOUT = qname2string(ioConstants.rdf_about)
  val RDF_RESOURCE = qname2string(ioConstants.rdf_resource)

  object write {

    import scala.collection.mutable.{Map => MMap}

    def apply(docRoot: DT#DocumentRoot): Js.Value = Js.Arr(
      docRoot.documents.seq.map(writeDocument) :_*)

    def writeDocument(doc: DT#Document): Js.Obj = {
      val id = doc.identity.seq map { i => RDF_ABOUT -> Js.Str(i.get) }
      val props = doc.properties.seq.groupBy(_.name).map { case (name, values) =>
        qname2string(name.get) -> Js.Arr(values.map { np =>
          writeValue(np.value.get) }  :_*) }

      Js.Obj(
        qname2string(doc.`type`.get) -> Js.Obj(
          (props ++ id).to[Seq]  :_*))
    }

    def writeValue(value: DT#PropertyValue): Js.Value = value.fold(
      writeDocument _,
      writeLiteral _)

    def writeLiteral(l: DT#Literal): Js.Value = l.fold(
      lLit => Js.Num(lLit.value),
      dLit => Js.Num(dLit.value),
      bLit => if(bLit.value) Js.True else Js.False,
      uLit => Js.Obj(RDF_RESOURCE -> Js.Str(uLit.value.get)),
      tLit => Js.Str(s"${tLit.value}^^${tLit.valueType}"))
  }

  object read {

    def apply(jValue: Js.Value): DT#DocumentRoot = {
      jValue match {
        case jArray : Js.Arr =>
          DocumentRoot(
            documents = readTLDs(jArray))
      }
    }

    def readTLDs(jArray: Js.Arr): DT#ZeroMany[DT#TopLevelDocument] = {
      ZeroMany(jArray.value map {
        case jObject: Js.Obj => readTLD(jObject)
      } :_*)
    }

    def readD[D <: DT#Document](jObject: Js.Obj, f: (DT#One[DT#QName], DT#ZeroOne[DT#Uri], DT#ZeroMany[DT#NamedProperty]) => D): D = {
      case class State(`type`: DT#QName,
                       identity: Option[DT#Uri] = None,
                       properties: Seq[DT#NamedProperty] = Seq.empty)

      val state = jObject.value match {
        case Seq((tp, jV)) =>
          val st0 = State(`type` = string2qname(tp))
          jV match {
            case jO : Js.Obj =>
              jO.value.foldLeft(st0) { case (st, (k, v)) =>
                (k, v) match {
                  case (RDF_ABOUT, Js.Str(a)) =>
                    st.copy(identity = Some(Uri(a)))
                  case (_, jA: Js.Arr) =>
                    val kName = string2qname(k) : DT#QName
                    st.copy(properties =
                      jA.value map { av => NamedProperty(name = kName, value = One(readPV(jA))) })
                }
              }
          }
      }

      f(One(state.`type`),
        ZeroOne.fromOption(state.identity),
        ZeroMany(state.properties :_*))
    }

    def readTLD(jObject: Js.Obj): DT#TopLevelDocument =
      readD(jObject, (t, i, ps) => TopLevelDocument.apply(ZeroMany(), `type` = t, identity = i, properties = ps))

    def readND(jObject: Js.Obj): DT#NestedDocument =
      readD(jObject, (t, i, ps) => NestedDocument.apply(ZeroMany(), `type` = t, identity = i, properties = ps))

    def readPV(jValue: Js.Value): DT#PropertyValue = jValue match {
      case Js.Arr(_) =>
        throw new IllegalArgumentException("Can't process array at this position")
      case Js.True =>
        BooleanLiteral(true)
      case Js.False =>
        BooleanLiteral(false)
//      case LongNum(l) =>
//        LongLiteral(l)
      case Js.Num(d) =>
        DoubleLiteral(d)
      case Js.Str(str) =>
        TypedLiteral(One(str), ZeroOne(), ZeroOne())
      case Js.Null =>
        throw new IllegalArgumentException("Can't process NULL at this position")
      case jO : Js.Obj =>
        readNdOrUri(jO)
    }

    def readNdOrUri(jObject: Js.Obj): DT#PropertyValue =
      (jObject.value.filter(_._1 == RDF_RESOURCE).map {
        case (_, Js.Str(uri)) => UriLiteral(Uri(uri))
      }).headOption getOrElse readND(jObject)
  }

}
