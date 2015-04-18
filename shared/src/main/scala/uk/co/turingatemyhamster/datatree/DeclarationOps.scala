package uk.co.turingatemyhamster
package datatree

import typeclass._
import web._
import relations._

import Monoid.Implicits._
import Monoid.ops._

import simulacrum._


object DeclarationOps {
  implicit def ops[DT <: Datatree](implicit
                                   _webDSL: WebDSL[DT],
                                   _datatreeDSL: DatatreeDSL[DT],
                                   _relationsDSL: RelationsDSL[DT]): DeclarationOps[DT] = new DeclarationOps[DT] {
    override protected val webDSL: WebDSL[DT] = _webDSL
    override protected val datatreeDSL: DatatreeDSL[DT] = _datatreeDSL
    override protected val relationsDSL: RelationsDSL[DT] = _relationsDSL
  }
}
/**
 *
 *
 * @author Matthew Pocock
 */
trait DeclarationOps[DT <: Datatree] {
  protected val webDSL: WebDSL[DT]
  protected val relationsDSL: RelationsDSL[DT]
  protected val datatreeDSL: DatatreeDSL[DT]

  import webDSL._
  import webDSL.Members._
  import webDSL.Methods._
  import relationsDSL._
  import relationsDSL.Methods._
  import datatreeDSL._
  import datatreeDSL.Members._

  case class DeclaredBindings(inScope: Set[DT#NamespaceBinding] = Set())
  {
    def addBindings[WB <: DT#WithBindings](wb: WB): DeclaredBindings =
      copy(inScope = inScope ++ wb.bindings.seq)

    def at(wb: DT#WithBindings): Map[DT#NamespaceBinding, Set[DT#WithBindings]] = {
      val wbS = Set(wb)
      (for(b <- inScope) yield b -> wbS).toMap
    }

  }

  object DeclaredBindings {
    def apply[WB <: DT#WithBindings](wb: WB): DeclaredBindings =
      DeclaredBindings(inScope = wb.bindings.seq.to[Set])
  }

  implicit val DeclaredBindingsMonoid: Monoid[DeclaredBindings] = new Monoid[DeclaredBindings] {
    override def mzero = DeclaredBindings()

    override def mappend(m1: DeclaredBindings, m2: DeclaredBindings) =
      DeclaredBindings(inScope = m1.inScope mappend m2.inScope)
  }


  case class CollectedBindings(referenced: Set[DT#NamespaceBinding] = Set(),
                               unused: Map[DT#NamespaceBinding, Set[DT#WithBindings]] = Map(),
                               undeclared: Map[DT#NamespaceBinding, Set[DT#WithBindings]] = Map())
  {
    def references(ref: DT#QName): CollectedBindings =
      copy(referenced = referenced + ref.namespaceBinding)

    def isUnused(nb: DT#NamespaceBinding): Boolean =
      unused.get(nb) match {
        case Some(wbs) if !wbs.isEmpty => true
        case _ => false
      }
  }

  implicit val CollectedBindingsMonoid: Monoid[CollectedBindings] = new Monoid[CollectedBindings] {
    override def mzero = CollectedBindings()
    override def mappend(m1: CollectedBindings, m2: CollectedBindings) = CollectedBindings(
      referenced = m1.referenced mappend m2.referenced,
      unused = m1.unused mappend m2.unused,
      undeclared = m1.undeclared mappend m2.undeclared)
  }

  @typeclass trait BindingsCollector[T] {
    def collect(self: T, inherited: DeclaredBindings): CollectedBindings
  }
  import BindingsCollector.ops._

  @typeclass trait BindingsRecursion[T] {
    def recurse(self: T, transitive: DeclaredBindings): CollectedBindings
  }
  import BindingsRecursion.ops._

  implicit def asSeqCollector[T : BindingsCollector]: BindingsCollector[DT#ZeroMany[T]] =
    new BindingsCollector[DT#ZeroMany[T]] {
      override def collect(self: DT#ZeroMany[T], inherited: DeclaredBindings) =
        self.seq.map(_.collect(inherited)).mreduce
    }

  implicit def withBindingsBindingsCollector[DT_WithBindings <: DT#WithBindings : BindingsRecursion]: BindingsCollector[DT_WithBindings] = new BindingsCollector[DT_WithBindings] {
    override def collect(self: DT_WithBindings, inherited: DeclaredBindings): CollectedBindings = {
      val local = DeclaredBindings(self)
      val transitive = inherited mappend local

      val collected = self.recurse(transitive)

      val unused = local.at(self) -- collected.referenced
      val selfS = Set(self : DT#WithBindings)
      val undeclared = (collected.referenced -- transitive.inScope) map (u => u -> selfS)

      CollectedBindings(
        referenced = collected.referenced,
        unused = collected.unused mappend unused,
        undeclared = collected.undeclared mappend undeclared.toMap)
    }
  }

  implicit val rootDocumentBindingsRecursion: BindingsRecursion[DT#DocumentRoot] = new BindingsRecursion[DT#DocumentRoot] {
    override def recurse(self: DT#DocumentRoot, transitive: DeclaredBindings) = self.documents.collect(transitive)
  }

  implicit def documentBindingsRecursion[DT_Document <: DT#Document]: BindingsRecursion[DT_Document] =
    new BindingsRecursion[DT_Document] {
      override def recurse(self: DT_Document, transitive: DeclaredBindings) =
        self.properties.collect(transitive) references self.`type`.get
    }

  implicit val namedPropertyRecursion: BindingsRecursion[DT#NamedProperty] = new BindingsRecursion[DT#NamedProperty] {
    override def recurse(self: DT#NamedProperty, transitive: DeclaredBindings): CollectedBindings = {
      val bindings = CollectedBindings() references self.name.get

      val collected: CollectedBindings = self.value.get.fold(
        (nd: DT#NestedDocument) => nd.collect(transitive) mappend bindings,
        (lit: DT#Literal) => bindings
      )

      collected
    }
  }

  object Syntax {
    implicit class Collector[C](val _c: C) {
      def collectBindings(implicit cb: BindingsCollector[C], rdfConstants: RdfConstants[DT]): CollectedBindings =
        _c.collect(DeclaredBindings(inScope = Set(rdfConstants.rdf)))
    }
  }
}
