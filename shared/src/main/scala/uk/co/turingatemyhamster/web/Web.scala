package uk.co.turingatemyhamster.web

/**
 *
 *
 * @author Matthew Pocock
 */
trait Web {
  
  type Uri

  type Namespace
  type Prefix
  type LocalName

  type NamespaceBinding
  type PrefixedName
  type NamespaceLocal

  type QName

}

trait WebOps {
  importedPackages : Web =>
  
  val Uri : UriApi

  trait UriApi {
    def apply(uri: String): Uri
    def unapply(uri: Uri): Option[String]
  }

  val Namespace : NamespaceApi
  
  trait NamespaceApi {
    def apply(uri: Uri): Namespace
    def unapply(ns: Namespace): Option[Uri]
  }
  
  val LocalName : LocalNameApi
  
  trait LocalNameApi {
    def apply(localName: String): LocalName
    def unapply(ln: LocalName): Option[String]
  }
  
  val Prefix : PrefixApi
  
  trait PrefixApi {
    def apply(prefix: String): Prefix
    def unapply(px: Prefix): Option[String]
  }

  val NamespaceBinding : NamespaceBindingApi
  
  trait NamespaceBindingApi {
    def apply(namespace: Namespace, prefix: Prefix): NamespaceBinding
    def unapply(nb: NamespaceBinding): Option[(Namespace, Prefix)]
  }

  val PrefixedName : PrefixedNameApi
  
  trait PrefixedNameApi {
    def apply(prefix: Prefix, localName: LocalName): PrefixedName
    def unappy(pn: PrefixedName): Option[(Prefix, LocalName)]
  }
  
  val NamespaceLocal : NamespaceLocalApi
  
  trait NamespaceLocalApi {
    def apply(namespace: Namespace, localName: LocalName): NamespaceLocal
    def unapply(nl: NamespaceLocal): Option[(Namespace, LocalName)]
  }

  val QName : QNameApi
    
  trait QNameApi {
    def apply(namespace: Namespace, localName: LocalName, prefix: Prefix): QName
    def unapply(qn: QName): Option[(Namespace, LocalName, Prefix)] 
  }

  implicit def uriOps: UriOps
  
  trait UriOps {
    def uriString(uri: Uri): String
    def extendWith(uri: Uri, part: String): Uri
  }
  
  implicit class UriSyntax(val _uri: Uri)(implicit val ops: UriOps) {
    def raw = ops.uriString(_uri)
    def extendWith(part: String) = ops.extendWith(_uri, part)
  }
  
  implicit def namespaceOps: NamespaceOps
  
  trait NamespaceOps {
    def uri(ns: Namespace): Uri
  }
  
  implicit class NamespaceSyntax(val _ns: Namespace)(implicit ops: NamespaceOps) {
    def nsWithPrefix(pfx: Prefix) = NamespaceBinding(_ns, pfx)
    def nsWithLocal(ln: LocalName) = NamespaceLocal(_ns, ln)
    def uri = ops.uri(_ns)
  }
  
  implicit def localNameOps: LocalNameOps
  
  trait LocalNameOps {
    def raw(ln: LocalName): String
  }
  
  implicit class LocalNameSyntax(val _ln: LocalName)(implicit ops: LocalNameOps) {
    def raw = ops.raw(_ln)
  }
  
  implicit def prefixOps: PrefixOps
  
  trait PrefixOps {
    def raw(pfx: Prefix): String
  }
  
  implicit class PrefixSyntax(val _pfx: Prefix)(implicit ops: PrefixOps) {
    def prefixedName(ln: LocalName) = PrefixedName(_pfx, ln)
    def raw = ops.raw(_pfx)
  }

  implicit def namespaceBindingOps: NamespaceBindingOps

  trait NamespaceBindingOps {
    def namespace(nb: NamespaceBinding): Namespace
    def prefix(nb: NamespaceBinding): Prefix
  }

  implicit class NamespaceBindingSyntax(val _nb: NamespaceBinding)(implicit ops: NamespaceBindingOps) {
    def withLocal(ln: LocalName): QName = QName(namespace, ln, prefix)
    def namespace: Namespace = ops.namespace(_nb)
    def prefix: Prefix = ops.prefix(_nb)
    def namespaceLocal(ln: LocalName): NamespaceLocal = NamespaceLocal(ops.namespace(_nb), ln)
    def prefixedName(ln: LocalName): PrefixedName = PrefixedName(ops.prefix(_nb), ln)
    def qName(ln: LocalName): QName = QName(ops.namespace(_nb), ln, ops.prefix(_nb))
    def qName(ln: String): QName = qName(LocalName(ln))
    def uri(localName: String): Uri = namespaceLocal(LocalName(localName)).asUri
  }

  implicit def prefixNameOps: PrefixedNameOps

  trait PrefixedNameOps {
    def prefix(pn: PrefixedName): Prefix
    def localName(pn: PrefixedName): LocalName
  }

  implicit class PrefixedNameSyntax(val _pn: PrefixedName)(implicit ops: PrefixedNameOps) {
    def prefix = ops.prefix(_pn)
    def localName = ops.localName(_pn)
  }

  implicit def namespaceLocalOps: NamespaceLocalOps

  trait NamespaceLocalOps {
    def namespace(nl: NamespaceLocal): Namespace
    def localName(nl: NamespaceLocal): LocalName
  }

  implicit class NamespaceLocalSyntax(val _nl: NamespaceLocal)(implicit ops: NamespaceLocalOps) {
    def namespace = ops.namespace(_nl)
    def localName = ops.localName(_nl)
    def asUri = namespace.uri.extendWith(localName.raw)
  }

  implicit def qnameOps: QNameOps

  trait QNameOps {
    def namespace(qn: QName): Namespace
    def localName(qn: QName): LocalName
    def prefix(qn: QName): Prefix
  }

  implicit class QNameSyntax(val _qn: QName)(implicit ops: QNameOps) {
    def namespace = ops.namespace(_qn)
    def localName = ops.localName(_qn)
    def prefix = ops.prefix(_qn)
  }
}

trait WebSingles {
  importedPackages : Web with WebOps =>
  override type Uri = SinglesImpl.Uri
  override type Namespace = SinglesImpl.Namespace
  override type Prefix = SinglesImpl.Prefix
  override type LocalName = SinglesImpl.LocalName


  override val Uri: UriApi = new UriApi {
    override def apply(uri: String) = SinglesImpl.Uri.apply(uri)
    override def unapply(uri: Uri) = SinglesImpl.Uri.unapply(uri)
  }


  override implicit def uriOps = new UriOps {
    override def uriString(uri: Uri) = uri.uriString
    override def extendWith(uri: Uri, part: String) = Uri(uri.uriString + part)
  }

  override val Namespace: NamespaceApi = new NamespaceApi {
    override def apply(uri: Uri) = SinglesImpl.Namespace.apply(uri)
    override def unapply(ns: Namespace) = SinglesImpl.Namespace.unapply(ns)
  }

  override implicit def namespaceOps = new NamespaceOps {
    override def uri(ns: Namespace) = ns.ns
  }

  override val Prefix: PrefixApi = new PrefixApi {
    override def apply(prefix: String) = SinglesImpl.Prefix.apply(prefix)
    override def unapply(px: Prefix) = SinglesImpl.Prefix.unapply(px)
  }


  override val LocalName: LocalNameApi = new LocalNameApi {
    override def apply(localName: String) = SinglesImpl.LocalName.apply(localName)
    override def unapply(ln: LocalName) = SinglesImpl.LocalName.unapply(ln)
  }


  override implicit def localNameOps = new LocalNameOps {
    override def raw(ln: LocalName) = ln.ln
  }

  override implicit def prefixOps = new PrefixOps {
    override def raw(pfx: Prefix) = pfx.pfx
  }

  object SinglesImpl {
    case class Uri(uriString: String)
    case class Namespace(ns: Uri)
    case class Prefix(pfx: String)
    case class LocalName(ln: String)
  }
}

trait WebPairs {
  importedPackages : Web with WebOps =>

  override type PrefixedName = PairsImpl.PrefixedName
  override type NamespaceBinding = PairsImpl.NamespaceBinding
  override type NamespaceLocal = PairsImpl.NamespaceLocal

  override val NamespaceBinding: NamespaceBindingApi = new NamespaceBindingApi {
    override def apply(namespace: Namespace, prefix: Prefix) = PairsImpl.NamespaceBinding(namespace, prefix)
    override def unapply(nb: NamespaceBinding) = PairsImpl.NamespaceBinding.unapply(nb)
  }

  override val PrefixedName: PrefixedNameApi = new PrefixedNameApi {
    override def apply(prefix: Prefix, localName: LocalName) = PairsImpl.PrefixedName.apply(prefix, localName)
    override def unappy(pn: PrefixedName) = PairsImpl.PrefixedName.unapply(pn)
  }

  override val NamespaceLocal: NamespaceLocalApi = new NamespaceLocalApi {
    override def apply(namespace: Namespace, localName: LocalName) = PairsImpl.NamespaceLocal(namespace, localName)
    override def unapply(nl: NamespaceLocal) = PairsImpl.NamespaceLocal.unapply(nl)
  }

  override implicit val namespaceBindingOps = new NamespaceBindingOps {
    override def namespace(nb: NamespaceBinding) = nb.namespace
    override def prefix(nb: NamespaceBinding) = nb.prefix
  }

  override implicit val prefixNameOps = new PrefixedNameOps {
    override def prefix(pn: PrefixedName) = pn.prefix
    override def localName(pn: PrefixedName) = pn.localName
  }

  override implicit val namespaceLocalOps = new NamespaceLocalOps {
    override def namespace(nl: NamespaceLocal) = nl.namespace
    override def localName(nl: NamespaceLocal) = nl.localName
  }

  object PairsImpl {
    case class PrefixedName(prefix: Prefix, localName: LocalName)
    case class NamespaceBinding(namespace: Namespace, prefix: Prefix)
    case class NamespaceLocal(namespace: Namespace, localName: LocalName)
  }
}

trait WebTriples {
  importedPackages : Web with WebOps =>

  override type QName = TriplesImpl.QName

  override val QName: QNameApi = new QNameApi {
    override def apply(namespace: Namespace, localName: LocalName, prefix: Prefix) =
      TriplesImpl.QName.apply(namespace, localName, prefix)

    override def unapply(qn: QName) =
      TriplesImpl.QName.unapply(qn)
  }


  override implicit def qnameOps = new QNameOps {
    override def namespace(qn: QName) = qn.namespace
    override def prefix(qn: QName) = qn.prefix
    override def localName(qn: QName) = qn.localName
  }

  object TriplesImpl {
    case class QName(namespace: Namespace, localName: LocalName, prefix: Prefix)
  }
}

trait WebOpsImpl extends WebOps with WebSingles with WebPairs with WebTriples {
  importedPackages : Web =>
}

object Web extends Web with WebOps with WebOpsImpl
