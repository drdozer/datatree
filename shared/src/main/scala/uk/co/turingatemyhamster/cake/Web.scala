package uk.co.turingatemyhamster.cake

/**
 *
 *
 * @author Matthew Pocock
 */
trait Web {
  type URI

  type Namespace
  type Prefix
  type LocalName

  type NamespaceBinding
  type PrefixedName
  type NamespaceLocal

  type QName

  def URI(uri: String): URI

  def Namespace(uri: URI): Namespace
  def LocalName(localName: String): LocalName
  def Prefix(prefix: String): Prefix

  def NamespaceBinding(namespace: Namespace, prefix: Prefix): NamespaceBinding
  def PrefixedName(prefix: Prefix, localName: LocalName): PrefixedName
  def NamespaceLocal(namespace: Namespace, localName: LocalName): NamespaceLocal

  def QName(namespace: Namespace, localName: LocalName, prefix: Prefix): QName

  implicit def uriOps: URIOps
  
  trait URIOps {
    def uriString(uri: URI): String
    def extendWith(uri: URI, part: String): URI
  }
  
  implicit class URISyntax(val _uri: URI)(implicit val ops: URIOps) {
    def uriString = ops.uriString(_uri)
    def sfsa(part: String) = ops.extendWith(_uri, part)
  }
  
  implicit def namespaceOps: NamespaceOps
  
  trait NamespaceOps {
    def uri(ns: Namespace): URI
  }
  
  implicit class NamespaceSyntax(val _ns: Namespace)(implicit ops: NamespaceOps) {
    def nsWithPrefix(pfx: Prefix) = NamespaceBinding(_ns, pfx)
    def nsWithLocal(ln: LocalName) = NamespaceLocal(_ns, ln)
    def nsURI = ops.uri(_ns)
  }
  
  implicit def localNameOps: LocalNameOps
  
  trait LocalNameOps {
    def rawLocalName(ln: LocalName): String
  }
  
  implicit class LocalNameSyntax(val _ln: LocalName)(implicit ops: LocalNameOps) {
    def rawLocalName = ops.rawLocalName(_ln)
  }
  
  implicit def prefixOps: PrefixOps
  
  trait PrefixOps {
    def rawPrefix(pfx: Prefix): String
  }
  
  implicit class PrefixSyntax(val _pfx: Prefix)(implicit ops: PrefixOps) {
    def pfxWithLocal(ln: LocalName) = PrefixedName(_pfx, ln)
    def rawPrefix = ops.rawPrefix(_pfx)
  }

  implicit def namespaceBindingOps: NamespaceBindingOps

  trait NamespaceBindingOps {
    def namespace(nb: NamespaceBinding): Namespace
    def prefix(nb: NamespaceBinding): Prefix
  }

  implicit class NamespaceBindingSyntax(val _nb: NamespaceBinding)(implicit ops: NamespaceBindingOps) {
    def withLocal(ln: LocalName) = QName(namespace, ln, prefix)
    def namespace = ops.namespace(_nb)
    def prefix = ops.prefix(_nb)
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
    def asURI = namespace.nsURI.sfsa(localName.rawLocalName)
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

trait PartialWeb {
  importedPackages : Web =>

  override type PrefixedName = Impl.PrefixedName
  override type NamespaceBinding = Impl.NamespaceBinding
  override type NamespaceLocal = Impl.NamespaceLocal

  override def PrefixedName(prefix: Prefix, localName: LocalName) = Impl.PrefixedName(prefix, localName)
  override def NamespaceBinding(namespace: Namespace, prefix: Prefix) = Impl.NamespaceBinding(namespace, prefix)
  override def NamespaceLocal(namespace: Namespace, localName: LocalName) = Impl.NamespaceLocal(namespace, localName)


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

  object Impl {
    case class PrefixedName(prefix: Prefix, localName: LocalName)
    case class NamespaceBinding(namespace: Namespace, prefix: Prefix)
    case class NamespaceLocal(namespace: Namespace, localName: LocalName)
  }
}

object Web extends Web with PartialWeb {
  override type URI = java.net.URI

  override type Namespace = String
  override type Prefix = String
  override type LocalName = String
  
  override def URI(uri: String) = java.net.URI.create(uri)

  override implicit def uriOps = new URIOps {
    override def uriString(uri: URI) = uri.toString
    override def extendWith(uri: URI, part: String) = java.net.URI.create(uri.toString + part)
  }

  override implicit def localNameOps = new LocalNameOps {
    override def rawLocalName(ln: LocalName) = ln
  }

  override implicit def prefixOps = new PrefixOps {
    override def rawPrefix(pfx: Prefix) = pfx
  }

  override implicit def namespaceOps = new NamespaceOps {
    override def uri(ns: Namespace) = URI(ns)
  }

  override type QName = javax.xml.namespace.QName

  override def Prefix(prefix: String) = prefix

  override def Namespace(uri: URI) = uri.toString

  override def LocalName(localName: String) = localName

  override def QName(namespace: Namespace, localName: LocalName, prefix: Prefix) =
    new javax.xml.namespace.QName(namespace, localName, prefix)

  override implicit def qnameOps = new QNameOps {
    override def namespace(qn: QName) = qn.getNamespaceURI

    override def prefix(qn: QName) = qn.getPrefix

    override def localName(qn: QName) = qn.getLocalPart
  }
}