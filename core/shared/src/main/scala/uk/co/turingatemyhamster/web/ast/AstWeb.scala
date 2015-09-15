package uk.co.turingatemyhamster
package web
package ast

import javax.xml.namespace

import typeclass._


case class Uri(value: String)

case class Namespace[U](uri: U)

case class Prefix(value: String)

case class LocalName(value: String)

case class NamespaceBinding[N, P](namespace: N, prefix: P)

case class PrefixedName[P, N](prefix: P, localName: N)

case class NamespaceLocal[N, L](namespace: N, localName: L)

case class QName[N, L, P](namespace: N, localName: L, prefix: P)

trait AstWeb extends Web {
  override final type Uri = ast.Uri
  override final type Namespace = ast.Namespace[Uri]
  override final type Prefix = ast.Prefix
  override final type LocalName = ast.LocalName

  override final type NamespaceLocal = ast.NamespaceLocal[Namespace, LocalName]
  override final type PrefixedName = ast.PrefixedName[Prefix, LocalName]
  override final type NamespaceBinding = ast.NamespaceBinding[Namespace, Prefix]

  override final type QName = ast.QName[Namespace, LocalName, Prefix]
}

object AstWeb {
  implicit def dsl[W <: AstWeb]: WebDSL[W] = new AstWebDSL[W] {}
}

trait AstWebDSL[W <: AstWeb] extends WebDSL[W] {

  implicit object Uri extends UriCompanion {
    override def apply(uri: String) = ast.Uri.apply(uri)
    override def unapply(uri: W#Uri) = ast.Uri.unapply(uri)
  }

  implicit object uriMembers extends UriMembers[W#Uri] {
    override def get(uri: W#Uri) = uri.value
  }

  implicit object uriMethods extends UriMethods[W#Uri] {
    override def extendWith(uri: W#Uri, part: String) = Uri(uri.value + part)
  }


  implicit val Namespace: NamespaceCompanion = new NamespaceCompanion {
    override def apply(uri: W#Uri) = ast.Namespace.apply(uri)
    override def unapply(ns: W#Namespace) = ast.Namespace.unapply(ns)
  }

  implicit object namespaceMembers extends NamespaceMembers[W#Namespace] {
    override def uri(ns: W#Namespace) = ns.uri
  }


  implicit object Prefix extends PrefixCompanion {
    override def apply(value: String) = ast.Prefix.apply(value)
    override def unapply(px: W#Prefix) = ast.Prefix.unapply(px)
  }

  implicit object prefixMembers extends PrefixMembers[W#Prefix] {
    override def get(pfx: W#Prefix) = pfx.value
  }


  implicit object LocalName extends LocalNameCompanion {
    override def apply(value: String) = ast.LocalName.apply(value)
    override def unapply(ln: W#LocalName) = ast.LocalName.unapply(ln)
  }

  implicit object localNameMembers extends LocalNameMembers[W#LocalName] {
    override def get(ln: W#LocalName) = ln.value
  }


  implicit object NamespaceBinding extends NamespaceBindingCompanion {
    override def apply(namespace: W#Namespace, prefix: W#Prefix) = ast.NamespaceBinding.apply(namespace, prefix)
    override def unapply(nb: W#NamespaceBinding) = ast.NamespaceBinding.unapply(nb)
  }

  implicit object namespaceBindingMembers extends NamespaceBindingMembers[W#NamespaceBinding] {
    override def namespace(nb: W#NamespaceBinding) = nb.namespace
    override def prefix(nb: W#NamespaceBinding) = nb.prefix
  }

  implicit object namespaceBindingMethods extends NamespaceBindingMethods[W#NamespaceBinding] {
    override implicit val members: NamespaceBindingMembers[W#NamespaceBinding] = namespaceBindingMembers
  }


  implicit object PrefixedName extends PrefixedNameCompanion {
    override def apply(prefix: W#Prefix, localName: W#LocalName) = ast.PrefixedName.apply(prefix, localName)
    override def unapply(pn: W#PrefixedName) = ast.PrefixedName.unapply(pn)
  }

  implicit object prefixNameMembers extends PrefixedNameMembers[W#PrefixedName] {
    override def prefix(pn: W#PrefixedName) = pn.prefix
    override def localName(pn: W#PrefixedName) = pn.localName
  }


  implicit object NamespaceLocal extends NamespaceLocalCompanion {
    override def apply(namespace: W#Namespace, localName: W#LocalName) = ast.NamespaceLocal.apply(namespace, localName)
    override def unapply(nl: W#NamespaceLocal) = ast.NamespaceLocal.unapply(nl)
  }

  implicit object namespaceLocalMembers extends NamespaceLocalMembers[W#NamespaceLocal] {
    override def namespace(nl: W#NamespaceLocal) = nl.namespace
    override def localName(nl: W#NamespaceLocal) = nl.localName
  }

  implicit object namespaceLocalMethods extends NamespaceLocalMethods[W#NamespaceLocal] {
    override implicit def members: NamespaceLocalMembers[W#NamespaceLocal] = namespaceLocalMembers
  }


  implicit object QName extends QNameCompanion {
    override def apply(namespace: W#Namespace, localName: W#LocalName, prefix: W#Prefix) =
      ast.QName.apply(namespace, localName, prefix)

    override def unapply(qn: W#QName) =
      ast.QName.unapply(qn)
  }

  implicit object qnameMembers extends QNameMembers[W#QName] {
    override def namespace(qn: W#QName) = qn.namespace
    override def prefix(qn: W#QName) = qn.prefix
    override def localName(qn: W#QName) = qn.localName
  }

  implicit object qnameMethods extends QNameMethods[W#QName] {
    override implicit def members: QNameMembers[AstWeb#QName] = qnameMembers
  }
}