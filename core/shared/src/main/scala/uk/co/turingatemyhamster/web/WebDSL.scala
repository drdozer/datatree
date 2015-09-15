package uk.co.turingatemyhamster
package web

import typeclass._
import simulacrum._

object WebDSL {
  def ops[W <: Web : WebDSL]: WebDSL[W] = implicitly[WebDSL[W]]
}

/**
 *
 *
 * @author Matthew Pocock
 */
trait WebDSL[W <: Web] {

  trait UriCompanion extends Companion1[W#Uri] { type A = String }
  implicit val Uri: UriCompanion
  
  implicit val uriMembers: UriMembers[W#Uri]
  @typeclass trait UriMembers[W_Uri] {
    def get(uri: W_Uri): String
  }

  implicit val uriMethods: UriMethods[W#Uri]
  @typeclass trait UriMethods[W_Uri] {
    def extendWith(uri: W_Uri, part: String): W_Uri
  }


  trait NamespaceCompanion extends Companion1[W#Namespace] {
    type A = W#Uri
    override def apply(uri: W#Uri): W#Namespace
    def apply[U : Cstr[W#Uri]#λ](uri: U): W#Namespace =
      apply(uri : W#Uri)
  }
  implicit val Namespace: NamespaceCompanion
  
  implicit val namespaceMembers: NamespaceMembers[W#Namespace]
  @typeclass trait NamespaceMembers[W_Namespace] {
    def uri(ns: W_Namespace): W#Uri
  }


  trait PrefixCompanion extends Companion1[W#Prefix] {
    type A = String
    override def apply(value: A): W#Prefix
  }
  implicit val Prefix: PrefixCompanion

  implicit val prefixMembers: PrefixMembers[W#Prefix]
  @typeclass trait PrefixMembers[W_Prefix] {
    def get(pfx: W_Prefix): String
  }


  trait LocalNameCompanion extends Companion1[W#LocalName] {
    type A = String
    override def apply(value: A): W#LocalName
  }
  implicit val LocalName: LocalNameCompanion
  
  implicit val localNameMembers: LocalNameMembers[W#LocalName]
  @typeclass trait LocalNameMembers[W_LocalName] {
    def get(ln: W_LocalName): String
  }


  trait NamespaceBindingCompanion extends Companion2[W#NamespaceBinding] {
    type A = W#Namespace
    type B = W#Prefix

    override def apply(namespace: W#Namespace, prefix: W#Prefix): W#NamespaceBinding
    def apply[N : Cstr[W#Namespace]#λ, P : Cstr[W#Prefix]#λ](namespace: N, prefix: P): W#NamespaceBinding =
      apply(namespace: W#Namespace, prefix: W#Prefix)
  }
  implicit val NamespaceBinding: NamespaceBindingCompanion
  
  implicit val namespaceBindingMembers: NamespaceBindingMembers[W#NamespaceBinding]
  @typeclass trait NamespaceBindingMembers[W_NamespaceBinding] {
    def namespace(nb: W_NamespaceBinding): W#Namespace

    def prefix(nb: W_NamespaceBinding): W#Prefix
  }

  implicit val namespaceBindingMethods: NamespaceBindingMethods[W#NamespaceBinding]
  @typeclass trait NamespaceBindingMethods[W_NamespaceBinding] {
    implicit val members: NamespaceBindingMembers[W_NamespaceBinding]

    import NamespaceBindingMembers.ops._
    import QNameMethods.ops._
    import NamespaceLocalMethods.ops._

    def withLocalName[LN: Cstr[W#LocalName]#λ](nb: W_NamespaceBinding, ln: LN): W#QName =
      QName(nb.namespace, ln, nb.prefix)
    def namespaceLocal[LN: Cstr[W#LocalName]#λ](nb: W_NamespaceBinding, ln: LN): W#NamespaceLocal =
      withLocalName(nb, ln).namespaceLocal
    def prefixedName[LN: Cstr[W#LocalName]#λ](nb: W_NamespaceBinding, ln: LN): W#PrefixedName =
      withLocalName(nb, ln).prefixedName
    def uri[LN: Cstr[W#LocalName]#λ](nb: W_NamespaceBinding, ln: LN): W#Uri =
      namespaceLocal(nb, ln).asUri
  }


  trait PrefixedNameCompanion extends Companion2[W#PrefixedName] {
    type A = W#Prefix
    type B = W#LocalName

    override def apply(prefix: W#Prefix, localName: W#LocalName): W#PrefixedName
    def apply[P : Cstr[W#Prefix]#λ, L : Cstr[W#LocalName]#λ](prefix: P, localName: L): W#PrefixedName =
      apply(prefix : W#Prefix, localName : W#LocalName)
  }
  implicit val PrefixedName: PrefixedNameCompanion

  implicit val prefixNameMembers: PrefixedNameMembers[W#PrefixedName]
  @typeclass trait PrefixedNameMembers[W_PrefixedName] {
    def prefix(pn: W_PrefixedName): W#Prefix
    def localName(pn: W_PrefixedName): W#LocalName
  }


  trait NamespaceLocalCompanion extends Companion2[W#NamespaceLocal] {
    type A = W#Namespace
    type B = W#LocalName

    override def apply(namespace: W#Namespace, localName: W#LocalName): W#NamespaceLocal
    def apply[N : Cstr[W#Namespace]#λ, L : Cstr[W#LocalName]#λ](namespace: N, localName: L): W#NamespaceLocal =
      apply(namespace : W#Namespace, localName : W#LocalName)
  }
  implicit val NamespaceLocal: NamespaceLocalCompanion

  implicit val namespaceLocalMembers: NamespaceLocalMembers[W#NamespaceLocal]
  @typeclass trait NamespaceLocalMembers[W_NamespaceLocal] {
    def namespace(nl: W_NamespaceLocal): W#Namespace

    def localName(nl: W_NamespaceLocal): W#LocalName
  }

  implicit val namespaceLocalMethods: NamespaceLocalMethods[W#NamespaceLocal]
  @typeclass trait NamespaceLocalMethods[W_NamespaceLocal] {
    implicit def members: NamespaceLocalMembers[W_NamespaceLocal]

    import NamespaceLocalMembers.ops._
    import NamespaceMembers.ops._
    import LocalNameMembers.ops._
    import UriMethods.ops._

    def asUri(nl: W_NamespaceLocal) = {
      nl.namespace.uri.extendWith(nl.localName.get)
    }
  }

  trait QNameCompanion extends Companion3[W#QName] {
    type A = W#Namespace
    type B = W#LocalName
    type C = W#Prefix

    override def apply(namespace: W#Namespace, localName: W#LocalName, prefix: W#Prefix): W#QName
    def apply[N : Cstr[W#Namespace]#λ,
              L : Cstr[W#LocalName]#λ,
              P : Cstr[W#Prefix]#λ](namespace: N, localName: L, prefix: P): W#QName =
      apply(namespace: W#Namespace, localName: W#LocalName, prefix: W#Prefix)
  }
  implicit val QName: QNameCompanion

  implicit val qnameMembers: QNameMembers[W#QName]
  @typeclass trait QNameMembers[W_QName] {
    def namespace(qn: W_QName): W#Namespace

    def localName(qn: W_QName): W#LocalName

    def prefix(qn: W_QName): W#Prefix
  }

  implicit val qnameMethods: QNameMethods[W#QName]
  @typeclass trait QNameMethods[W_QName] {
    implicit def members: QNameMembers[W_QName]
    import QNameMembers.ops._

    def namespaceBinding(qn: W_QName): W#NamespaceBinding = NamespaceBinding(qn.namespace, qn.prefix)
    def prefixedName(qn: W_QName): W#PrefixedName = PrefixedName(qn.prefix, qn.localName)
    def namespaceLocal(qn: W_QName): W#NamespaceLocal = NamespaceLocal(qn.namespace, qn.localName)
  }


  object Syntax {

    implicit class PrefixSyntax[P](val _p: P) {
      def -:- [N](n: N)(implicit pP: ConstructorChain[P, W#Prefix], nN: ConstructorChain[N, W#Namespace]): W#NamespaceBinding =
        NamespaceBinding(namespace = n, prefix = _p)
    }

    implicit class NamespaceBindingSyntax[NB](val _nb: NB) {
      import NamespaceBindingMethods.ops._
      def :- [L](l: L)(implicit nbNB: ConstructorChain[NB, W#NamespaceBinding], lL: ConstructorChain[L, W#LocalName]): W#QName =
        (_nb: W#NamespaceBinding).withLocalName(l)
    }

  }

  object Members
    extends UriMembers.ToUriMembersOps
            with NamespaceMembers.ToNamespaceMembersOps
            with LocalNameMembers.ToLocalNameMembersOps
            with PrefixMembers.ToPrefixMembersOps
            with NamespaceBindingMembers.ToNamespaceBindingMembersOps
            with PrefixedNameMembers.ToPrefixedNameMembersOps
            with NamespaceLocalMembers.ToNamespaceLocalMembersOps
            with QNameMembers.ToQNameMembersOps

  object Methods
    extends UriMethods.ToUriMethodsOps
            with NamespaceBindingMethods.ToNamespaceBindingMethodsOps
            with NamespaceLocalMethods.ToNamespaceLocalMethodsOps
            with QNameMethods.ToQNameMethodsOps

  object SI5070 {
    implicit val dtLocalName: ConstructorChain[String, W#LocalName] =
      ConstructorChain.fromChain(LocalName, ConstructorChain.castS[String, String])
    implicit val dtUri: ConstructorChain[String, W#Uri] =
      ConstructorChain.fromChain(Uri, ConstructorChain.castS[String, String])
    implicit val dtPrefix: ConstructorChain[String, W#Prefix] =
      ConstructorChain.fromChain(Prefix, ConstructorChain.castS[String, String])
    implicit val dtNamespace: ConstructorChain[String, W#Namespace] =
      ConstructorChain.fromChain(Namespace, dtUri)
  }
}

import javax.xml.namespace.{QName => jQName}

@typeclass trait JQName[W <: Web] {
  implicit def j2w(jQName: jQName): W#QName
  implicit def w2j(wQName: W#QName): jQName
}

object JQName {
  import javax.xml.namespace.{QName => jQName}

    implicit def jqname[W <: Web](implicit webDSL: WebDSL[W]): JQName[W] = new JQName[W] {
      import webDSL._
      import webDSL.Members._
      import webDSL.SI5070._

      override implicit def j2w(jQName: jQName) =
        QName(jQName.getNamespaceURI, jQName.getLocalPart, jQName.getPrefix)
      override implicit def w2j(wQName: W#QName) =
        new jQName(wQName.namespace.uri.get, wQName.localName.get, wQName.prefix.get)
    }
}