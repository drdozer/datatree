package uk.co.turingatemyhamster
package web

import typeclass._

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

//object Web2Web {
//
//  def ops[W1 <: Web : WebDSL, W2 <: Web : WebDSL]: Web2Web[W1, W2] =
//    new Web2Web[W1, W2] {
//      override final val w1Ops: WebDSL[W1] = implicitly[WebDSL[W1]]
//      override final val w2Ops: WebDSL[W2] = implicitly[WebDSL[W2]]
//    }
//}

//trait Web2Web[W1 <: Web, W2 <: Web] {
//  val w1Ops: WebDSL[W1]
//  val w2Ops: WebDSL[W2]
//
//  import w2Ops._
//
//  implicitly[Constructor1[W2#Uri, String]]
//  implicitly[Constructor1[W2#Namespace, W2#Uri]]
//  implicitly[ConstructorChain[String, W2#Uri]]
//  implicitly[ConstructorChain[W2#Uri, W2#Namespace]]
//  implicitly[ConstructorChain[String, W2#Namespace]]
//
//  implicit val uri12: Constructor1[W2#Uri, W1#Uri] = new Constructor1[W2#Uri, W1#Uri] {
//    override def apply(a: W1#Uri) = a match { case w1Ops.Uri(value) => w2Ops.Uri(value) }
//  }
//  implicit val namespace12: Constructor1[W2#Namespace, W1#Namespace] = new Constructor1[W2#Namespace, W1#Namespace] {
//    override def apply(a: W1#Namespace) = a match { case w1Ops.Namespace(uri) => w2Ops.Namespace(uri) }
//  }
//  implicit val prefix12: Constructor1[W2#Prefix, W1#Prefix] = new Constructor1[W2#Prefix, W1#Prefix] {
//    override def apply(a: W1#Prefix) = a match { case w1Ops.Prefix(p) => w2Ops.Prefix(p) }
//  }
//  implicit val localName12: Constructor1[W2#LocalName, W1#LocalName] = new Constructor1[W2#LocalName, W1#LocalName] {
//    override def apply(a: W1#LocalName) = a match { case w1Ops.LocalName(ln) => w2Ops.LocalName(ln) }
//  }
//  implicit val namespaceBinding12: Constructor1[W2#NamespaceBinding, W1#NamespaceBinding] = new Constructor1[W2#NamespaceBinding, W1#NamespaceBinding]{
//    override def apply(a: W1#NamespaceBinding) = a match { case w1Ops.NamespaceBinding(ns, p) => w2Ops.NamespaceBinding(ns, p) }
//  }
//  implicit val prefixedName12: Constructor1[W2#PrefixedName, W1#PrefixedName] = new Constructor1[W2#PrefixedName, W1#PrefixedName] {
//    override def apply(a: W1#PrefixedName) = a match { case w1Ops.PrefixedName(p, l) => w2Ops.PrefixedName(p, l) }
//  }
//  implicit val namespaceLocal12: Constructor1[W2#NamespaceLocal, W1#NamespaceLocal] = new Constructor1[W2#NamespaceLocal, W1#NamespaceLocal]{
//    override def apply(a: W1#NamespaceLocal) = a match { case w1Ops.NamespaceLocal(n, l) => w2Ops.NamespaceLocal(n, l) }
//  }
//  implicit val qname12: Constructor1[W2#QName, W1#QName] = new Constructor1[W2#QName, W1#QName] {
//    override def apply(a: W1#QName) = a match { case w1Ops.QName(ns, ln, pfx) => w2Ops.QName(ns, ln, pfx) }
//  }
//
//}