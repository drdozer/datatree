import sbt._
import Keys._

import scala.scalajs.sbtplugin.env.nodejs.NodeJSEnv
import scala.scalajs.sbtplugin.ScalaJSPlugin._

import scala.scalajs.sbtplugin.ScalaJSPlugin.ScalaJSKeys._

object Build extends sbt.Build{
  val cross = new utest.jsrunner.JsCrossBuild(
    organization := "uk.co.turingatemyhamster",

    version := "0.1.0",
    scalaVersion := "2.11.2",
    name := "datatree",

    // Sonatype
    publishArtifact in Test := false,
    publishTo <<= version { (v: String) =>
      Some("releases"  at "https://oss.sonatype.org/service/local/staging/deploy/maven2")
    },
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided"
    ),
    autoCompilerPlugins := true,

    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2"),
    pomExtra :=
      <url>https://github.com/turingatemyhamster/datatree</url>
      <developers>
        <developer>
          <id>turingatemyhamster</id>
          <name>Matthew Pocock</name>
          <url>https://github.com/turingatemyhamster</url>
        </developer>
      </developers>

  )

  lazy val root = cross.root.aggregate(sharedJs, sharedJvm)

  val sharedSettings = Seq(
    scalaVersion := "2.11.2",
    target := target.value / "js",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "acyclic" % "0.1.2" % "provided",
      "org.scala-lang" % "scala-reflect" % scalaVersion.value % "provided",
      "com.lihaoyi" %%% "utest" % "0.2.0"
    ),
    addCompilerPlugin("com.lihaoyi" %% "acyclic" % "0.1.2")
  )
  lazy val sharedJvm = project.in(file("shared")).settings(sharedSettings:_*).settings(
    scalaVersion := "2.11.2",
    target := target.value / "jvm",
    moduleName := "shared"
  )
  lazy val sharedJs = project.in(file("shared")).settings(scalaJSSettings ++ sharedSettings:_*).settings(
    scalaVersion := "2.11.2",
    target := target.value / "js",
    moduleName := "shared"
  )
  lazy val js = cross.js.dependsOn(sharedJs % "compile->compile;test->test").settings(
    scalaVersion := "2.11.2",
    (jsEnv in Test) := new NodeJSEnv
  )
  lazy val jvm = cross.jvm.dependsOn(sharedJvm % "compile->compile;test->test").settings(
    scalaVersion := "2.11.2",
    resolvers += "bintray/non" at "http://dl.bintray.com/non/maven",
    libraryDependencies += "org.jsawn" %% "jawn-parser" % "0.5.4"
  )
}

