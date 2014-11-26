import sbt._
import sbt.Keys._
import com.inthenow.sbt.scalajs._
import com.inthenow.sbt.scalajs.SbtScalajs._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._

object DatatreeBuild extends Build{
  val module = XModule(id = "datatree", defaultSettings = buildSettings)

  val logger = ConsoleLogger()

  lazy val buildSettings: Seq[Setting[_]] = Seq(
    organization := "uk.co.turingatemyhamster",
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.11.4", "2.11.2"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    version := "0.1.1"
  )

  lazy val datatree            = module.project(datatreeJvm, datatreeJs)
  lazy val datatreeJvm         = module.jvmProject(datatreeSharedJvm)
  lazy val datatreeJs          = module.jsProject(datatreeSharedJs)
  lazy val datatreeSharedJvm   = module.jvmShared()
  lazy val datatreeSharedJs    = module.jsShared(datatreeSharedJvm)
}
