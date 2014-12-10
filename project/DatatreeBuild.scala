import sbt._
import sbt.Keys._
import com.inthenow.sbt.scalajs._
import com.inthenow.sbt.scalajs.SbtScalajs._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import bintray.Plugin._
import org.eclipse.jgit.lib._

object DatatreeBuild extends Build{
  val module = XModule(id = "datatree", defaultSettings = buildSettings)

  val logger = ConsoleLogger()

  val branch = fetchGitBranch()
  val baseVersion = "0.1.2"

  lazy val buildSettings: Seq[Setting[_]] = bintrayPublishSettings ++ Seq(
    organization := "uk.co.turingatemyhamster",
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.11.4", "2.10.4"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    version := {
      if(branch == "main")
        baseVersion
      else
        s"$branch-$baseVersion"
    },
    publishMavenStyle := true,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html")),
    bintray.Keys.bintrayOrganization in bintray.Keys.bintray := None
  )

  lazy val datatree             = module.project(datatreePlatformJvm, datatreePlatformJs)
  lazy val datatreePlatformJvm  = module.jvmProject(datatreeSharedJvm).settings(platformJvmSettings : _*)
  lazy val datatreePlatformJs   = module.jsProject(datatreeSharedJs)
  lazy val datatreeSharedJvm    = module.jvmShared()
  lazy val datatreeSharedJs     = module.jsShared(datatreeSharedJvm)

  lazy val platformJvmSettings = Seq(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
    )
  )

  def fetchGitBranch(): String = {
    val builder = new RepositoryBuilder()
    builder.setGitDir(file(".git"))
    val repo = builder.readEnvironment().findGitDir().build()
    val branch = repo.getBranch
    repo.close()
    branch
  }
}
