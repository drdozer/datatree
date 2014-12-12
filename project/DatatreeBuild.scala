import sbt._
import sbt.Keys._
import com.inthenow.sbt.scalajs._
import com.inthenow.sbt.scalajs.SbtScalajs._
import scala.scalajs.sbtplugin.ScalaJSPlugin._
import ScalaJSKeys._
import bintray.Plugin._
import bintray.Keys._
import org.eclipse.jgit.lib._

object DatatreeBuild extends Build {
  val logger = ConsoleLogger()

  logger.info("Java environment:")
  logger.info(System.getenv.toString)

  val baseVersion = "0.1.2"

  lazy val buildSettings: Seq[Setting[_]] = bintrayPublishSettings ++ Seq(
    scalaVersion := "2.11.4",
    crossScalaVersions := Seq("2.11.4", "2.10.4"),
    scalacOptions ++= Seq("-deprecation", "-unchecked"),
    organization := "uk.co.turingatemyhamster",
    version := makeVersion(baseVersion),
    publishMavenStyle := false,
    repository in bintray := "turingatemhyamster",
    bintrayOrganization in bintray := None,
    licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

  val module = XModule(id = "datatree", defaultSettings = buildSettings)

  lazy val datatree             = module.project(datatreePlatformJvm, datatreePlatformJs)
  lazy val datatreePlatformJvm  = module.jvmProject(datatreeSharedJvm).
      settings(platformSettings : _*).settings(platformJvmSettings : _*)
  lazy val datatreePlatformJs   = module.jsProject(datatreeSharedJs).
      settings(platformSettings : _*)
  lazy val datatreeSharedJvm    = module.jvmShared().
        settings(sharedSettings : _*)
  lazy val datatreeSharedJs     = module.jsShared(datatreeSharedJvm).
          settings(sharedSettings : _*)

  lazy val platformSettings = Seq(description := "Datatree platform-specific API")
  lazy val sharedSettings = Seq(description := "Datatree cross-platform API")

  lazy val platformJvmSettings = Seq(
    libraryDependencies ++= Seq(
      "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
    )
  )

  def fetchGitBranch(): String = {
    val builder = new RepositoryBuilder()
    builder.setGitDir(file(".git"))
    val repo = builder.readEnvironment().findGitDir().build()
    val gitBranch = repo.getBranch
    logger.info(s"Git branch reported as: $gitBranch")
    repo.close()
    val travisBranch = Option(System.getenv("TRAVIS_BRANCH"))
    logger.info(s"Travis branch reported as: $travisBranch")

    val branch = (travisBranch getOrElse gitBranch) replaceAll ("/", "_")
    logger.info(s"Computed branch is $branch")
    branch
  }

  def makeVersion(baseVersion: String): String = {
    val branch = fetchGitBranch()
    if(branch == "master") {
      baseVersion
    } else {
      val tjn = Option(System.getenv("TRAVIS_JOB_NUMBER"))
      s"$branch-$baseVersion${
        tjn.map("." + _) getOrElse ""
      }"
    }
  }
}
