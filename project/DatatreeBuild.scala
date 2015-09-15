import sbt._
import sbt.Keys._
import bintray.Plugin._
import bintray.Keys._
import org.eclipse.jgit.lib._
import org.scalajs.sbtplugin.cross.CrossProject._

object DatatreeBuild extends Build {
  val logger = ConsoleLogger()

  logger.info("Java environment:")
  logger.info(System.getenv.toString)

  val baseVersion = "0.2.0"

  lazy val sharedSettings = bintrayPublishSettings ++ Seq(
    //    scalacOptions ++= Seq("-Xlog-implicits"),
        scalaVersion := "2.11.6",
        scalacOptions ++= Seq("-deprecation", "-unchecked"),
        organization := "uk.co.turingatemyhamster",
        version := makeVersion(baseVersion),
        publishMavenStyle := true,
        repository in bintray := "maven",
        bintrayOrganization in bintray := None,
        licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
  )

  lazy val core = crossProject.settings(
    libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.3.0",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0-M5" cross CrossVersion.full)
  ).settings(sharedSettings : _*)

  lazy val coreJVM = core.jvm.settings(
    libraryDependencies += "org.spire-math" %% "jawn-ast" % "0.7.2",
    libraryDependencies += "uk.co.turingatemyhamster" %% "gv-core" % "develop-0.3.3",
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.1" % "test"
  )

  lazy val coreJS = core.js

  lazy val root = Project(
    id = "datatree",
    base = file(".")) aggregate (coreJS, coreJVM)


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
