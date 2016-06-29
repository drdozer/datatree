lazy val sharedSettings = Seq(
  //    scalacOptions ++= Seq("-Xlog-implicits"),
      scalaVersion := "2.11.8",
      scalacOptions ++= Seq("-deprecation", "-unchecked"),
      organization := "uk.co.turingatemyhamster",
      scalacOptions in Test ++= Seq("-Yrangepos"),
      version := "0.2.2",
      publishMavenStyle := true,
      licenses +=("Apache-2.0", url("http://www.apache.org/licenses/LICENSE-2.0.html"))
)

lazy val core = crossProject.enablePlugins(BintrayPlugin).settings(
  name := "datatree-core",
  bintrayRepository in bintray := "maven",
  bintrayOrganization in bintray := None,
  libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.7.0",
  libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.1",
  libraryDependencies += "uk.co.turingatemyhamster" %%% "gv-core" % "0.4.1",
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
).settings(sharedSettings : _*)

lazy val coreJVM = core.jvm.settings(
  libraryDependencies += "org.specs2" %% "specs2-core" % "3.7.2" % "test",
  libraryDependencies += "org.specs2" %% "specs2-scalacheck" % "3.7.2" % "test"
)

lazy val coreJS = core.js.settings(
  libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.5"
)

lazy val root = Project(
  id = "datatree",
  base = file(".")) aggregate (coreJS, coreJVM)
