Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / scalaVersion := "2.13.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "semver",
    organization := "codes.quine.labo",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import codes.quine.labo.semver._
      """.stripMargin,
    // Set URL mapping of scala standard API for Scaladoc.
    apiMappings ++= scalaInstance.value.libraryJars
      .filter(file => file.getName.startsWith("scala-library") && file.getName.endsWith(".jar"))
      .map(_ -> url(s"http://www.scala-lang.org/api/${scalaVersion.value}/"))
      .toMap,
    // test dependencies:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
