ThisBuild / scalaVersion := "2.13.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "semver",
    organization := "codes.quine.labo",
    version := "0.1.0-SNAPSHOT",
    console / initialCommands := """
      |import scala.util.chaining._
      |
      |import codes.quine.labo.semver._
      """.stripMargin,
    // test dependencies:
    libraryDependencies += "io.monix" %% "minitest" % "2.8.2" % Test,
    testFrameworks += new TestFramework("minitest.runner.Framework")
  )
