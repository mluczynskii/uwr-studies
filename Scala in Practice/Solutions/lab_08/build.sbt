import Dependencies._

ThisBuild / scalaVersion     := "2.13.12"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.mluczynski"
ThisBuild / organizationName := "mluczynski"

lazy val root = (project in file("."))
  .settings(
    name := "lab_08",
    libraryDependencies += munit % Test,
    libraryDependencies += restfb
  )

// See https://www.scala-sbt.org/1.x/docs/Using-Sonatype.html for instructions on how to publish to Sonatype.
