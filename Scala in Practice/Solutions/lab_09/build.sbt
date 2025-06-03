import Dependencies._

ThisBuild / scalaVersion := "2.13.16"
ThisBuild / version := "1.0"
ThisBuild / libraryDependencies ++= Seq(scalactic, scalatest)
ThisBuild / coverageEnabled := true

scalacOptions ++= Seq(
  "-Werror",
  "-Wunused",
  "-deprecation"
)

lazy val core = (project in file("core"))
  .settings(
    name := "core"
  )

lazy val blackjack = (project in file("blackjack"))
  .settings(
    name := "blackjack"
  )
  .dependsOn(core)

lazy val root = (project in file("."))
  .aggregate(core, blackjack)
  .settings(
    name := "lab_09"
  )
