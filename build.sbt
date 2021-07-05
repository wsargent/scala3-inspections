import Dependencies._

val scala3Version = "3.0.0"

lazy val root = project
  .in(file("."))
  .settings(
    name := "scala3-inspections",
    version := "0.1.0",

    //scalacOptions ++= Seq("-Xcheck-macros", "-Ycheck:all"),
    scalaVersion := scala3Version,

    libraryDependencies += slf4jApi,
    libraryDependencies += logbackClassic
  )
