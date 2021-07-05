import sbt._

object Dependencies {
  val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9"

  lazy val slf4jApi = "org.slf4j" % "slf4j-api" % "1.7.31"
  lazy val logbackClassic = "ch.qos.logback" % "logback-classic" % "1.2.3"
}