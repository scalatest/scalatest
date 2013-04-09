import sbt._
import Keys._

object ScalaTestBuildPlugins extends Build {
  val root = Project("scalatest-plugins", file(".")) settings(
    libraryDependencies += "org.antlr" % "stringtemplate" % "3.2"
  )
}
