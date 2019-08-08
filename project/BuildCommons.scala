import sbt._
import Keys._

trait BuildCommons {

  val releaseVersion: String

  val previousReleaseVersion: String  

  def scalaXmlDependency(theScalaVersion: String): Seq[ModuleID]

  def scalatestLibraryDependencies: Seq[ModuleID]

  def scalaLibraries(theScalaVersion: String): Seq[ModuleID]

  def sharedSettings: Seq[Setting[_]]

  def scalacticDocSettings: Seq[Setting[_]]

  def scalatestDocSettings: Seq[Setting[_]]

  def scalatestJSDocTaskSetting: Setting[_]

  def crossBuildTestLibraryDependencies: sbt.Def.Initialize[Seq[sbt.ModuleID]]

  def scalatestTestOptions: Seq[sbt.TestOption]

  lazy val projectTitle = settingKey[String]("Name of project to display in doc titles")

  val javaSourceManaged: SettingKey[java.io.File] = sbt.SettingKey[java.io.File]("javaSourceManaged")  

}