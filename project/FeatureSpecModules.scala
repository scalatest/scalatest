/*
* Copyright 2001-2015 Artima, Inc.
*
* Licensed under the Apache License, Version 2.0 (the "License");
* you may not use this file except in compliance with the License.
* You may obtain a copy of the License at
*
*     http://www.apache.org/licenses/LICENSE-2.0
*
* Unless required by applicable law or agreed to in writing, software
* distributed under the License is distributed on an "AS IS" BASIS,
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
* See the License for the specific language governing permissions and
* limitations under the License.
*/

import com.typesafe.sbt.osgi.SbtOsgi._
import sbt.Keys._
import sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

trait FeatureSpecModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestTestOptions: Seq[Tests.Argument]

  def scalatestTestJSOptions: Seq[Tests.Argument]

  def scalatestCore: Project

  def scalatestCoreJS: Project

  def scalatestLibraryDependencies: Seq[ModuleID]

  def scalatestJSLibraryDependencies: Seq[ModuleID]

  def commonTest: Project

  def commonTestJS: Project

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File]

  val genSafeStylesTask: Def.Setting[Task[Unit]]

  def scalacticJS: Project

  def scalatestAll: Project

  def root: Project

  lazy val scalatestFeatureSpec = Project("scalatestFeatureSpec", file("scalatest-featurespec"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec"
    )
    .settings(osgiSettings: _*)
    .settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.fixture"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "org.scalactic.*",
        "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest FeatureSpec",
        "Bundle-Description" -> "ScalaTest FeatureSpec is style trait of ScalaTest, an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore).aggregate(LocalProject("scalatestFeatureSpecTest"))

  lazy val scalatestFeatureSpecJS = Project("scalatestFeatureSpecJS", file("scalatest-featurespec.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genFeatureSpecMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + root.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      test in Test := {},
      testOnly in Test := {}
    )
    .settings(osgiSettings: _*)
    .settings(
      OsgiKeys.exportPackage := Seq(
        "org.scalatest",
        "org.scalatest.fixture"
      ),
      OsgiKeys.importPackage := Seq(
        "org.scalatest.*",
        "org.scalactic.*",
        "scala.util.parsing.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.xml.*;version=\"$<range;[==,=+);$<replace;1.0.4;-;.>>\"",
        "scala.*;version=\"$<range;[==,=+);$<replace;"+scalaBinaryVersion.value+";-;.>>\"",
        "*;resolution:=optional"
      ),
      OsgiKeys.additionalHeaders:= Map(
        "Bundle-Name" -> "ScalaTest Feature Spec",
        "Bundle-Description" -> "ScalaTest FeatureSpec is style trait of ScalaTest, an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS).aggregate(LocalProject("scalatestFeatureSpecTestJS")).enablePlugins(ScalaJSPlugin)

  lazy val scalatestFeatureSpecTests = Project("scalatestFeatureSpecTest", file("scalatest-featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestOptions,
      libraryDependencies ++= scalatestLibraryDependencies,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestFeatureSpec % "test", commonTest % "test")

  lazy val scalatestFeatureSpecTestsJS = Project("scalatestFeatureSpecTestJS", file("scalatest-featurespec-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestJSOptions,
      libraryDependencies ++= scalatestJSLibraryDependencies,
      jsDependencies += RuntimeDOM % "test",
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genFeatureSpecTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestFeatureSpecJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  /*lazy val scalatestSafeFeatureSpec = Project("scalatestSafeFeatureSpec", file("scalatest-featurespec-safe"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      genSafeStylesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genFeatureSpecMain)
    ).dependsOn(scalatestCore).aggregate(LocalProject("scalatestSafeFeatureSpecTest"))

  lazy val scalatestSafeFeatureSpecJS = Project("scalatestSafeFeatureSpecJS", file("scalatest-featurespec-safe.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      genSafeStylesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genFeatureSpecMainJS),
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + scalatestAll.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      test in Test := {},
      testOnly in Test := {}
    ).dependsOn(scalatestCoreJS).aggregate(LocalProject("scalatestSafeFeatureSpecTestJS")).enablePlugins(ScalaJSPlugin)

  lazy val scalatestSafeFeatureSpecTest = Project("scalatestSafeFeatureSpecTest", file("scalatest-featurespec-safe-test"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestOptions,
      libraryDependencies ++= scalatestLibraryDependencies,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      genSafeStylesTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genFeatureSpecTest)
    ).dependsOn(scalatestSafeFeatureSpec % "test", commonTest % "test")

  lazy val scalatestSafeFeatureSpecTestJS = Project("scalatestSafeFeatureSpecTestJS", file("scalatest-featurespec-safe-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestJSOptions,
      libraryDependencies ++= scalatestJSLibraryDependencies,
      jsDependencies += RuntimeDOM % "test",
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      genSafeStylesTask,
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genFeatureSpecTestJS)
    ).dependsOn(scalatestSafeFeatureSpecJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)*/

}