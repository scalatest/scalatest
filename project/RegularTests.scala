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

import sbt.Keys._
import sbt._
import org.scalajs.sbtplugin.ScalaJSPlugin
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport._

trait RegularTests {

  def sharedSettings: Seq[Setting[_]]

  def scalacheckDependency(config: String): ModuleID

  def scalacticMacro: Project

  def scalacticMacroJS: Project

  def scalactic: Project

  def scalatest: Project

  def scalacticJS: Project

  def scalatestJS: Project

  def scalacheckVersion: String

  def crossBuildLibraryDependencies(theScalaVersion: String): Seq[ModuleID]

  def scalatestLibraryDependencies: Seq[ModuleID]

  def scalatestTestOptions: Seq[Tests.Argument]

  def scalatestJSLibraryDependencies: Seq[ModuleID]

  def scalatestTestJSOptions: Seq[Tests.Argument]

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File]

  // Common test classes used by scalactic and scalatest
  lazy val commonTest = Project("common-test", file("common-test"))
    .settings(sharedSettings: _*)
    .settings(
      libraryDependencies += scalacheckDependency("optional")
    ).dependsOn(scalacticMacro, LocalProject("scalatest"), LocalProject("scalatestFeatureSpec"))

  // Common test classes used by scalactic.js and scalatest.js
  lazy val commonTestJS = Project("commonTestJS", file("common-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      libraryDependencies += scalacheckDependency("optional"),
      sourceGenerators in Compile += {
        Def.task{
          GenCommonTestJS.genMain((sourceManaged in Compile).value / "scala" / "org" / "scalatest", version.value, scalaVersion.value)
        }.taskValue
      }
    ).dependsOn(scalacticMacroJS, LocalProject("scalatestJS")).enablePlugins(ScalaJSPlugin)

  lazy val scalacticTest = Project("scalactic-test", file("scalactic-test"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalactic",
      libraryDependencies += scalacheckDependency("test"),
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalactic, scalatest % "test", commonTest % "test")

  lazy val scalacticTestJS = Project("scalacticTestJS", file("scalactic-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalactic",
      jsDependencies += RuntimeDOM % "test",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test",
      //scalaJSStage in Global := FastOptStage,
      //postLinkJSEnv := PhantomJSEnv().value,
      //postLinkJSEnv := NodeJSEnv(executable = "node").value,
      sourceGenerators in Test += {
        Def.task {
          GenScalacticJS.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalacticJS, scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

  lazy val scalatestTest = Project("scalatest-test", file("scalatest-test"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestLibraryDependencies,
      testOptions in Test := scalatestTestOptions,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatest % "test", commonTest % "test")

  lazy val scalatestTestJS = Project("scalatestTestJS", file("scalatest-test.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      libraryDependencies ++= crossBuildLibraryDependencies(scalaVersion.value),
      libraryDependencies ++= scalatestJSLibraryDependencies,
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion % "test",
      jsDependencies += RuntimeDOM % "test",
      //scalaJSStage in Global := FastOptStage,
      //postLinkJSEnv := PhantomJSEnv().value,
      //postLinkJSEnv := NodeJSEnv(executable = "node").value,
      testOptions in Test := scalatestTestJSOptions,
      publishArtifact := false,
      publish := {},
      publishLocal := {},
      sourceGenerators in Test += {
        Def.task {
          GenScalaTestJS.genTest((sourceManaged in Test).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("gengen", "GenGen.scala")(GenGen.genTest),
      sourceGenerators in Test <+=
        (baseDirectory, sourceManaged in Test, version, scalaVersion) map genFiles("genmatchers", "GenMustMatchersTests.scala")(GenMustMatchersTests.genTestForScalaJS)
    ).dependsOn(scalatestJS % "test", commonTestJS % "test").enablePlugins(ScalaJSPlugin)

}