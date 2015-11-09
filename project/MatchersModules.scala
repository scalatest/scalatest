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

trait MatchersModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestCore: Project

  def scalatestCoreJS: Project

  def root: Project

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File]

  val genFactories = TaskKey[Unit]("genfactories", "Generate Matcher Factories")
  val genFactoriesTask = genFactories <<= (sourceManaged in Compile, sourceManaged in Test, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, theVersion: String, theScalaVersion: String) =>
    GenFactories.genMain(new File(mainTargetDir, "scala/genfactories"), theVersion, theScalaVersion)
  }

  lazy val scalatestMatchersCore = Project("scalatestMatchersCore", file("scalatest-matchers-core"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-matchers-core",
      genFactoriesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMain)
    ).dependsOn(scalatestCore)

  lazy val scalatestMatchersCoreJS = Project("scalatestMatchersCoreJS", file("scalatest-matchers-core.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-matchers-core",
      genFactoriesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genfactories", "GenFactories.scala")(GenFactories.genMainJS),
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genMatchersCoreMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + root.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      test in Test := {},
      testOnly in Test := {}
    ).dependsOn(scalatestCoreJS).enablePlugins(ScalaJSPlugin)

  lazy val scalatestMatchers = Project("scalatestMatchers", file("scalatest-matchers"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-matchers"
    ).dependsOn(scalatestMatchersCore)

  lazy val scalatestMatchersJS = Project("scalatestMatchersJS", file("scalatest-matchers.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-matchers",
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genMatchersMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + root.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      test in Test := {},
      testOnly in Test := {}
    ).dependsOn(scalatestMatchersCoreJS).enablePlugins(ScalaJSPlugin)

  val genMustMatchers = TaskKey[Unit]("genmatchers", "Generate Must Matchers")
  val genMustMatchersTask = genMustMatchers <<= (sourceManaged in Compile, sourceManaged in Test, name, version, scalaVersion) map { (mainTargetDir: File, testTargetDir: File, projName: String, theVersion: String, theScalaVersion: String) =>
    GenMatchers.genMain(new File(mainTargetDir, "scala/genmatchers"), theVersion, theScalaVersion)
  }

  lazy val scalatestMustMatchers = Project("scalatestMustMatchers", file("scalatest-mustmatchers"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-mustmatchers",
      genMustMatchersTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMain)
    ).dependsOn(scalatestMatchersCore)

  lazy val scalatestMustMatchersJS = Project("scalatestMustMatchersJS", file("scalatest-mustmatchers.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-mustmatchers",
      genMustMatchersTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("genmatchers", "MustMatchers.scala")(GenMatchers.genMainForScalaJS),
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genMatchersMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
        }.taskValue
      },
      scalacOptions ++= Seq("-P:scalajs:mapSourceURI:" + root.base.toURI + "->https://raw.githubusercontent.com/scalatest/scalatest/v" + version.value + "/"),
      test in Test := {},
      testOnly in Test := {}
    ).dependsOn(scalatestMatchersCoreJS).enablePlugins(ScalaJSPlugin)

}