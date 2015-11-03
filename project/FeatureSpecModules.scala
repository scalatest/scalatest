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

trait FeatureSpecModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestTestOptions: Seq[Tests.Argument]

  def scalatest: Project

  def scalatestLibraryDependencies: Seq[ModuleID]

  def commonTest: Project

  def genFiles(name: String, generatorSource: String)(gen: (File, String, String) => Unit)(basedir: File, outDir: File, theVersion: String, theScalaVersion: String): Seq[File]

  val genSafeStylesTask: Def.Setting[Task[Unit]]

  lazy val scalatestFeatureSpec = Project("scalatestFeatureSpec", file("scalatest-featurespec"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec"
    ).dependsOn(scalatest).aggregate(LocalProject("scalatestFeatureSpecTest"))

  lazy val scalatestFeatureSpecTests = Project("scalatestFeatureSpecTest", file("scalatest-featurespec-test"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestOptions,
      libraryDependencies ++= scalatestLibraryDependencies,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestFeatureSpec % "test", commonTest % "test")

  lazy val scalatestSafeFeatureSpec = Project("scalatestSafeFeatureSpec", file("scalatest-featurespec-safe"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-featurespec",
      genSafeStylesTask,
      sourceGenerators in Compile <+=
        (baseDirectory, sourceManaged in Compile, version, scalaVersion) map genFiles("gensafestyles", "GenSafeStyles.scala")(GenSafeStyles.genFeatureSpecMain)
    ).dependsOn(scalatest).aggregate(LocalProject("scalatestSafeFeatureSpecTest"))

  lazy val scalatestSafeFeatureSpecTests = Project("scalatestSafeFeatureSpecTest", file("scalatest-featurespec-safe-test"))
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

}