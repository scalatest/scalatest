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

import sbt._
import sbt.Keys._

trait JUnitModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestCore: Project

  def scalacticMacro: Project

  def scalatestTestOptions: Seq[Tests.Argument]

  def commonTest: Project

  def scalatestLibraryDependencies: Seq[ModuleID]

  def junitVersion: String

  lazy val scalatestJUnit = Project("scalatestJUnit", file("scalatest-junit"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-junit",
      libraryDependencies += "junit" % "junit" % junitVersion
    ).dependsOn(scalatestCore).aggregate(LocalProject("scalatestJUnitTest"))

  lazy val scalatestJUnitTest = Project("scalatestJUnitTest", file("scalatest-junit-test"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestOptions,
      libraryDependencies ++= scalatestLibraryDependencies,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestJUnit % "test", scalacticMacro % "test", commonTest % "test")

}