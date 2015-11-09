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

trait JMockModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestCore: Project

  def scalatestTestOptions: Seq[Tests.Argument]

  def scalatestLibraryDependencies: Seq[ModuleID]

  def commonTest: Project

  def jMockVersion: String

  lazy val scalatestJMock = Project("scalatestJMock", file("scalatest-jmock"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-jmock",
      libraryDependencies += "org.jmock" % "jmock-legacy" % jMockVersion
    ).dependsOn(scalatestCore).aggregate(LocalProject("scalatestJMockTest"))

  lazy val scalatestJMockTest = Project("scalatestJMockTest", file("scalatest-jmock-test"))
    .settings(sharedSettings: _*)
    .settings(
      testOptions in Test := scalatestTestOptions,
      libraryDependencies ++= scalatestLibraryDependencies,
      publishArtifact := false,
      publish := {},
      publishLocal := {}
    ).dependsOn(scalatestJMock % "test", commonTest % "test")


}