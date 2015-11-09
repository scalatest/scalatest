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

trait SeleniumModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestCore: Project

  lazy val scalatestSelenium = Project("scalatestSelenium", file("scalatest-selenium"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-selenium",
      libraryDependencies += "org.seleniumhq.selenium" % "selenium-java" % "2.35.0"
    ).dependsOn(scalatestCore)

}