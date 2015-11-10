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

trait FlatSpecModules {

  def sharedSettings: Seq[Setting[_]]

  def scalatestCore: Project

  def scalatestCoreJS: Project

  def root: Project

  lazy val scalatestFlatSpec = Project("scalatestFlatSpec", file("scalatest-flatspec"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-flatspec"
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
        "Bundle-Name" -> "ScalaTest FlatSpec",
        "Bundle-Description" -> "ScalaTest FlatSpec is style trait of ScalaTest, an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCore)/*.aggregate(LocalProject("scalatestFlatSpecTest"))*/

  lazy val scalatestFlatSpecJS = Project("scalatestFlatSpecJS", file("scalatest-flatspec.js"))
    .settings(sharedSettings: _*)
    .settings(
      organization := "org.scalatest",
      moduleName := "scalatest-flatspec",
      sourceGenerators in Compile += {
        Def.task {
          GenScalaTestJS.genFlatSpecMain((sourceManaged in Compile).value / "scala", version.value, scalaVersion.value)
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
        "Bundle-Name" -> "ScalaTest FlatSpec",
        "Bundle-Description" -> "ScalaTest FlatSpec is style trait of ScalaTest, an open-source test framework for the Java Platform designed to increase your productivity by letting you write fewer lines of test code that more clearly reveal your intent.",
        "Bundle-DocURL" -> "http://www.scalatest.org/",
        "Bundle-Vendor" -> "Artima, Inc."
      )
    ).dependsOn(scalatestCoreJS)/*.aggregate(LocalProject("scalatestFlatSpecTestJS"))*/.enablePlugins(ScalaJSPlugin)

}