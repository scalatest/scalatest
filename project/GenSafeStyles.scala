/*
* Copyright 2001-2011 Artima, Inc.
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

import java.io.{FileWriter, BufferedWriter, File}
import sbt._
import scala.io.Source

object GenSafeStyles {

  def translateLine(traitName: String)(line: String): String =
    line.replaceAllLiterally("with TestRegistration", "with SafeTestRegistration")
        .replaceAllLiterally("Any /* Assertion */", "Assertion")
        .replaceAllLiterally(traitName, "Safe" + traitName)
        .replaceAllLiterally("Resources.concurrentSafe" + traitName + "Mod", "Resources.concurrent" + traitName + "Mod")
        .replaceAllLiterally("Resources.concurrentFixtureSafe" + traitName + "Mod", "Resources.concurrentFixture" + traitName + "Mod")
        //.replaceAllLiterally("final override val styleName: String = \"org.scalatest.Safe" + traitName + "\"", "final override val styleName: String = \"org.scalatest." + traitName + "\"")
        .replaceAllLiterally("@Finders(Array(\"org.scalatest.finders.Safe" + traitName + "Finder\"))", "@Finders(Array(\"org.scalatest.finders." + traitName + "Finder\"))")

  def translateTestLine(traitName: String)(line: String): String =
    line.replaceAllLiterally(traitName, "Safe" + traitName)
    .replaceAllLiterally("/* ASSERTION_SUCCEED */", "succeed")

  def translateFile(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, scalaJS: Boolean, translateFun: String => String): Unit = {
    val outputFile = new File(targetDir, fileName)
    val outputWriter = new BufferedWriter(new FileWriter(outputFile))
    try {
      val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
      var skipMode = false
      for (line <- lines) {
        val mustLine: String =
          if (scalaJS) {
            if (line.trim == "// SKIP-SCALATESTJS-START") {
              skipMode = true
              ""
            }
            else if (line.trim == "// SKIP-SCALATESTJS-END") {
              skipMode = false
              ""
            }
            else if (!skipMode) {
              if (line.trim.startsWith("//SCALATESTJS-ONLY "))
                translateFun(line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19))
              else
                translateFun(line)
            }
            else
              ""
          }
          else
            translateFun(line)

        outputWriter.write(mustLine)
        outputWriter.newLine()
      }
    }
    finally {
      outputWriter.flush()
      outputWriter.close()
      println("Generated " + outputFile.getAbsolutePath)
    }
  }

  def genFeatureSpecMainImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val scalatestDir = new File(targetDir, "org/scalatest")
    scalatestDir.mkdirs()

    translateFile(scalatestDir, "SafeFeatureSpecLike.scala", "scalatest-featurespec/src/main/scala/org/scalatest/FeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))
    translateFile(scalatestDir, "SafeFeatureSpec.scala", "scalatest-featurespec/src/main/scala/org/scalatest/FeatureSpec.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))

    val fixtureDir = new File(scalatestDir, "fixture")
    fixtureDir.mkdirs()

    translateFile(fixtureDir, "SafeFeatureSpecLike.scala", "scalatest-featurespec/src/main/scala/org/scalatest/fixture/FeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))
    translateFile(fixtureDir, "SafeFeatureSpec.scala", "scalatest-featurespec/src/main/scala/org/scalatest/fixture/FeatureSpec.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))

    IO.copy(
      List(
        (new File("scalatest-featurespec/src/main/scala/org/scalatest/AsyncFeatureSpecLike.scala"), new File(scalatestDir, "AsyncFeatureSpecLike.scala")),
        (new File("scalatest-featurespec/src/main/scala/org/scalatest/AsyncFeatureSpec.scala"), new File(scalatestDir, "AsyncFeatureSpec.scala")),
        (new File("scalatest-featurespec/src/main/scala/org/scalatest/fixture/AsyncFeatureSpecLike.scala"), new File(fixtureDir, "AsyncFeatureSpecLike.scala")),
        (new File("scalatest-featurespec/src/main/scala/org/scalatest/fixture/AsyncFeatureSpec.scala"), new File(fixtureDir, "AsyncFeatureSpec.scala"))
      ),
      true
    )
  }

  def genFeatureSpecMain(targetDir: File, version: String, scalaVersion: String) = genFeatureSpecMainImpl(targetDir, version, scalaVersion, false)

  def genFeatureSpecMainJS(targetDir: File, version: String, scalaVersion: String) = genFeatureSpecMainImpl(targetDir, version, scalaVersion, true)

  def genFeatureSpecTestImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val scalatestDir = new File(targetDir, "org/scalatest")
    scalatestDir.mkdirs()

    translateFile(scalatestDir, "SafeFeatureSpecSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/FeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FeatureSpec"))
    translateFile(scalatestDir, "AsyncFeatureSpecLikeSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/AsyncFeatureSpecLikeSpec.scala", scalaVersion, scalaJS, s => s)
    translateFile(scalatestDir, "AsyncFeatureSpecLikeSpec2.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/AsyncFeatureSpecLikeSpec2.scala", scalaVersion, scalaJS, s => s)
    translateFile(scalatestDir, "AsyncFeatureSpecSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/AsyncFeatureSpecSpec.scala", scalaVersion, scalaJS, s => s)
    translateFile(scalatestDir, "AsyncFeatureSpecSpec2.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/AsyncFeatureSpecSpec2.scala", scalaVersion, scalaJS, s => s)

    val fixtureDir = new File(scalatestDir, "fixture")
    fixtureDir.mkdirs()

    translateFile(fixtureDir, "SafeFeatureSpecSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/fixture/FeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FeatureSpec"))
    translateFile(fixtureDir, "AsyncFeatureSpecLikeSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/fixture/AsyncFeatureSpecLikeSpec.scala", scalaVersion, scalaJS, s => s)
    translateFile(fixtureDir, "AsyncFeatureSpecLikeSpec2.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/fixture/AsyncFeatureSpecLikeSpec2.scala", scalaVersion, scalaJS, s => s)
    translateFile(fixtureDir, "AsyncFeatureSpecSpec.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/fixture/AsyncFeatureSpecSpec.scala", scalaVersion, scalaJS, s => s)
    translateFile(fixtureDir, "AsyncFeatureSpecSpec2.scala", "scalatest-featurespec-test/src/test/scala/org/scalatest/fixture/AsyncFeatureSpecSpec2.scala", scalaVersion, scalaJS, s => s)
  }

  def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String) = genFeatureSpecTestImpl(targetDir, version, scalaVersion, false)

  def genFeatureSpecTestJS(targetDir: File, version: String, scalaVersion: String) = genFeatureSpecTestImpl(targetDir, version, scalaVersion, true)

  def genMainImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val safeDir = new File(targetDir, "safe")
    safeDir.mkdirs()

    translateFile(safeDir, "SafeTestRegistration.scala", "scalatest-core/src/main/scala/org/scalatest/TestRegistration.scala", scalaVersion, scalaJS, translateLine("TestRegistration"))

    translateFile(safeDir, "SafeFunSuiteLike.scala", "scalatest-core/src/main/scala/org/scalatest/FunSuiteLike.scala", scalaVersion, scalaJS, translateLine("FunSuite"))
    translateFile(safeDir, "SafeFunSuite.scala", "scalatest-core/src/main/scala/org/scalatest/FunSuite.scala", scalaVersion, scalaJS, translateLine("FunSuite"))

    translateFile(safeDir, "SafeFlatSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/FlatSpecLike.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))
    translateFile(safeDir, "SafeFlatSpec.scala", "scalatest-core/src/main/scala/org/scalatest/FlatSpec.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))

    translateFile(safeDir, "SafeFreeSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/FreeSpecLike.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))
    translateFile(safeDir, "SafeFreeSpec.scala", "scalatest-core/src/main/scala/org/scalatest/FreeSpec.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))

    translateFile(safeDir, "SafeFunSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/FunSpecLike.scala", scalaVersion, scalaJS, translateLine("FunSpec"))
    translateFile(safeDir, "SafeFunSpec.scala", "scalatest-core/src/main/scala/org/scalatest/FunSpec.scala", scalaVersion, scalaJS, translateLine("FunSpec"))

    translateFile(safeDir, "SafePropSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/PropSpecLike.scala", scalaVersion, scalaJS, translateLine("PropSpec"))
    translateFile(safeDir, "SafePropSpec.scala", "scalatest-core/src/main/scala/org/scalatest/PropSpec.scala", scalaVersion, scalaJS, translateLine("PropSpec"))

    translateFile(safeDir, "SafeWordSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/WordSpecLike.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
    translateFile(safeDir, "SafeWordSpec.scala", "scalatest-core/src/main/scala/org/scalatest/WordSpec.scala", scalaVersion, scalaJS, translateLine("WordSpec"))

    val safeFixtureDir = new File(targetDir, "safefixture")
    safeFixtureDir.mkdirs()

    translateFile(safeFixtureDir, "SafeTestRegistration.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/TestRegistration.scala", scalaVersion, scalaJS, translateLine("TestRegistration"))

    translateFile(safeFixtureDir, "SafeFunSuiteLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FunSuiteLike.scala", scalaVersion, scalaJS, translateLine("FunSuite"))
    translateFile(safeFixtureDir, "SafeFunSuite.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FunSuite.scala", scalaVersion, scalaJS, translateLine("FunSuite"))

    translateFile(safeFixtureDir, "SafeFlatSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FlatSpecLike.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))
    translateFile(safeFixtureDir, "SafeFlatSpec.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FlatSpec.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))

    translateFile(safeFixtureDir, "SafeFreeSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FreeSpecLike.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))
    translateFile(safeFixtureDir, "SafeFreeSpec.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FreeSpec.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))

    translateFile(safeFixtureDir, "SafeFunSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FunSpecLike.scala", scalaVersion, scalaJS, translateLine("FunSpec"))
    translateFile(safeFixtureDir, "SafeFunSpec.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/FunSpec.scala", scalaVersion, scalaJS, translateLine("FunSpec"))

    translateFile(safeFixtureDir, "SafePropSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/PropSpecLike.scala", scalaVersion, scalaJS, translateLine("PropSpec"))
    translateFile(safeFixtureDir, "SafePropSpec.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/PropSpec.scala", scalaVersion, scalaJS, translateLine("PropSpec"))

    translateFile(safeFixtureDir, "SafeWordSpecLike.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/WordSpecLike.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
    translateFile(safeFixtureDir, "SafeWordSpec.scala", "scalatest-core/src/main/scala/org/scalatest/fixture/WordSpec.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
  }

  def genMain(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, false)
  }

  def genMainForScalaJS(targetDir: File, version: String, scalaVersion: String) {
    genMainImpl(targetDir, version, scalaVersion, true)
  }

  def genTestImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val safeDir = new File(targetDir, "safe")
    safeDir.mkdirs()

    translateFile(safeDir, "SafeFunSuiteSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSuite"))
    translateFile(safeDir, "SafeFunSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))
    translateFile(safeDir, "SafeFunSpecSuite.scala", "scalatest-test/src/test/scala/org/scalatest/FunSpecSuite.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))

    translateFile(safeDir, "SafeFlatSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FlatSpec"))
    translateFile(safeDir, "SafeFreeSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FreeSpec"))
    translateFile(safeDir, "SafePropSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/PropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("PropSpec"))
    translateFile(safeDir, "SafeWordSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/WordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("WordSpec"))

    val fixtureDir = new File(safeDir, "fixture")
    fixtureDir.mkdirs()

    translateFile(fixtureDir, "SafeFunSuiteSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSuite"))
    translateFile(fixtureDir, "SafeFunSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))

    translateFile(fixtureDir, "SafeFlatSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FlatSpec"))
    translateFile(fixtureDir, "SafeFreeSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FreeSpec"))
    translateFile(fixtureDir, "SafePropSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/PropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("PropSpec"))
    translateFile(fixtureDir, "SafeWordSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/WordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("WordSpec"))
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Unit = {
    genTestImpl(targetDir, version, scalaVersion, false)
  }
}
