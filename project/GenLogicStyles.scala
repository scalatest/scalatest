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

import scala.io.Source

object GenLogicStyles {

  def translateLine(traitName: String)(line: String): String =
    line.replaceAllLiterally("with TestRegistration", "with LogicTestRegistration")
        .replaceAllLiterally("Any /* Assertion */", "org.scalatest.Assertion")
        .replaceAllLiterally(traitName, "Logic" + traitName)
        .replaceAllLiterally("Resources.concurrentLogic" + traitName + "Mod", "Resources.concurrent" + traitName + "Mod")
        .replaceAllLiterally("Resources.concurrentFixtureLogic" + traitName + "Mod", "Resources.concurrentFixture" + traitName + "Mod")
        .replaceAllLiterally("@Finders(Array(\"org.scalatest.finders.Logic" + traitName + "Finder\"))", "@Finders(Array(\"org.scalatest.finders." + traitName + "Finder\"))")

  def translateTestLine(traitName: String)(line: String): String =
    line.replaceAllLiterally(traitName, "Logic" + traitName)
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

  def genMainImpl(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Unit = {
    targetDir.mkdirs()
    val safeDir = new File(targetDir, "safe")
    safeDir.mkdirs()

    translateFile(safeDir, "LogicTestRegistration.scala", "scalatest/src/main/scala/org/scalatest/TestRegistration.scala", scalaVersion, scalaJS, translateLine("TestRegistration"))

    translateFile(safeDir, "LogicFunSuiteLike.scala", "scalatest/src/main/scala/org/scalatest/FunSuiteLike.scala", scalaVersion, scalaJS, translateLine("FunSuite"))
    translateFile(safeDir, "LogicFunSuite.scala", "scalatest/src/main/scala/org/scalatest/FunSuite.scala", scalaVersion, scalaJS, translateLine("FunSuite"))

    translateFile(safeDir, "LogicFeatureSpecLike.scala", "scalatest/src/main/scala/org/scalatest/FeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))
    translateFile(safeDir, "LogicFeatureSpec.scala", "scalatest/src/main/scala/org/scalatest/FeatureSpec.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))

    translateFile(safeDir, "LogicFlatSpecLike.scala", "scalatest/src/main/scala/org/scalatest/FlatSpecLike.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))
    translateFile(safeDir, "LogicFlatSpec.scala", "scalatest/src/main/scala/org/scalatest/FlatSpec.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))

    translateFile(safeDir, "LogicFreeSpecLike.scala", "scalatest/src/main/scala/org/scalatest/FreeSpecLike.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))
    translateFile(safeDir, "LogicFreeSpec.scala", "scalatest/src/main/scala/org/scalatest/FreeSpec.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))

    translateFile(safeDir, "LogicFunSpecLike.scala", "scalatest/src/main/scala/org/scalatest/FunSpecLike.scala", scalaVersion, scalaJS, translateLine("FunSpec"))
    translateFile(safeDir, "LogicFunSpec.scala", "scalatest/src/main/scala/org/scalatest/FunSpec.scala", scalaVersion, scalaJS, translateLine("FunSpec"))

    translateFile(safeDir, "LogicPropSpecLike.scala", "scalatest/src/main/scala/org/scalatest/PropSpecLike.scala", scalaVersion, scalaJS, translateLine("PropSpec"))
    translateFile(safeDir, "LogicPropSpec.scala", "scalatest/src/main/scala/org/scalatest/PropSpec.scala", scalaVersion, scalaJS, translateLine("PropSpec"))

    translateFile(safeDir, "LogicWordSpecLike.scala", "scalatest/src/main/scala/org/scalatest/WordSpecLike.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
    translateFile(safeDir, "LogicWordSpec.scala", "scalatest/src/main/scala/org/scalatest/WordSpec.scala", scalaVersion, scalaJS, translateLine("WordSpec"))

    val safeFixtureDir = new File(targetDir, "safefixture")
    safeFixtureDir.mkdirs()

    translateFile(safeFixtureDir, "LogicTestRegistration.scala", "scalatest/src/main/scala/org/scalatest/fixture/TestRegistration.scala", scalaVersion, scalaJS, translateLine("TestRegistration"))

    translateFile(safeFixtureDir, "LogicFunSuiteLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/FunSuiteLike.scala", scalaVersion, scalaJS, translateLine("FunSuite"))
    translateFile(safeFixtureDir, "LogicFunSuite.scala", "scalatest/src/main/scala/org/scalatest/fixture/FunSuite.scala", scalaVersion, scalaJS, translateLine("FunSuite"))

    translateFile(safeFixtureDir, "LogicFeatureSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/FeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))
    translateFile(safeFixtureDir, "LogicFeatureSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/FeatureSpec.scala", scalaVersion, scalaJS, translateLine("FeatureSpec"))

    translateFile(safeFixtureDir, "LogicFlatSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/FlatSpecLike.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))
    translateFile(safeFixtureDir, "LogicFlatSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/FlatSpec.scala", scalaVersion, scalaJS, translateLine("FlatSpec"))

    translateFile(safeFixtureDir, "LogicFreeSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/FreeSpecLike.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))
    translateFile(safeFixtureDir, "LogicFreeSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/FreeSpec.scala", scalaVersion, scalaJS, translateLine("FreeSpec"))

    translateFile(safeFixtureDir, "LogicFunSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/FunSpecLike.scala", scalaVersion, scalaJS, translateLine("FunSpec"))
    translateFile(safeFixtureDir, "LogicFunSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/FunSpec.scala", scalaVersion, scalaJS, translateLine("FunSpec"))

    translateFile(safeFixtureDir, "LogicPropSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/PropSpecLike.scala", scalaVersion, scalaJS, translateLine("PropSpec"))
    translateFile(safeFixtureDir, "LogicPropSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/PropSpec.scala", scalaVersion, scalaJS, translateLine("PropSpec"))

    translateFile(safeFixtureDir, "LogicWordSpecLike.scala", "scalatest/src/main/scala/org/scalatest/fixture/WordSpecLike.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
    translateFile(safeFixtureDir, "LogicWordSpec.scala", "scalatest/src/main/scala/org/scalatest/fixture/WordSpec.scala", scalaVersion, scalaJS, translateLine("WordSpec"))
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

    translateFile(safeDir, "LogicFunSuiteSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSuite"))
    translateFile(safeDir, "LogicFunSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))
    translateFile(safeDir, "LogicFunSpecSuite.scala", "scalatest-test/src/test/scala/org/scalatest/FunSpecSuite.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))
    translateFile(safeDir, "LogicFeatureSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FeatureSpec"))
    translateFile(safeDir, "LogicFlatSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FlatSpec"))
    translateFile(safeDir, "LogicFreeSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/FreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FreeSpec"))
    translateFile(safeDir, "LogicPropSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/PropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("PropSpec"))
    translateFile(safeDir, "LogicWordSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/WordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("WordSpec"))

    val fixtureDir = new File(safeDir, "fixture")
    fixtureDir.mkdirs()

    translateFile(fixtureDir, "LogicFunSuiteSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSuite"))
    translateFile(fixtureDir, "LogicFunSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FunSpec"))
    translateFile(fixtureDir, "LogicFeatureSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FeatureSpec"))
    translateFile(fixtureDir, "LogicFlatSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FlatSpec"))
    translateFile(fixtureDir, "LogicFreeSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/FreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("FreeSpec"))
    translateFile(fixtureDir, "LogicPropSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/PropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("PropSpec"))
    translateFile(fixtureDir, "LogicWordSpecSpec.scala", "scalatest-test/src/test/scala/org/scalatest/fixture/WordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("WordSpec"))
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Unit = {
    genTestImpl(targetDir, version, scalaVersion, false)
  }
}
