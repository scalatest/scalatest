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

object GenSafeStyles {

  val generatorSource = new File("GenSafeStyles.scala")

  def translateLine(traitName: String)(line: String): String =
    line.replaceAllLiterally("Any /* Assertion */", "Assertion")
        .replaceAllLiterally(traitName, (if (traitName.startsWith("Any")) traitName.drop(3) else traitName))
        .replaceAllLiterally("Resources.concurrentSafe" + traitName + "Mod", "Resources.concurrent" + traitName + "Mod")
        .replaceAllLiterally("Resources.concurrentFixtureSafe" + traitName + "Mod", "Resources.concurrentFixture" + traitName + "Mod")
        .replaceAllLiterally("@Finders(Array(\"org.scalatest.finders.Safe" + traitName + "Finder\"))", "@Finders(Array(\"org.scalatest.finders." + traitName + "Finder\"))")
        .replaceAllLiterally("SafeTestRegistrationClosedException", "TestRegistrationClosedException")

  def translateTestLine(traitName: String)(line: String): String =
    line.replaceAllLiterally(traitName, traitName.drop(3))
    .replaceAllLiterally("/* ASSERTION_SUCCEED */", "succeed")

  def translateFile(targetDir: File, fileName: String, sourceFileName: String, scalaVersion: String, scalaJS: Boolean, translateFun: String => String): File = {
    val outputFile = new File(targetDir, fileName)
    if (!outputFile.exists || generatorSource.lastModified > outputFile.lastModified) {
      val outputWriter = new BufferedWriter(new FileWriter(outputFile))
      try {
        val lines = Source.fromFile(new File(sourceFileName)).getLines.toList
        var skipMode = false
        for (line <- lines) {
          val mustLine: String =
            if (scalaJS) {
              if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START") {
                skipMode = true
                ""
              }
              else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END") {
                skipMode = false
                ""
              }
              else if (!skipMode) {
                if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
                  translateFun(line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26))
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
    outputFile
  }

  def genFunSuite(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FunSuiteLike.scala", "jvm/funsuite/src/main/scala/org/scalatest/funsuite/AnyFunSuiteLike.scala", scalaVersion, scalaJS, translateLine("AnyFunSuite")),
      translateFile(targetDir, "FunSuite.scala", "jvm/funsuite/src/main/scala/org/scalatest/funsuite/AnyFunSuite.scala", scalaVersion, scalaJS, translateLine("AnyFunSuite")),
      translateFile(targetDir, "FixtureFunSuiteLike.scala", "jvm/funsuite/src/main/scala/org/scalatest/funsuite/FixtureAnyFunSuiteLike.scala", scalaVersion, scalaJS, translateLine("AnyFunSuite")),
      translateFile(targetDir, "FixtureFunSuite.scala", "jvm/funsuite/src/main/scala/org/scalatest/funsuite/FixtureAnyFunSuite.scala", scalaVersion, scalaJS, translateLine("AnyFunSuite")),
    )
  }

  def genFeatureSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FeatureSpecLike.scala", "jvm/featurespec/src/main/scala/org/scalatest/featurespec/AnyFeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFeatureSpec")),
      translateFile(targetDir, "FeatureSpec.scala", "jvm/featurespec/src/main/scala/org/scalatest/featurespec/AnyFeatureSpec.scala", scalaVersion, scalaJS, translateLine("AnyFeatureSpec")),
      translateFile(targetDir, "FixtureFeatureSpecLike.scala", "jvm/featurespec/src/main/scala/org/scalatest/featurespec/FixtureAnyFeatureSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFeatureSpec")),
      translateFile(targetDir, "FixtureFeatureSpec.scala", "jvm/featurespec/src/main/scala/org/scalatest/featurespec/FixtureAnyFeatureSpec.scala", scalaVersion, scalaJS, translateLine("AnyFeatureSpec")),
    )
  }

  def genFlatSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FlatSpecLike.scala", "jvm/flatspec/src/main/scala/org/scalatest/flatspec/AnyFlatSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFlatSpec")),
      translateFile(targetDir, "FlatSpec.scala", "jvm/flatspec/src/main/scala/org/scalatest/flatspec/AnyFlatSpec.scala", scalaVersion, scalaJS, translateLine("AnyFlatSpec")),
      translateFile(targetDir, "FixtureFlatSpecLike.scala", "jvm/flatspec/src/main/scala/org/scalatest/flatspec/FixtureAnyFlatSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFlatSpec")),
      translateFile(targetDir, "FixtureFlatSpec.scala", "jvm/flatspec/src/main/scala/org/scalatest/flatspec/FixtureAnyFlatSpec.scala", scalaVersion, scalaJS, translateLine("AnyFlatSpec")),
    )
  }

  def genFreeSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FreeSpecLike.scala", "jvm/freespec/src/main/scala/org/scalatest/freespec/AnyFreeSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFreeSpec")),
      translateFile(targetDir, "FreeSpec.scala", "jvm/freespec/src/main/scala/org/scalatest/freespec/AnyFreeSpec.scala", scalaVersion, scalaJS, translateLine("AnyFreeSpec")),
      translateFile(targetDir, "FixtureFreeSpecLike.scala", "jvm/freespec/src/main/scala/org/scalatest/freespec/FixtureAnyFreeSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFreeSpec")),
      translateFile(targetDir, "FixtureFreeSpec.scala", "jvm/freespec/src/main/scala/org/scalatest/freespec/FixtureAnyFreeSpec.scala", scalaVersion, scalaJS, translateLine("AnyFreeSpec")),
    )
  }

  def genFunSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FunSpecLike.scala", "jvm/funspec/src/main/scala/org/scalatest/funspec/AnyFunSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFunSpec")),
      translateFile(targetDir, "FunSpec.scala", "jvm/funspec/src/main/scala/org/scalatest/funspec/AnyFunSpec.scala", scalaVersion, scalaJS, translateLine("AnyFunSpec")),
      translateFile(targetDir, "FixtureFunSpecLike.scala", "jvm/funspec/src/main/scala/org/scalatest/funspec/FixtureAnyFunSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyFunSpec")),
      translateFile(targetDir, "FixtureFunSpec.scala", "jvm/funspec/src/main/scala/org/scalatest/funspec/FixtureAnyFunSpec.scala", scalaVersion, scalaJS, translateLine("AnyFunSpec")),
    )
  }

  def genPropSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "PropSpecLike.scala", "jvm/propspec/src/main/scala/org/scalatest/propspec/AnyPropSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyPropSpec")),
      translateFile(targetDir, "PropSpec.scala", "jvm/propspec/src/main/scala/org/scalatest/propspec/AnyPropSpec.scala", scalaVersion, scalaJS, translateLine("AnyPropSpec")),
      translateFile(targetDir, "FixturePropSpecLike.scala", "jvm/propspec/src/main/scala/org/scalatest/propspec/FixtureAnyPropSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyPropSpec")),
      translateFile(targetDir, "FixturePropSpec.scala", "jvm/propspec/src/main/scala/org/scalatest/propspec/FixtureAnyPropSpec.scala", scalaVersion, scalaJS, translateLine("AnyPropSpec")),
    )
  }

  def genWordSpec(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "WordSpecLike.scala", "jvm/wordspec/src/main/scala/org/scalatest/wordspec/AnyWordSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyWordSpec")),
      translateFile(targetDir, "WordSpec.scala", "jvm/wordspec/src/main/scala/org/scalatest/wordspec/AnyWordSpec.scala", scalaVersion, scalaJS, translateLine("AnyWordSpec")),
      translateFile(targetDir, "FixtureWordSpecLike.scala", "jvm/wordspec/src/main/scala/org/scalatest/wordspec/FixtureAnyWordSpecLike.scala", scalaVersion, scalaJS, translateLine("AnyWordSpec")),
      translateFile(targetDir, "FixtureWordSpec.scala", "jvm/wordspec/src/main/scala/org/scalatest/wordspec/FixtureAnyWordSpec.scala", scalaVersion, scalaJS, translateLine("AnyWordSpec"))
    )
  }

  def genFunSuiteTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FunSuiteSpec.scala", "jvm/funsuite-test/src/test/scala/org/scalatest/funsuite/AnyFunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFunSuite")),
      translateFile(targetDir, "FixtureFunSuiteSpec.scala", "jvm/funsuite-test/src/test/scala/org/scalatest/funsuite/FixtureAnyFunSuiteSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFunSuite")),
    )
  }

  def genFunSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FunSpecSpec.scala", "jvm/funspec-test/src/test/scala/org/scalatest/funspec/AnyFunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFunSpec")),
      translateFile(targetDir, "FunSpecSuite.scala", "jvm/funspec-test/src/test/scala/org/scalatest/funspec/AnyFunSpecSuite.scala", scalaVersion, scalaJS, translateTestLine("AnyFunSpec")),
      translateFile(targetDir, "FixtureFunSpecSpec.scala", "jvm/funspec-test/src/test/scala/org/scalatest/funspec/FixtureAnyFunSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFunSpec")),
    )
  }

  def genFeatureSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FeatureSpecSpec.scala", "jvm/featurespec-test/src/test/scala/org/scalatest/featurespec/AnyFeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFeatureSpec")),
      translateFile(targetDir, "FixtureFeatureSpecSpec.scala", "jvm/featurespec-test/src/test/scala/org/scalatest/featurespec/FixtureAnyFeatureSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFeatureSpec")),
    )
  }

  def genFlatSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FlatSpecSpec.scala", "jvm/flatspec-test/src/test/scala/org/scalatest/flatspec/AnyFlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFlatSpec")),
      translateFile(targetDir, "FixtureFlatSpecSpec.scala", "jvm/flatspec-test/src/test/scala/org/scalatest/flatspec/FixtureAnyFlatSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFlatSpec")),
    )
  }

  def genFreeSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "FreeSpecSpec.scala", "jvm/freespec-test/src/test/scala/org/scalatest/freespec/AnyFreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFreeSpec")),
      translateFile(targetDir, "FixtureFreeSpecSpec.scala", "jvm/freespec-test/src/test/scala/org/scalatest/freespec/FixtureAnyFreeSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyFreeSpec")),
    )
  }

  def genPropSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "PropSpecSpec.scala", "jvm/propspec-test/src/test/scala/org/scalatest/propspec/AnyPropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyPropSpec")),
      translateFile(targetDir, "FixturePropSpecSpec.scala", "jvm/propspec-test/src/test/scala/org/scalatest/propspec/FixtureAnyPropSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyPropSpec")),
    )
  }

  def genWordSpecTest(targetDir: File, version: String, scalaVersion: String, scalaJS: Boolean): Seq[File] = {
    targetDir.mkdirs()

    Seq(
      translateFile(targetDir, "WordSpecSpec.scala", "jvm/wordspec-test/src/test/scala/org/scalatest/wordspec/AnyWordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyWordSpec")),
      translateFile(targetDir, "FixtureWordSpecSpec.scala", "jvm/wordspec-test/src/test/scala/org/scalatest/wordspec/FixtureAnyWordSpecSpec.scala", scalaVersion, scalaJS, translateTestLine("AnyWordSpec"))
    )
  }
}
