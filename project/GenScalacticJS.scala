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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalacticJS {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19)
    else
      line

  private def transformLine(line: String): String =
    uncommentJsExport(line)

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-SCALATESTJS-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS-END")
          skipMode = false
        else if (!skipMode) {
          destWriter.write(transformLine(line))
          destWriter.newLine()
        }
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, files: List[String], targetDir: File): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = {

    val scalacticPackageDir = new File(targetDir, "org/scalactic")
    scalacticPackageDir.mkdirs()
    val scalacticSourceDir = new File("scalactic/src/main/scala/org/scalactic")
    scalacticSourceDir.listFiles.map { sourceFile =>
      val destFile = new File(scalacticPackageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
      destFile
    } ++
    GenVersions.genScalacticVersions(scalacticPackageDir, version, scalaVersion)
  }

  def genMacroScala(targetDir: File, version: String, scalaVersion: String): Seq[File] = {

    val scalacticPackageDir = new File(targetDir, "org/scalactic")
    scalacticPackageDir.mkdirs()
    val scalacticSourceDir = new File("scalactic-macro/src/main/scala/org/scalactic")
    scalacticSourceDir.listFiles.flatMap { sourceFile =>
      if (sourceFile.isFile) {
        val destFile = new File(scalacticPackageDir, sourceFile.getName)
        copyFile(sourceFile, destFile)
        List(destFile)
      }
      else
        List.empty[File]
    }
  }

  def genResource(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val sourceResourceFile = new File("scalactic-macro/src/main/resources/org/scalactic/ScalacticBundle.properties")
    val destResourceDir = new File(targetDir.getParentFile, "resources/org/scalactic")
    destResourceDir.mkdirs()
    val destResourceFile = new File(destResourceDir, "ScalacticBundle.properties")
    copyFile(sourceResourceFile, destResourceFile)
    List(destResourceFile)
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("scalactic-test/src/test/scala/org/scalactic", "org/scalactic",
      List(
        "UnitSpec.scala",
        "AccumulationSpec.scala",
        "ChainSpec.scala",
        "CatcherSpec.scala",
        "ConversionCheckedMapEqualityConstraintsSpec.scala",
        "ConversionCheckedSeqEqualityConstraintsSpec.scala",
        "ConversionCheckedSetEqualityConstraintsSpec.scala",
        "ConversionCheckedTraversableEqualityConstraintsSpec.scala",
        "ConversionCheckedTripleEqualsExplicitlySpec.scala",
        "ConversionCheckedTripleEqualsSpec.scala",
        "ExplicitlySpecHelpers.scala",
        "DecidersSpec.scala",
        "DefaultEqualitySpec.scala",
        "EitherSugarSpec.scala",
        "EqualitySpec.scala",
        "EverySpec.scala",
        //"FutureSugarSpec.scala",
        "NormalizationSpec.scala",
        "NormalizingEqualitySpec.scala",
        "NormMethodsSpec.scala",
        //"NumericEqualityConstraintsSpec.scala",
        "OptionSugarSpec.scala",
        "OrSpec.scala",
        "PrettifierSpec.scala",
        "PrettyMethodsSpec.scala",
        "RequirementsSpec.scala",
        "SnapshotsSpec.scala",
        "SpreadSpec.scala",
        "StringNormalizationsSpec.scala",
        "TimesOnIntSpec.scala",
        "ToleranceSpec.scala",
        "TolerantEqualitySpec.scala",
        "TolerantEquivalenceSpec.scala",
        "TraversableConstraintsSpec.scala",
        "TripleEqualsSpec.scala"
      ), targetDir)
  }

}