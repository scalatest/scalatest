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
import java.io.{BufferedWriter, File, FileWriter}
import sbt.IO

import GenCompatibleClasses.generatorSource

object GenScalacticDotty {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else
      line

  private def rewrite213(line: String): String =
    line.replaceAllLiterally("final def startsWith(that: GenSeq[Char]): Boolean = theString.startsWith(that)", "final def startsWith(that: GenSeq[Char]): Boolean = theString.startsWith(that.mkString)")
        .replaceAllLiterally("final def startsWith(that: Every[Char]): Boolean = theString.startsWith(that.toVector)", "final def startsWith(that: Every[Char]): Boolean = theString.startsWith(that.mkString)")
        .replaceAllLiterally("""val fn: Int => Char = NonEmptyString("123").compose((idx: Int) => (idx + 1).toChar)""", """val fn: Int => Char = NonEmptyString("123").compose { case idx => (idx + 1).toChar }""")

  private def transformLine(line: String): String =
    ((uncommentJsExport _) andThen (rewrite213 _))(line)

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-DOTTY-START" || line.trim == "// SKIP-DOTTY-START")
          skipMode = true
        else if (line.trim == "// SKIP-DOTTY-END" || line.trim == "// SKIP-DOTTY-END")
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

  def copyFiles(sourceDirName: String, packageDirName: String, targetDir: File, files: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && f.getName.endsWith(".scala")).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified) {
        copyFile(sourceFile, destFile)
      }

      destFile
    }
  }

  def copyResourceDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName)).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        IO.copyFile(sourceFile, destFile)
      destFile
    }
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic/src/main/scala/org/scalactic", "org/scalactic", targetDir,
      List(
        "BooleanMacro.scala", // Re-implemented
        "Requirements.scala", // Re-implemented
        "Snapshots.scala"  // Re-implemented
      )
    ) ++
    copyDir("scalactic/src/main/scala/org/scalactic/exceptions", "org/scalactic/exceptions", targetDir, List.empty) ++
    copyDir("scalactic/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir,
      List(
        "Position.scala",  // Re-implemented
        "TypeInfo.scala"  // Re-implemented
      )) ++
    copyDir("scalactic/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty)

  def genMacroScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic-macro/src/main/scala/org/scalactic", "org/scalactic", targetDir,
      List(
        "BooleanMacro.scala", // Seems to be moved to scalactic already
        "MacroOwnerRepair.scala"  // No longer needed by the new macro?
      )
    ) ++
    //copyDir("scalactic-macro/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty) ++
    copyDir("scalactic-macro/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir,
      List(
        "PositionMacro.scala",  // Already reimplemented in Position.scala
        "TypeInfoMacro.scala",  // Already reimplemented
        "Position.scala"        // Already exists for scalactic-dotty
      )
    )

  def genResource(targetDir: File): Seq[File] = {
    val sourceResourceFile = new File("scalactic-macro/src/main/resources/org/scalactic/ScalacticBundle.properties")
    val destResourceDir = new File(targetDir, "org/scalactic")
    destResourceDir.mkdirs()
    val destResourceFile = new File(destResourceDir, "ScalacticBundle.properties")
    if (!destResourceFile.exists || sourceResourceFile.lastModified > destResourceFile.lastModified)
      copyFile(sourceResourceFile, destResourceFile)

    List(destResourceFile)
  }

  def genHtml(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyResourceDir("scalatest/src/main/html", "html", targetDir, List.empty)
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic-test/src/test/scala/org/scalactic", "org/scalactic", targetDir,
      List(
        "TripleEqualsSpec.for210",  // Old staff, we shall delete this soon.
        "FutureSugarSpec.scala"     // instability, occasional timeout in CI
      )) ++
    copyDir("scalactic-test/src/test/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir,
      List(
        "OddIntMacro.scala",  // not used, scala2 macros
        "OddInt.scala"        // not used, scala2 macros
      )) ++
    copyDir("scalactic-test/src/test/scala/org/scalactic/source", "org/scalactic/source", targetDir, List.empty)

}
