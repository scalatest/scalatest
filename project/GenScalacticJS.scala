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

import scala.io.Source
import java.io.{BufferedWriter, File, FileWriter}
import sbt.IO

import GenCompatibleClasses.generatorSource

object GenScalacticJS {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTJS-ONLY "))
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
        if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTJS-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTJS-END")
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

  def copyFiles(sourceDirName: String, packageDirName: String, files: List[String], targetDir: File): Seq[File] = {
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
    copyDir("scalactic/src/main/scala/org/scalactic", "org/scalactic", targetDir, List.empty) ++
    copyDir("scalactic/src/main/scala/org/scalactic/exceptions", "org/scalactic/exceptions", targetDir, List.empty) ++
    copyDir("scalactic/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir, List("ObjectMeta.scala")) ++
    copyDir("scalactic/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty) ++
    GenVersions.genScalacticVersions(new File(targetDir, "org/scalactic"), version, scalaVersion)

  def genMacroScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic-macro/src/main/scala/org/scalactic", "org/scalactic", targetDir, List.empty) ++
    copyDir("scalactic-macro/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty) ++
    copyDir("scalactic-macro/src/main/scala/org/scalactic/source", "org/scalactic/source", targetDir, List.empty)

  def genResource(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
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
        "TripleEqualsSpec.for210"
      )) ++
    copyDir("scalactic-test/src/test/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty) ++
    copyDir("scalactic-test/src/test/scala/org/scalactic/source", "org/scalactic/source", targetDir, List.empty)

}
