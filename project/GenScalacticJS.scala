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
      copyFile(sourceFile, destFile)
    }
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic/src/main/scala/org/scalactic", "org/scalactic", targetDir, List.empty) ++
    copyDir("scalactic/src/main/scala/org/scalactic/enablers", "org/scalactic/enablers", targetDir, List.empty) ++
    copyDir("scalactic/src/main/scala/org/scalactic/algebra", "org/scalactic/algebra", targetDir, List.empty) ++
    GenVersions.genScalacticVersions(new File(targetDir, "org/scalactic"), version, scalaVersion)

  def genMacroScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic-macro/src/main/scala/org/scalactic", "org/scalactic", targetDir, List.empty) ++
    copyDir("scalactic-macro/src/main/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty)

  def genResource(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    val sourceResourceFile = new File("scalactic-macro/src/main/resources/org/scalactic/ScalacticBundle.properties")
    val destResourceDir = new File(targetDir.getParentFile, "resources/org/scalactic")
    destResourceDir.mkdirs()
    val destResourceFile = new File(destResourceDir, "ScalacticBundle.properties")
    copyFile(sourceResourceFile, destResourceFile)
    List(destResourceFile)
  }

  def genTest(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("scalactic-test/src/test/scala/org/scalactic", "org/scalactic", targetDir,
      List(
        "TripleEqualsSpec.for210"
      )) ++
    copyDir("scalactic-test/src/test/scala/org/scalactic/anyvals", "org/scalactic/anyvals", targetDir, List.empty)
    List.empty

}
