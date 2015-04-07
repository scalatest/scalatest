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

  private def copyFile(sourceFile: File, destFile: File) {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(line)
        destWriter.newLine()
      }
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
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

  /*def genTest(targetDir: File, version: String, scalaVersion: String) {
    val scalatestDir = new File(targetDir, "org/scalatest")
    scalatestDir.mkdirs()
    val sharedHelpersSourceFile = new File("src/test/scala/org/scalatest/SharedHelpers.scala")
    val sharedHelpersTargetFile = new File(scalatestDir, sharedHelpersSourceFile.getName)
    copyFile(sharedHelpersSourceFile, sharedHelpersTargetFile)

    val packageDir = new File(targetDir, "org/scalactic")
    packageDir.mkdirs()
    val sourceDir = new File("src/test/scala/org/scalactic")
    sourceDir.listFiles.foreach { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
  }*/


}