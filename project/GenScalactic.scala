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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenScalactic {

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

  def genMain(targetDir: File, version: String, scalaVersion: String) {

    val scalacticPackageDir = new File(targetDir, "org/scalactic")
    scalacticPackageDir.mkdirs()
    val scalacticSourceDir = new File("src/main/scala/org/scalactic")
    scalacticSourceDir.listFiles.foreach { sourceFile =>
      val destFile = new File(scalacticPackageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }
    GenVersions.genMain(scalacticPackageDir, version, scalaVersion)

    val scalautilsPackageDir = new File(targetDir, "org/scalautils")
    scalautilsPackageDir.mkdirs()
    val scalautilsSourceDir = new File("src/main/scala/org/scalautils")
    scalautilsSourceDir.listFiles.foreach { sourceFile =>
      val destFile = new File(scalautilsPackageDir, sourceFile.getName)
      copyFile(sourceFile, destFile)
    }

    val sourceResourceFile = new File("src/main/resources/org/scalactic/ScalacticBundle.properties")
    val destResourceDir = new File(targetDir.getParentFile, "resources/org/scalactic")
    destResourceDir.mkdirs()
    val destResourceFile = new File(destResourceDir, "ScalacticBundle.properties")
    copyFile(sourceResourceFile, destResourceFile)

    val sourceCssFile = new File("src/main/html/addl.css")
    val destCssDir = new File(targetDir.getParentFile, "html")
    destCssDir.mkdirs()
    val destCssFile = new File(destCssDir, "addl.css")
    copyFile(sourceCssFile, destCssFile)
  }

  def genTest(targetDir: File, version: String, scalaVersion: String) {
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
  }


}