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
import java.io.{File, FileWriter, BufferedWriter}

object GenCommonTestDotty {

  private def uncommentJsExport(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
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

  def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  def copyFiles(sourceDirName: String, packageDirName: String, targetDir: File, files: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    files.map { sourceFileName =>
      val sourceFile = new File(sourceDir, sourceFileName)
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFile(sourceFile, destFile)

      destFile
    }
  }

  private def uncommentJsExportJS(line: String): String =
    if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//DOTTY-ONLY "))
      line.substring(line.indexOf("//DOTTY-ONLY ") + 13)
    else if (line.trim.startsWith("//SCALATESTJS,NATIVE-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS,NATIVE-ONLY ") + 26)
    else if (line.trim.startsWith("//SCALATESTJS-ONLY "))
      line.substring(line.indexOf("//SCALATESTJS-ONLY ") + 19)  
    else
      line

  private def transformLineJS(line: String): String =
    uncommentJsExportJS(line)

  private def copyFileJS(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      var skipMode = false
      for (line <- lines) {
        if (line.trim == "// SKIP-DOTTY-START" || line.trim == "// SKIP-DOTTY-START")
          skipMode = true
        else if (line.trim == "// SKIP-DOTTY-END" || line.trim == "// SKIP-DOTTY-END")
          skipMode = false
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-START" || line.trim == "// SKIP-SCALATESTJS-START")
          skipMode = true
        else if (line.trim == "// SKIP-SCALATESTJS,NATIVE-END" || line.trim == "// SKIP-SCALATESTJS-END")
          skipMode = false  
        else if (!skipMode) {
          destWriter.write(transformLineJS(line))
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

  def copyDirJS(sourceDirName: String, packageDirName: String, targetDir: File, skipList: List[String]): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.filter(f => f.isFile && !skipList.contains(f.getName) && (f.getName.endsWith(".scala") || f.getName.endsWith(".java"))).map { sourceFile =>
      val destFile = new File(packageDir, sourceFile.getName)
      if (!destFile.exists || sourceFile.lastModified > destFile.lastModified)
        copyFileJS(sourceFile, destFile)

      destFile
    }
  }

  def genMain(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDir("jvm/common-test/src/main/scala/org/scalatest", "org/scalatest", targetDir, 
      List(
        "LineNumberHelper.scala", 
        "LineNumberMacro.scala"
      )
    ) ++
    copyDir("jvm/common-test/src/main/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty)
  }

  def genMainJS(targetDir: File, version: String, scalaVersion: String): Seq[File] = {
    copyDirJS("jvm/common-test/src/main/scala/org/scalatest", "org/scalatest", targetDir,
      List(
        "LineNumberHelper.scala", 
        "LineNumberMacro.scala", 
        "TestConcurrentDistributor.scala"
      )
    ) ++
    copyDirJS("dotty/common-test/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty) ++
    copyDirJS("jvm/common-test/src/main/scala/org/scalatest/path", "org/scalatest/path", targetDir, List.empty) ++ 
    copyDirJS("js/common-test/src/main/scala/org/scalatest", "org/scalatest", targetDir, List.empty)
  }

  /*copyFiles("jvm/common-test/src/main/scala/org/scalatest", "org/scalatest",
      List(
        "SharedHelpers.scala",
        "mytags.scala",
        "StubReporter.scala",
        "LineNumberMacro.scala",
        "LineNumberHelper.scala",
        "ReturnsNormallyThrowsAssertion.scala",
        "BookPropertyMatchers.scala",
        "EmptyMocks.scala",
        "FileMocks.scala",
        "StringFixture.scala"
      ), targetDir) ++
    copyFiles("jvm/common-test/src/main/scala/org/scalatest/path", "org/scalatest/path",
      List("ExampleLikeSpecs.scala"), targetDir) ++
    copyDir("jvm/common-test/src/main/scala/org/scalatest/enablers", "org/scalatest/enablers", targetDir, List.empty) ++
    copyDir("jvm/common-test/src/main/scala/org/scalatest/prop", "org/scalatest/prop", targetDir, List.empty)*/

}
