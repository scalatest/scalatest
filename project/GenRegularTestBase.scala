/*
* Copyright 2001-2014 Artima, Inc.
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

import collection.mutable.ListBuffer
import io.Source
import java.io.{File, FileWriter, BufferedWriter}

trait GenRegularTestsBase {

  def includeFile(file: File): Boolean
  val baseDirPath: String
  val name: String

  def copyFile(inputFile: File, outputFile: File): File = {
    if (!outputFile.exists || inputFile.lastModified > outputFile.lastModified) {
      val writer = new BufferedWriter(new FileWriter(outputFile))
      try {
        val inputLines = Source.fromFile(inputFile).getLines().toList // for 2.8
        for (line <- inputLines) {
          writer.write(line.toString)
          writer.newLine() // add for 2.8
        }
      }
      finally {
        writer.flush()
        writer.close()
        println("Generated " + outputFile.getAbsolutePath)
      }
    }
    outputFile
  }

  def copyFile(targetBaseDir: File, filePath: String): File = {
    val source = new File(filePath)
    val targetDir = new File(targetBaseDir, source.getPath.substring("scalatest-test/src/test/scala/".length, source.getPath.lastIndexOf("/")))
    targetDir.mkdirs()
    val target = new File(targetDir, source.getName)
    copyFile(source, target)
  }

  def processDir(dir: File, targetDir: File): Seq[File] = {
    targetDir.mkdirs()
    val (sourceFiles, subDirs) = dir.listFiles.partition(_.isFile)
    sourceFiles.flatMap { sourceFile =>
      val sourceFileName = sourceFile.getName
      if (sourceFileName.endsWith(".scala") && includeFile(sourceFile)) {
        val outputFile = new File(targetDir, sourceFileName)
        Seq(copyFile(new File(dir, sourceFileName), outputFile))
      }
      else
        Seq.empty[File]
    } ++
    subDirs.flatMap { subDir =>
      processDir(subDir, new File(targetDir, subDir.getName))
    }
  }

  def processJavaDir(dir: File, targetDir: File): Seq[File] = {
    targetDir.mkdirs()
    val (sourceFiles, subDirs) = dir.listFiles.partition(_.isFile)
    sourceFiles.flatMap { sourceFile =>
      val sourceFileName = sourceFile.getName
      if (sourceFileName.endsWith(".java")) {
        val sourceFile = new File(dir, sourceFileName)
        val outputFile = new File(targetDir, sourceFileName)
        if (!outputFile.exists || sourceFile.lastModified > outputFile.lastModified)
          copyFile(sourceFile, outputFile)
        Seq(outputFile)
      }
      else
        Seq.empty[File]
    } ++
    subDirs.flatMap { subDir =>
      processJavaDir(subDir, new File(targetDir, subDir.getName))
    }
  }

  def copyJavaDir(targetBaseDir: File): Seq[File] = {
    processJavaDir(new File("scalatest-test/src/test/java"), targetBaseDir)
  }

  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File] = {
    val sourceBaseDir = new File(baseDirPath)
    val regularDir = new File(targetBaseDir, "/org/scalatest/")

    processDir(sourceBaseDir, regularDir)
  }
}