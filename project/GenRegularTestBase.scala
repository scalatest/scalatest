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

  def copyFile(inputFile: File, outputFile: File) {
    val writer = new BufferedWriter(new FileWriter(outputFile))
    try {
      val inputLines = Source.fromFile(inputFile).getLines().toList // for 2.8
      for (line <- inputLines) {
        writer.write(line.toString)
        writer.newLine() // add for 2.8
      }
    }
    finally {
      writer.close()
      println("Generated " + outputFile.getAbsolutePath)
    }
  }

  def copyFile(targetBaseDir: File, filePath: String): Unit = {
    val source = new File(filePath)
    val targetDir = new File(targetBaseDir, source.getPath.substring("src/test/scala/".length, source.getPath.lastIndexOf("/")))
    targetDir.mkdirs()
    val target = new File(targetDir, source.getName)
    copyFile(source, target)
  }

  def processDir(dir: File, targetDir: File): Unit = {
    targetDir.mkdirs()
    for (sourceFile <- dir.listFiles) {
      val sourceFileName = sourceFile.getName
      if (!GenTestsHelper.fileList.contains(sourceFile.getPath) && includeFile(sourceFile)) {
        val outputFile = new File(targetDir, sourceFileName)
        copyFile(new File(dir, sourceFileName), outputFile)
      }
      else if (sourceFile.isDirectory)
        processDir(sourceFile, new File(targetDir, sourceFileName))
    }
  }

  def processJavaDir(dir: File, targetDir: File): Unit = {
    targetDir.mkdirs()
    for (sourceFile <- dir.listFiles) {
      val sourceFileName = sourceFile.getName
      if (sourceFile.isFile) {
        val outputFile = new File(targetDir, sourceFileName)
        copyFile(new File(dir, sourceFileName), outputFile)
      }
      else if (sourceFile.isDirectory)
        processJavaDir(sourceFile, new File(targetDir, sourceFileName))
    }
  }

  def copyJavaDir(targetBaseDir: File): Unit = {
    val testDir = targetBaseDir.getParentFile.getParentFile
    val javaDir = new File(testDir, "java")
    processJavaDir(new File("src/test/java"), new File(javaDir, name))
  }

  def genTest(targetBaseDir: File, version: String, scalaVersion: String) {
    val sourceBaseDir = new File(baseDirPath)
    val regularDir = new File(targetBaseDir, name + "/org/scalatest/")

    processDir(sourceBaseDir, regularDir)
  }
}