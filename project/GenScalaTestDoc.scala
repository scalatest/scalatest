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

object GenScalaTestDoc {

  private def copyFile(sourceFile: File, destFile: File): File = {
    val destWriter = new BufferedWriter(new FileWriter(destFile))
    try {
      val lines = Source.fromFile(sourceFile).getLines.toList
      for (line <- lines) {
        destWriter.write(line)
        destWriter.newLine()
      }
      destFile
    }
    finally {
      destWriter.flush()
      destWriter.close()
      println("Copied " + destFile.getAbsolutePath)
    }
  }

  def copyDir(sourceDirName: String, packageDirName: String, targetDir: File, extension: String): Seq[File] = {
    val packageDir = new File(targetDir, packageDirName)
    packageDir.mkdirs()
    val sourceDir = new File(sourceDirName)
    sourceDir.listFiles.toList.flatMap { sourceFile =>
      if (sourceFile.isFile) {
        if (sourceFile.getName.endsWith(extension)) {
          val destFile = new File(packageDir, sourceFile.getName)
          if (!destFile.exists || sourceFile.lastModified > destFile.lastModified) {
            copyFile(sourceFile, destFile)
          }
          List(destFile)
        }
        else
          List.empty[File]
      }
      else {
        val packageName = sourceFile.getName
        copyDir(sourceDirName + File.separator + packageName, packageDirName + File.separator + packageName, targetDir, extension)  
      }
    }
  }

  def genScala(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("jvm/core/src/main/scala/org/scalatest", "org/scalatest", targetDir, ".scala") ++ 
    copyDir("jvm/diagrams/src/main/scala/org/scalatest/diagrams", "org/scalatest/diagrams", targetDir, ".scala") ++
    copyDir("jvm/expectations/src/main/scala/org/scalatest/expectations", "org/scalatest/expectations", targetDir, ".scala") ++
    copyDir("jvm/featurespec/src/main/scala/org/scalatest/featurespec", "org/scalatest/featurespec", targetDir, ".scala") ++
    copyDir("jvm/flatspec/src/main/scala/org/scalatest/flatspec", "org/scalatest/flatspec", targetDir, ".scala") ++ 
    copyDir("jvm/freespec/src/main/scala/org/scalatest/freespec", "org/scalatest/freespec", targetDir, ".scala") ++
    copyDir("jvm/funspec/src/main/scala/org/scalatest/funspec", "org/scalatest/funspec", targetDir, ".scala") ++
    copyDir("jvm/funsuite/src/main/scala/org/scalatest/funsuite", "org/scalatest/funsuite", targetDir, ".scala") ++
    copyDir("jvm/propspec/src/main/scala/org/scalatest/propspec", "org/scalatest/propspec", targetDir, ".scala") ++
    copyDir("jvm/refspec/src/main/scala/org/scalatest/refspec", "org/scalatest/refspec", targetDir, ".scala") ++
    copyDir("jvm/wordspec/src/main/scala/org/scalatest/wordspec", "org/scalatest/wordspec", targetDir, ".scala") ++
    copyDir("jvm/matchers-core/src/main/scala/org/scalatest/matchers", "org/scalatest/matchers", targetDir, ".scala") ++ 
    copyDir("jvm/shouldmatchers/src/main/scala/org/scalatest/matchers/should", "org/scalatest/matchers/should", targetDir, ".scala") 

  def genJava(targetDir: File, version: String, scalaVersion: String): Seq[File] =
    copyDir("jvm/core/src/main/java/org/scalatest", "org/scalatest/", targetDir, ".java") ++
    copyDir("jvm/compatible/src/main/java/org/scalatest", "org/scalatest/", targetDir, ".java")

}
