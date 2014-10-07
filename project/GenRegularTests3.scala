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

object GenRegularTests3 {

  def genTest(targetBaseDir: File, version: String, scalaVersion: String) {
    val sourceBaseDir = new File("src/test/scala/org/scalatest")
    val regularDir = new File(targetBaseDir, "regular1")

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

    def isShouldFile(file: File): Boolean =
      file.isFile && (file.getName.startsWith("ListShould") || file.getName.startsWith("EveryShould") || file.getName.startsWith("OptionShould"))

    def processDir(dir: File, targetDir: File): Unit = {
      targetDir.mkdirs()
      for (sourceFile <- dir.listFiles) {
        val sourceFileName = sourceFile.getName
        if (isShouldFile(sourceFile)) {
          val outputFile = new File(targetDir, sourceFileName)
          copyFile(new File(dir, sourceFileName), outputFile)
        }
        else if (sourceFile.isDirectory)
          processDir(sourceFile, new File(targetDir, sourceFileName))
      }
    }

    processDir(sourceBaseDir, regularDir)
  }
}