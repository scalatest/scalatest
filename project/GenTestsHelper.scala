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

import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenTestsHelper {

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

  private def copyFile(targetBaseDir: File, filePath: String): Unit = {
    val source = new File(filePath)
    val targetDir = new File(targetBaseDir, source.getPath.substring("src/test/scala/".length, source.getPath.lastIndexOf("/")))
    targetDir.mkdirs()
    val target = new File(targetDir, source.getName)
    copyFile(source, target)
  }

  val fileList: List[String] =
    List(
      "src/test/scala/org/scalatest/SharedHelpers.scala",
      "src/test/scala/org/scalatest/FruitMocks.scala",
      "src/test/scala/org/scalatest/BookPropertyMatchers.scala",
      "src/test/scala/org/scalatest/EmptyMocks.scala",
      "src/test/scala/org/scalatest/ReturnsNormallyThrowsAssertion.scala",
      "src/test/scala/org/scalatest/OperatorNames.scala",
      "src/test/scala/org/scalatest/FileMocks.scala",
      "src/test/scala/org/scalatest/StubReporter.scala",
      "src/test/scala/org/scalatest/mytags.scala",
      "src/test/scala/org/scalatest/path/ExampleLikeSpecs.scala"
    )

  def genTest(targetDir: File, version: String, scalaVersion: String): Unit = {
    fileList.foreach { filePath =>
      copyFile(targetDir, filePath)
    }
  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetDir + "/org/scalatest/"), version, scalaVersion)
  }

}