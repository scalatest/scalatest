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



  def genTest(targetDir: File, version: String, scalaVersion: String) {

    val sharedHelpersSource = new File("src/test/scala/org/scalatest/SharedHelpers.scala")
    val sharedHelpersTarget = new File(targetDir, sharedHelpersSource.getName)
    copyFile(sharedHelpersSource, sharedHelpersTarget)

    val fruitMocksSource = new File("src/test/scala/org/scalatest/FruitMocks.scala")
    val fruitMocksTarget = new File(targetDir, fruitMocksSource.getName)
    copyFile(fruitMocksSource, fruitMocksTarget)

    val bookPropertyMatchersSource = new File("src/test/scala/org/scalatest/BookPropertyMatchers.scala")
    val bookPropertyMatchersTarget = new File(targetDir, bookPropertyMatchersSource.getName)
    copyFile(bookPropertyMatchersSource, bookPropertyMatchersTarget)

    val emptyMocksSource = new File("src/test/scala/org/scalatest/EmptyMocks.scala")
    val emptyMocksTarget = new File(targetDir, emptyMocksSource.getName)
    copyFile(emptyMocksSource, emptyMocksTarget)

    val returnsNormallyThrowsAssertionSource = new File("src/test/scala/org/scalatest/ReturnsNormallyThrowsAssertion.scala")
    val returnsNormallyThrowsAssertionTarget = new File(targetDir, returnsNormallyThrowsAssertionSource.getName)
    copyFile(returnsNormallyThrowsAssertionSource, returnsNormallyThrowsAssertionTarget)

    val operatorNamesSource = new File("src/test/scala/org/scalatest/OperatorNames.scala")
    val operatorNamesTarget = new File(targetDir, operatorNamesSource.getName)
    copyFile(operatorNamesSource, operatorNamesTarget)

    val fileMocksSource = new File("src/test/scala/org/scalatest/FileMocks.scala")
    val fileMocksTarget = new File(targetDir, fileMocksSource.getName)
    copyFile(fileMocksSource, fileMocksTarget)

    val stubReporterSource = new File("src/test/scala/org/scalatest/StubReporter.scala")
    val stubReporterTarget = new File(targetDir, stubReporterSource.getName)
    copyFile(stubReporterSource, stubReporterTarget)

    val mytagsSource = new File("src/test/scala/org/scalatest/mytags.scala")
    val mytagsTarget = new File(targetDir, mytagsSource.getName)
    copyFile(mytagsSource, mytagsTarget)

    val pathExampleLikeSpecsSource = new File("src/test/scala/org/scalatest/path/ExampleLikeSpecs.scala")
    val pathExampleLikeSpecsTarget = new File(targetDir, pathExampleLikeSpecsSource.getName)
    copyFile(pathExampleLikeSpecsSource, pathExampleLikeSpecsTarget)

  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetDir + "/org/scalatest/"), version, scalaVersion)
  }

}