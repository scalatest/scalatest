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
import java.io.File
import java.io.FileWriter
import java.io.BufferedWriter
import scala.io.Source
import scala.io.Codec

object DeprecatedShouldMatcherTestsHelper {

  implicit val codec = Codec.default
  
  def translateShouldToDeprecatedShould(shouldLine: String): String = {
    shouldLine.replaceAll("Should", "DeprecatedShould")
  }

  def generateFile(targetDir: String, srcFileName: String, targetFileName: String) {
    val matchersDir = new File(targetDir + "/main/scala/org/scalatest/matchers")
    matchersDir.mkdirs()
    val writer = new BufferedWriter(new FileWriter(targetDir + "/main/scala/org/scalatest/" + targetFileName))
    try {
      val shouldLines = Source.fromFile("src/main/scala/org/scalatest/" + srcFileName).getLines().toList
      for (shouldLine <- shouldLines) {
        val deprecatedShouldLine = translateShouldToDeprecatedShould(shouldLine)
        writer.write(deprecatedShouldLine)
        writer.newLine()
      }
    }
    finally {
      writer.close()
    }
  }
}

import DeprecatedShouldMatcherTestsHelper._

object GenDeprecatedShouldMatchersTests {

  def main(args: Array[String]) {
    val targetDir = args(0)
    val matchersDir = new File("gen/" + targetDir + "/test/scala/org/scalatest/matchers")
    matchersDir.mkdirs()
    val shouldFileNames = 
      List(
        "ShouldBehaveLikeSpec.scala",
        "ShouldContainElementSpec.scala",
        "ShouldContainKeySpec.scala",
        "ShouldContainValueSpec.scala",
        "ShouldEqualSpec.scala",
        "ShouldHavePropertiesSpec.scala",
        "ShouldLengthSpec.scala",
        "ShouldOrderedSpec.scala",
        "ShouldSizeSpec.scala",
        "ShouldBeASymbolSpec.scala",
        "ShouldBeAnSymbolSpec.scala",
        "ShouldBeMatcherSpec.scala",
        "ShouldBePropertyMatcherSpec.scala",
        "ShouldBeSymbolSpec.scala",
        "ShouldEndWithRegexSpec.scala",
        "ShouldEndWithSubstringSpec.scala",
        "ShouldFullyMatchSpec.scala",
        "ShouldIncludeRegexSpec.scala",
        "ShouldIncludeSubstringSpec.scala",
        "ShouldLogicalMatcherExprSpec.scala",
        "ShouldMatcherSpec.scala",
        "ShouldPlusOrMinusSpec.scala",
        "ShouldSameInstanceAsSpec.scala",
        "ShouldStartWithRegexSpec.scala",
        "ShouldStartWithSubstringSpec.scala",
        "ShouldBeNullSpec.scala"
      )

    for (shouldFileName <- shouldFileNames) {

      val deprecatedShouldFileName = shouldFileName.replace("Should", "DeprecatedShould")
      val writer = new BufferedWriter(new FileWriter("gen/" + targetDir + "/test/scala/org/scalatest/matchers/" + deprecatedShouldFileName))
      try {
        val shouldLines = Source.fromFile("src/test/scala/org/scalatest/matchers/" + shouldFileName).getLines().toList // for 2.8
        for (shouldLine <- shouldLines) {
          val deprecatedShouldLine = translateShouldToDeprecatedShould(shouldLine)
          writer.write(deprecatedShouldLine.toString)
          writer.newLine() // add for 2.8
        }
      }
      finally {
        writer.close()
      }
    }
  }
}

