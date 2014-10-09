/*
* Copyright 2001-2013 Artima, Inc.
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

import scala.annotation.tailrec
import io.Source
import java.io.{File, FileWriter, BufferedWriter}

object GenContain2 extends GenContainBase {

  def genTest(targetBaseDir: File, version: String, scalaVersion: String) {

    val sourceBaseDir = new File("src/test/scala/org/scalatest")
    val containDir = new File(targetBaseDir, "containTests")
    containDir.mkdirs()

    def generateFile(sourceFileName: String, typeName: String, mapping: (String, String)*) {
      val generatedFileName = sourceFileName.replaceAll("List", typeName)
      val generatedFile = new File(containDir, generatedFileName)
      val writer = new BufferedWriter(new FileWriter(generatedFile))
      try {
        val lines = Source.fromFile(new File(sourceBaseDir, sourceFileName)).getLines().toList // for 2.8
        for (line <- lines) {
          val generatedLine = translateLine(line, mapping.toList: _*)
          writer.write(generatedLine.toString)
          writer.newLine() // add for 2.8
        }
      }
      finally {
        writer.close()
        println("Generated " + generatedFile.getAbsolutePath)
      }
    }

    // Generate tests for theSameElementsInOrderAs
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for only
    generateFile("ListShouldContainOnlySpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOnlyLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOnlyLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOnlySpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOnlyLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOnlyLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOnlySpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOnlyLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOnlyLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOnlySpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOnlyLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOnlyLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOnlySpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOnlyLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOnlyLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for allOf
    generateFile("ListShouldContainAllOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAllOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAllOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for allElementsOf
    generateFile("ListShouldContainAllElementsOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAllElementsOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAllElementsOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAllElementsOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAllElementsOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAllElementsOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for inOrderOnly
    generateFile("ListShouldContainInOrderOnlySpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for inOrder
    generateFile("ListShouldContainInOrderSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "Set", sortedSetMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "Map", sortedMapMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "JavaMap", sortedJavaMapMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "JavaSet", javaSortedSetMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for atMostOneOf
    generateFile("ListShouldContainAtMostOneOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtMostOneOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtMostOneOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtMostOneOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtMostOneOfLogicalOrSpec.scala", "String", stringMapping: _*)
  }

  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetDir + "/org/scalatest/"), version, scalaVersion)
  }
}
