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

object GenContain1 extends GenContainBase {
  
  def genTest(targetBaseDir: File, version: String, scalaVersion: String) {
    
    val sourceBaseDir = new File("scalatest-test/src/test/scala/org/scalatest")
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
    
    // Generate tests for atLeastOneOf
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for atLeastOneElementOf
    generateFile("ListShouldContainAtLeastOneElementOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainAtLeastOneElementOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for oneOf
    generateFile("ListShouldContainOneOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOneOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOneOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for oneElementOf
    generateFile("ListShouldContainOneElementOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainOneElementOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainOneElementOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainOneElementOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainOneElementOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainOneElementOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for noneOf
    generateFile("ListShouldContainNoneOfSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoneOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoneOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoneOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoneOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoneOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainNoneOfLogicalOrSpec.scala", "String", stringMapping: _*)

    // Generate tests for noElementsOf
    //generateFile("ListShouldContainNoElementsOfSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldContainNoElementsOfSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainNoElementsOfSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainNoElementsOfSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainNoElementsOfSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainNoElementsOfSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainNoElementsOfLogicalOrSpec.scala", "String", stringMapping: _*)
    
    // Generate tests for theSameElementsAs
    generateFile("ListShouldContainTheSameElementsAsSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainTheSameElementsAsLogicalOrSpec.scala", "String", stringMapping: _*)
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetDir + "/org/scalatest/"), version, scalaVersion)
  }
}
