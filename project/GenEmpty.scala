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

object GenEmpty {
  
  def translateLine(line: String, mapping: (String, String)*): String = {
    @tailrec
    def translate(current: String, itr: Iterator[(String, String)]): String = 
      if (itr.hasNext) {
        val next = itr.next
        translate(current.replaceAll(next._1, next._2), itr)
      }
      else
        current
    translate(line, mapping.toIterator)
  }
  
  def genTest(targetBaseDir: File, scalaVersion: String) {
    
    val sourceBaseDir = new File("src/test/scala/org/scalatest")
    val targetDir = new File(targetBaseDir, "empty")
    targetDir.mkdirs()
    
    def generateFile(sourceFileName: String, typeName: String, mapping: (String, String)*) {
      val generatedFileName = sourceFileName.replaceAll("List", typeName)
      val generatedFile = new File(targetDir, generatedFileName)
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
    
    val arrayMapping = 
      List(
        "List" -> "Array"
      )
      
    val setMapping = 
      List(
        "List" -> "Set"
      )
      
    val stringMapping = 
      List(
        "List\\(emptyList\\)" -> "Vector(emptyList)", 
        "List\\(nonEmptyList\\)" -> "Vector(nonEmptyList)", 
        "List\\(1, 2, 3\\)" -> "\"123\"", 
        "List.empty\\[Int\\]" -> "\"\"", 
        "List" -> "String"
      )
      
    val optionMapping = 
      List(
        "List\\(emptyList\\)" -> "Vector(emptyList)", 
        "List\\(nonEmptyList\\)" -> "Vector(nonEmptyList)", 
        "List\\(1, 2, 3\\)" -> "Some(123)", 
        "List.empty\\[Int\\]" -> "None", 
        "List" -> "Option"
      )
      
    val mapMapping = 
      List(
        "List\\(emptyList\\)" -> "Vector(emptyList)", 
        "List\\(nonEmptyList\\)" -> "Vector(nonEmptyList)", 
        "List\\(1, 2, 3\\)" -> "Map(1 -> 1, 2 -> 2, 3 -> 3)", 
        "List.empty\\[Int\\]" -> "Map.empty[Int, Int]", 
        "List" -> "Map"
      )
      
    val javaColMapping = 
      List(
        "ListShouldBeEmptySpec" -> "JavaColShouldBeEmptySpec",
        "ListShouldBeEmptyLogicalAndSpec" -> "JavaColShouldBeEmptyLogicalAndSpec",
        "ListShouldBeEmptyLogicalOrSpec" -> "JavaColShouldBeEmptyLogicalOrSpec",
        "List\\(emptyList\\)" -> "Vector(emptyList)", 
        "List\\(nonEmptyList\\)" -> "Vector(nonEmptyList)", 
        "List" -> "javaList", 
        "javaList.empty\\[Int\\]" -> "javaList[Int]()", 
        "//ADDITIONAL//" -> "import SharedHelpers.javaList"
      )
      
    val javaMapMapping = 
      List(
        "ListShouldBeEmptySpec" -> "JavaMapShouldBeEmptySpec",
        "ListShouldBeEmptyLogicalAndSpec" -> "JavaMapShouldBeEmptyLogicalAndSpec",
        "ListShouldBeEmptyLogicalOrSpec" -> "JavaMapShouldBeEmptyLogicalOrSpec",
        "List\\(emptyList\\)" -> "Vector(emptyList)", 
        "List\\(nonEmptyList\\)" -> "Vector(nonEmptyList)", 
        "List\\(1, 2, 3\\)" -> "javaMap(Entry(1, 1), Entry(2, 2), Entry(3, 3))", 
        "List.empty\\[Int\\]" -> "javaMap[Int, Int]()", 
        "List" -> "javaMap", 
        "//ADDITIONAL//" -> "import SharedHelpers.javaMap"
      )
      
    generateFile("ListShouldBeEmptySpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "Array", arrayMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "Set", setMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "Set", setMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "Set", setMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "String", stringMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "Option", optionMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "Option", optionMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "Map", mapMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    
    generateFile("ListShouldBeEmptySpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldBeEmptyLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldBeEmptyLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}
