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

object GenSorted {
  
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
    val sortedDir = new File(targetBaseDir, "sorted")
    sortedDir.mkdirs()
    
    def generateFile(sourceFileName: String, generatedFileName: String, mapping: (String, String)*) {
      val generatedFile = new File(sortedDir, generatedFileName)
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
        "List\\[String\\]" -> "Array[String]", 
        "List\\[Int\\]" -> "Array[Int]", 
        "List" -> "Array", 
        "Nil" -> "Array()", 
        "ShouldBeSortedSpec" -> "ShouldBeSortedForArraySpec", 
        "ShouldBeSortedLogicalAndSpec" -> "ShouldBeSortedLogicalAndForArraySpec", 
        "ShouldBeSortedLogicalOrSpec" -> "ShouldBeSortedLogicalOrForArraySpec"
      )
      
    val javaListMapping = 
      List(
        "//ADDITIONAL//" -> "import SharedHelpers.javaList", 
        "List\\[String\\]" -> "java.util.List[String]", 
        "List\\[Int\\]" -> "java.util.List[Int]", 
        "List.empty\\[Int\\]" -> "javaList[Int]()", 
        "List.empty\\[Student\\]" -> "javaList[Student]()",
        "List\\(0\\)" -> "javaList(0)",
        "List\\(1, 2, 3\\)" -> "javaList(1, 2, 3)",
        "List\\(3, 2, 1\\)" -> "javaList(3, 2, 1)", 
        "List\\(Student" -> "javaList(Student", 
        "ShouldBeSortedSpec" -> "ShouldBeSortedForJavaColSpec", 
        "ShouldBeSortedLogicalAndSpec" -> "ShouldBeSortedLogicalAndForJavaColSpec",
        "ShouldBeSortedLogicalOrSpec" -> "ShouldBeSortedLogicalOrForJavaColSpec"
      )
    
    generateFile("ShouldBeSortedSpec.scala", "ShouldBeSortedForArraySpec.scala", arrayMapping: _*)
    generateFile("ShouldBeSortedLogicalAndSpec.scala", "ShouldBeSortedLogicalAndForArraySpec.scala", arrayMapping: _*)
    generateFile("ShouldBeSortedLogicalOrSpec.scala", "ShouldBeSortedLogicalOrForArraySpec.scala", arrayMapping: _*)
    
    generateFile("ShouldBeSortedSpec.scala", "ShouldBeSortedForJavaColSpec.scala", javaListMapping: _*)
    generateFile("ShouldBeSortedLogicalAndSpec.scala", "ShouldBeSortedLogicalAndForJavaColSpec.scala", javaListMapping: _*)
    generateFile("ShouldBeSortedLogicalOrSpec.scala", "ShouldBeSortedLogicalOrForJavaColSpec.scala", javaListMapping: _*)
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}