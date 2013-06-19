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

object GenLoneElement {
  
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
    val targetDir = new File(targetBaseDir, "lone")
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
      
    val mapMapping = 
      List(
        "List\\(10\\).loneElement" -> "Map(10 -> 10).loneElement._1", 
        "List\\(8\\).loneElement" -> "Map(8 -> 8).loneElement._1", 
        "List\\(10, 12\\)" -> "Map(10 -> 10, 12 -> 12)", 
        "List.empty\\[Int\\]" -> "Map.empty[Int, Int]", 
        "xs.loneElement" -> "xs.loneElement._1", 
        "List" -> "Map"
      )
      
    val javaColMapping = 
      List(
        "ListLoneElementSpec" -> "JavaColLoneElementSpec", 
        "List" -> "javaList", 
        "javaList.empty\\[Int\\]" -> "javaList[Int]()" // as List has been replaced with javaList above
      )
      
    val javaMapMapping = 
      List(
        "List\\(10\\).loneElement" -> "javaMap(10 -> 10).entrySet.loneElement.getKey", 
        "List\\(8\\).loneElement" -> "javaMap(8 -> 8).entrySet.loneElement.getKey", 
        "List\\(10, 12\\)" -> "javaMap(10 -> 10, 12 -> 12).entrySet", 
        "List.empty\\[Int\\]" -> "javaMap[Int, Int]().entrySet", 
        "xs.loneElement" -> "xs.loneElement.getKey", 
        "List" -> "JavaMap"
      )
      
    generateFile("ListLoneElementSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListLoneElementSpec.scala", "Set", setMapping: _*)
    generateFile("ListLoneElementSpec.scala", "Map", mapMapping: _*)
    generateFile("ListLoneElementSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListLoneElementSpec.scala", "JavaMap", javaMapMapping: _*)
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}
