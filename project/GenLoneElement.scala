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

  val generatorSource = new File("GenLoneElement.scala")
  
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
  
  def genTest(targetBaseDir: File, version: String, scalaVersion: String): Seq[File] = {
    
    val sourceBaseDir = new File("scalatest-test/src/test/scala/org/scalatest")

    targetBaseDir.mkdirs()
    
    def generateFile(sourceFileName: String, typeName: String, mapping: (String, String)*): File = {
      val generatedFileName = sourceFileName.replaceAll("List", typeName)
      val generatedFile = new File(targetBaseDir, generatedFileName)
      if (!generatedFile.exists || generatorSource.lastModified > generatedFile.lastModified) {
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
          writer.flush()
          writer.close()
          println("Generated " + generatedFile.getAbsolutePath)
        }
      }
      generatedFile
    }
    
    val arrayMapping = 
      List(
        "List" -> "Array"
      )
      
    val setMapping = 
      List(
        "List" -> "Set",
        "xs.loneElement\\(1\\) === 2" -> "xs.loneElement(1) === true"
      )
      
    val mapMapping = 
      List(
        "List\\(10\\).loneElement" -> "Map(10 -> 10).loneElement._1", 
        "List\\(8\\).loneElement" -> "Map(8 -> 8).loneElement._1", 
        "List\\(10, 12\\)" -> "Map(10 -> 10, 12 -> 12)", 
        "List.empty\\[Int\\]" -> "Map.empty[Int, Int]",
        "List\\(List\\(1, 2, 3\\)\\)" -> "Map(Map(1 -> 2, 2 -> 3, 3 -> 4) -> 0)",
        "xs.loneElement" -> "xs.loneElement._1", 
        "List" -> "Map"
      )
      
    val javaColMapping = 
      List(
        "ListLoneElementSpec" -> "JavaColLoneElementSpec", 
        "List" -> "javaList",
        "xs.loneElement\\(1\\)" -> "xs.loneElement.get\\(1\\)",
        "javaList.empty\\[Int\\]" -> "javaList[Int]()" // as List has been replaced with javaList above
      )
      
    val javaMapMapping = 
      List(
        "List\\(10\\).loneElement" -> "javaMap(Entry(10, 10)).loneElement.getKey", 
        "List\\(8\\).loneElement" -> "javaMap(Entry(8, 8)).loneElement.getKey", 
        "List\\(10, 12\\)" -> "javaMap(Entry(10, 10), Entry(12, 12))", 
        "List.empty\\[Int\\]" -> "javaMap[Int, Int]()",
        "List\\(List\\(1, 2, 3\\)\\)" -> "javaMap(Entry(javaMap(Entry(1, 2), Entry(2, 3), Entry(3, 4)), 0))",
        "xs.loneElement" -> "xs.loneElement.getKey",
        "xs.loneElement.getKey\\(1\\)" -> "xs.loneElement.getKey.get(1)",
        "List" -> "JavaMap"
      )

    Seq(
      generateFile("ListLoneElementSpec.scala", "Array", arrayMapping: _*),
      generateFile("ListLoneElementSpec.scala", "Set", setMapping: _*),
      generateFile("ListLoneElementSpec.scala", "Map", mapMapping: _*),
      generateFile("ListLoneElementSpec.scala", "JavaCol", javaColMapping: _*),
      generateFile("ListLoneElementSpec.scala", "JavaMap", javaMapMapping: _*)
    )
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val version = args(1)
    val scalaVersion = args(2)
    genTest(new File(targetDir + "/org/scalatest/"), version, scalaVersion)
  }
}
