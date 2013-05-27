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

object GenContain {
  
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
    val containDir = new File(targetBaseDir, "contain")
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
    generateFile("ListShouldContainAtLeastOneOfSpec.scala", "Array", 
                 "ListShouldContainAtLeastOneOfSpec" -> "ArrayShouldContainAtLeastOneOfSpec", 
                     "List\\[String\\]" -> "Array[String]", 
                     "List\\[Int\\]" -> "Array[Int]", 
                     "List" -> "Array", 
                     "listsNil" -> "listsArray", 
                     "Nil" -> "Array()")
    generateFile("ListShouldContainAtLeastOneOfLogicalAndSpec.scala", "Array", 
                 "ListShouldContainAtLeastOneOfLogicalAndSpec" -> "ArrayShouldContainAtLeastOneOfLogicalAndSpec", 
                     "List\\[String\\]" -> "Array[String]", 
                     "List\\[Int\\]" -> "Array[Int]", 
                     "List" -> "Array", 
                     "listsNil" -> "listsArray", 
                     "Nil" -> "Array()")
    generateFile("ListShouldContainAtLeastOneOfLogicalOrSpec.scala", "Array", 
                 "ListShouldContainAtLeastOneOfLogicalOrSpec" -> "ArrayShouldContainAtLeastOneOfLogicalOrSpec", 
                     "List\\[String\\]" -> "Array[String]", 
                     "List\\[Int\\]" -> "Array[Int]", 
                     "List" -> "Array", 
                     "listsNil" -> "listsArray", 
                     "Nil" -> "Array()")
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}