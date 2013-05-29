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
    
    val stringLowerCased = "val lowerCased: Normalization[Char] = new Normalization[Char] {\n" +
    "    def normalized(c: Char): Char = c.toString.toLowerCase.toCharArray()(0)\n" +
    "    def normalizedIfInstanceOfA(b: Any) =\n" +
    "      b match {\n" + 
    "        case c: Char => normalized(c)\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val mapLowerCased = "val lowerCased: Normalization[(String, String)] = new Normalization[(String, String)] {\n" + 
    "    def normalized(s: (String, String)): (String, String) = (s._1.toLowerCase, s._2.toLowerCase)\n" +
    "    def normalizedIfInstanceOfA(b: Any) =\n" + 
    "      b match {\n" + 
    "        case (s1: String, s2: String) => normalized((s1, s2))\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val mapTrimmed = "val trimmed: Normalization[(String, String)] = new Normalization[(String, String)] {\n" + 
    "    def normalized(s: (String, String)): (String, String) = (s._1.trim, s._2.trim)\n" +
    "    def normalizedIfInstanceOfA(b: Any) =\n" + 
    "      b match {\n" + 
    "        case (s1: String, s2: String) => normalized((s1, s2))\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val javaList = "def javaList[E](elements: E*): java.util.ArrayList[E] = {\n" +
    "    val l = new java.util.ArrayList[E]\n" +
    "    elements.foreach(l.add(_))\n" +
    "    l\n" +
    "  }"
    
    val javaMap = "def javaMap[K, V](elements: (K, V)*): java.util.HashMap[K, V] = {\n" +
    "    val m = new java.util.HashMap[K, V]\n" +
    "    elements.foreach(e => m.put(e._1, e._2))\n" +
    "    m\n" +
    "  }"
    
    val arrayMapping = 
      List(
        "List\\[String\\]" -> "Array[String]", 
        "List\\[Int\\]" -> "Array[Int]", 
        "List" -> "Array", 
        "listsNil" -> "listsArray", 
        "Nil" -> "Array()"  
      )
      
    val javaColMapping = 
      List(
        "ListShould" -> "JavaColShould", 
        "//ADDITIONAL//" -> (javaList), 
        "List\\[String\\]" -> "java.util.List[String]", 
        "List\\[Int\\]" -> "java.util.List[Int]", 
        "List\\(" -> "javaList(", 
        "listsNil" -> "listsJavaCol", 
        "Nil" -> "new java.util.ArrayList"  
      )
      
    val mapMapping = 
      List(
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
         "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed), 
         "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)",
         "defaultEquality\\[String\\]" -> "defaultEquality[(String, String)]", 
         "List\\[String\\]" -> "Map[String, String]", 
         "List\\[Int\\]" -> "Map[Int, Int]", 
         "List\\(\\\"fum\\\"\\)" -> "Map(\"fum\" -> \"fum\")", 
         "List\\(\\\"to\\\"\\)" -> "Map(\"to\" -> \"to\")", 
         "List\\(1\\)" -> "Map(1 -> 1)", 
         "List\\(2\\)" -> "Map(2 -> 2)", 
         "List\\(3\\)" -> "Map(3 -> 3)", 
         "List\\(8\\)" -> "Map(8 -> 8)", 
         "List\\(\\\"hi\\\"\\)" -> "Map(\"hi\" -> \"hi\")", 
         "List\\(\\\"hey\\\"\\)" -> "Map(\"hey\" -> \"hey\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fum\" -> \"fum\")", 
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")", 
         "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
         "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
         "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
         "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
         "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
         "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
         "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
         "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
         "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
         "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
         "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)", 
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
         "of \\(1, 2, 8\\)" -> "of ((1,1), (2,2), (8,8))", 
         "of \\(1, 3, 4\\)" -> "of ((1,1), (3,3), (4,4))", 
         "of \\(1, 6, 8\\)" -> "of ((1,1), (6,6), (8,8))", 
         "of \\(2, 3, 1\\)" -> "of ((2,2), (3,3), (1,1))", 
         "of \\(2, 3, 4\\)" -> "of ((2,2), (3,3), (4,4))", 
         "of \\(2, 3, 8\\)" -> "of ((2,2), (3,3), (8,8))", 
         "of \\(2, 6, 8\\)" -> "of ((2,2), (6,6), (8,8))", 
         "of \\(3, 6, 8\\)" -> "of ((3,3), (6,6), (8,8))", 
         "of \\(\\\\\"ho\\\\\"\\)" -> "of ((ho,ho))", 
         "of \\(\\\\\"hi\\\\\"\\)" -> "of ((hi,hi))", 
         "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ((hi,hi), (hello,hello))", 
         "List\\(to\\)" -> "Map(to -> to)", 
         "List\\(ho\\)" -> "Map(ho -> ho)", 
         "List\\(hi\\)" -> "Map(hi -> hi)",
         "List\\(hey\\)" -> "Map(hey -> hey)",
         "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
         "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
         "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
         "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
         "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
         "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
         "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
         "\\(2, 3, 8\\)" -> "(2 -> 2, 3 -> 3, 8 -> 8)", 
         "\\(2, 6, 8\\)" -> "(2 -> 2, 6 -> 6, 8 -> 8)", 
         "\\(3, 1, 5\\)" -> "(3 -> 3, 1 -> 1, 5 -> 5)", 
         "\\(3, 2, 1\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1)", 
         "\\(3, 2, 8\\)" -> "(3 -> 3, 2 -> 2, 8 -> 8)", 
         "\\(3, 4, 5\\)" -> "(3 -> 3, 4 -> 4, 5 -> 5)", 
         "\\(3, 6, 8\\)" -> "(3 -> 3, 6 -> 6, 8 -> 8)", 
         "\\(3, 6, 9\\)" -> "(3 -> 3, 6 -> 6, 9 -> 9)", 
         "\\(3, 8, 5\\)" -> "(3 -> 3, 8 -> 8, 5 -> 5)", 
         "\\(5, 3, 4\\)" -> "(5 -> 5, 3 -> 3, 4 -> 4)", 
         "\\(8, 3, 4\\)" -> "(8 -> 8, 3 -> 3, 4 -> 4)", 
         "\\(1, 3, Nil\\)" -> "(1 -> 1, 3 -> 3, Map())", 
         "List" -> "Map", 
         "listsNil" -> "listsMap", 
         "Nil" -> "Map()"
      )
      
    val javaMapMapping = 
      List(
        "ListShould" -> "JavaMapShould", 
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
        "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed + "\n" + javaMap), 
        "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)",
        "defaultEquality\\[String\\]" -> "defaultEquality[(String, String)]", 
        "List\\[String\\]" -> "java.util.Map[String, String]", 
        "List\\[Int\\]" -> "java.util.Map[Int, Int]", 
        "List\\(\\\"fum\\\"\\)" -> "javaMap(\"fum\" -> \"fum\")", 
        "List\\(\\\"to\\\"\\)" -> "javaMap(\"to\" -> \"to\")", 
        "List\\(1\\)" -> "javaMap(1 -> 1)", 
        "List\\(2\\)" -> "javaMap(2 -> 2)", 
        "List\\(3\\)" -> "javaMap(3 -> 3)", 
        "List\\(8\\)" -> "javaMap(8 -> 8)", 
        "List\\(\\\"hi\\\"\\)" -> "javaMap(\"hi\" -> \"hi\")", 
        "List\\(\\\"hey\\\"\\)" -> "javaMap(\"hey\" -> \"hey\")", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fum\" -> \"fum\")", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")", 
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
        "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
        "of \\(1, 2, 8\\)" -> "of ((1,1), (2,2), (8,8))", 
        "of \\(1, 3, 4\\)" -> "of ((1,1), (3,3), (4,4))", 
        "of \\(1, 6, 8\\)" -> "of ((1,1), (6,6), (8,8))", 
        "of \\(2, 3, 1\\)" -> "of ((2,2), (3,3), (1,1))", 
        "of \\(2, 3, 4\\)" -> "of ((2,2), (3,3), (4,4))", 
        "of \\(2, 3, 8\\)" -> "of ((2,2), (3,3), (8,8))", 
        "of \\(2, 6, 8\\)" -> "of ((2,2), (6,6), (8,8))", 
        "of \\(3, 6, 8\\)" -> "of ((3,3), (6,6), (8,8))", 
        "of \\(\\\\\"ho\\\\\"\\)" -> "of ((ho,ho))", 
        "of \\(\\\\\"hi\\\\\"\\)" -> "of ((hi,hi))", 
        "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ((hi,hi), (hello,hello))", 
        "List\\(to\\)" -> "javaMap(to -> to)", 
        "List\\(ho\\)" -> "javaMap(ho -> ho)", 
        "List\\(hi\\)" -> "javaMap(hi -> hi)",
        "List\\(hey\\)" -> "javaMap(hey -> hey)",
        "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
        "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
        "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
        "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
        "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
        "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
        "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
        "\\(2, 3, 8\\)" -> "(2 -> 2, 3 -> 3, 8 -> 8)", 
        "\\(2, 6, 8\\)" -> "(2 -> 2, 6 -> 6, 8 -> 8)", 
        "\\(3, 1, 5\\)" -> "(3 -> 3, 1 -> 1, 5 -> 5)", 
        "\\(3, 2, 1\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1)", 
        "\\(3, 2, 8\\)" -> "(3 -> 3, 2 -> 2, 8 -> 8)", 
        "\\(3, 4, 5\\)" -> "(3 -> 3, 4 -> 4, 5 -> 5)", 
        "\\(3, 6, 8\\)" -> "(3 -> 3, 6 -> 6, 8 -> 8)", 
        "\\(3, 6, 9\\)" -> "(3 -> 3, 6 -> 6, 9 -> 9)", 
        "\\(3, 8, 5\\)" -> "(3 -> 3, 8 -> 8, 5 -> 5)", 
        "\\(5, 3, 4\\)" -> "(5 -> 5, 3 -> 3, 4 -> 4)", 
        "\\(8, 3, 4\\)" -> "(8 -> 8, 3 -> 3, 4 -> 4)", 
        "\\(1, 3, Nil\\)" -> "(1 -> 1, 3 -> 3, Map())", 
        "List" -> "javaMap", 
        "listsNil" -> "listsMap", 
        "Nil" -> "javaMap()"
      )
      
    val stringMapping = 
      List(
        "ListShould" -> "StringShould", 
        "new Equality\\[String\\]" -> "new Equality[Char]", 
        "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: Char, b: Any)",
        "defaultEquality\\[String\\]" -> "defaultEquality[Char]", 
        " and trimmed" -> "", 
        "//ADDITIONAL//" -> (stringLowerCased), 
        "List\\[String\\]" -> "String", 
        "List\\[Int\\]" -> "String", 
        "List\\(\\\"fum\\\"\\)" -> "\"u\"", 
        "List\\(\\\"to\\\"\\)" -> "\"o\"",
        "List\\(\\\"ho\\\"\\)" -> "\"o\"",
        "List\\(1\\)" -> "\"1\"", 
        "List\\(2\\)" -> "\"2\"", 
        "List\\(3\\)" -> "\"3\"", 
        "List\\(8\\)" -> "\"8\"", 
        "List\\(\\\"hi\\\"\\)" -> "\"i\"", 
        "List\\(\\\"hey\\\"\\)" -> "\"e\"", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "('s', 't', 'u', 'v')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('s', 'u', 't', 'v')", 
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('o', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "('i', 'e')", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u')", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u')", 
        "\\(\\\"hi\\\"\\)" -> "('i')", 
        "\\(\\\"ho\\\"\\)" -> "('o')", 
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "('h', 'b', 'o', 'y')",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "('h', 'a', 'n', 'd')", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u', 'u')",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "('S', 'T', 'U', 'V')",
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "('o', 'o', 'o', 'o')", 
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "('O', 'O', 'O', 'O')", 
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"'h', 'b', 'o', 'y'\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "'o', 'e', 'd'", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "'h', 'b', 'o', 'y'", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "'h', 'a', 'n', 'd'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "'s', 't', 'u', 'v'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u'",
        "of \\(1, 2, 8\\)" -> "of ('1', '2', '8')", 
        "of \\(1, 3, 4\\)" -> "of ('1', '3', '4')", 
        "of \\(1, 6, 8\\)" -> "of ('1', '6', '8')", 
        "of \\(2, 3, 1\\)" -> "of ('2', '3', '1')", 
        "of \\(2, 3, 4\\)" -> "of ('2', '3', '4')", 
        "of \\(2, 3, 8\\)" -> "of ('2', '3', '8')", 
        "of \\(2, 6, 8\\)" -> "of ('2', '6', '8')", 
        "of \\(3, 6, 8\\)" -> "of ('3', '6', '8')", 
        "of \\(\\\\\"ho\\\\\"\\)" -> "of ('o')", 
        "of \\(\\\\\"hi\\\\\"\\)" -> "of ('i')", 
        "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ('i', 'e')", 
        "List\\(to\\)" -> "\\\"to\\\"", 
        "List\\(ho\\)" -> "\\\"ho\\\"", 
        "List\\(hi\\)" -> "\\\"hi\\\"",
        "List\\(hey\\)" -> "\\\"hey\\\"",
        "\\(1, 2, 8\\)" -> "('1', '2', '8')", 
        "\\(1, 2, 9\\)" -> "('1', '2', '9')", 
        "\\(1, 3, 4\\)" -> "('1', '3', '4')", 
        "\\(1, 6, 8\\)" -> "('1', '6', '8')", 
        "\\(2, 1, 5\\)" -> "('2', '1', '5')", 
        "\\(2, 3, 1\\)" -> "('2', '3', '1')", 
        "\\(2, 3, 4\\)" -> "('2', '3', '4')", 
        "\\(2, 3, 8\\)" -> "('2', '3', '8')", 
        "\\(2, 6, 8\\)" -> "('2', '6', '8')", 
        "\\(3, 1, 5\\)" -> "('3', '1', '5')", 
        "\\(3, 2, 1\\)" -> "('3', '2', '1')", 
        "\\(3, 2, 8\\)" -> "('3', '2', '8')", 
        "\\(3, 4, 5\\)" -> "('3', '4', '5')", 
        "\\(3, 6, 8\\)" -> "('3', '6', '8')", 
        "\\(3, 6, 9\\)" -> "('3', '6', '9')", 
        "\\(3, 8, 5\\)" -> "('3', '8', '5')", 
        "\\(5, 3, 4\\)" -> "('5', '3', '4')", 
        "\\(8, 3, 4\\)" -> "('8', '3', '4')", 
        "\\(1, 3, Nil\\)" -> "('1', '3')", 
        "listsNil" -> "listsString", 
        "Nil" -> "\\\"\\\"", 
        "Resources\\(\\\"didNotEqual\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"didNotEqual\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))", 
        "Resources\\(\\\"equaled\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"equaled\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))",
        "Resources\\(\\\"wasNotEqualTo\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"wasNotEqualTo\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))", 
        "decorateToStringValue\\(\\\"1\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"2\\\"\\)" -> "decorateToStringValue(\"[1]\") + \" was not equal to \" + decorateToStringValue(\"[2]\")",
        "decorateToStringValue\\(\\\"2\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"1\\\"\\)" -> "decorateToStringValue(\"[2]\") + \" was not equal to \" + decorateToStringValue(\"[1]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"e\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[e]\")", 
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"1\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[1]\")", 
        "decorateToStringValue\\(\\\"i\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[i]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")", 
        "decorateToStringValue\\(\\\"2\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[2]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")"
      )
    
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
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}