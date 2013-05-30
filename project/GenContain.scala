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
    
    val mapUpperCasedAreEqual = "def areEqual(a: (String, String), b: Any): Boolean = b match {\n" +
    "    case (b1: String, b2: String) => a._1.toUpperCase == b1 && a._2.toUpperCase == b2\n" +
    "    case _ => (a._1.toUpperCase, a._2.toUpperCase) == b\n" +
    "  }"
    
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
    
    val optionMapping = 
      List(
        "List\\[String\\]" -> "Option[String]", 
        "List\\[Int\\]" -> "Option[Int]", 
        "List" -> "Option", 
        "listsNil" -> "listsOption", 
        "Nil" -> "None"  
      )
    
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
        "def areEqual\\(a: java.util.List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: java.util.List[String], b: Any): Boolean = a.asScala.map(_.toUpperCase) == b",
        "List\\[Int\\]" -> "java.util.List[Int]", 
        "List\\(" -> "javaList(", 
        "listsNil" -> "listsJavaCol", 
        "Nil" -> "new java.util.ArrayList"  
      )
      
    val mapMapping = 
      List(
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
         "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed), 
         "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> mapUpperCasedAreEqual,
         "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: Map[String, String], b: Any): Boolean = a.map(e => (e._1.toUpperCase, e._2.toUpperCase)) == b",
         "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)", 
         "defaultEquality\\[String\\]" -> "defaultEquality[(String, String)]", 
         "List\\[String\\]" -> "Map[String, String]", 
         "List\\[Int\\]" -> "Map[Int, Int]", 
         "List\\(\\\"fum\\\"\\)" -> "Map(\"fum\" -> \"fum\")", 
         "List\\(\\\"to\\\"\\)" -> "Map(\"to\" -> \"to\")", 
         "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "Map(\"fum\" -> \"fum\", \"fu\" -> \"fu\")", 
         "List\\(1\\)" -> "Map(1 -> 1)", 
         "List\\(2\\)" -> "Map(2 -> 2)", 
         "List\\(3\\)" -> "Map(3 -> 3)", 
         "List\\(8\\)" -> "Map(8 -> 8)", 
         "List\\(\\\"hi\\\"\\)" -> "Map(\"hi\" -> \"hi\")", 
         "List\\(\\\"hey\\\"\\)" -> "Map(\"hey\" -> \"hey\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fum\" -> \"fum\")", 
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")", 
         "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "(\"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fu\" -> \"fu\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fam\" -> \"fam\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")",
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fam\" -> \"fam\", \"foe\" -> \"foe\")", 
         "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
         "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(\"ho\" -> \"ho\", \"hello\" -> \"hello\")", 
         "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"hi\" -> \"hi\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")",
         "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
         "\\(\\\"he\\\"\\)" -> "(\"he\" -> \"he\")", 
         "\\(\\\"HI\\\"\\)" -> "(\"HI\" -> \"HI\")", 
         "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
         "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
         "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
         "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
         "\\(\\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FAM \" -> \" FAM \")", 
         "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"HAPPY\" -> \"HAPPY\", \"BIRTHDAY\" -> \"BIRTHDAY\", \"TO\" -> \"TO\", \"YOU\" -> \"YOU\")", 
         "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" HAPPY \" -> \" HAPPY \", \" BIRTHDAY \" -> \" BIRTHDAY \", \" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
         "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
         "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
         "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
         "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
         "\\\\\"ho\\\\\", \\\\\"hello\\\\\"" -> "(ho,ho), (hello,hello)", 
         "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(hi,hi), (hey,hey), (howdy,howdy)", 
         "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
         "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)", 
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
         "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "(fee,fee), (fum,fum), (foe,foe), (fu,fu)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fam,fam)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fee,fee), (fie,fie), (fum,fum), (foe,foe)",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fum,fum), (foe,foe)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUM,FUM)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FUM,FUM), (FOE,FOE)",
         "of \\(1, 2, 3\\)" -> "of ((1,1), (2,2), (3,3))", 
         "of \\(1, 2, 8\\)" -> "of ((1,1), (2,2), (8,8))", 
         "of \\(1, 3, 4\\)" -> "of ((1,1), (3,3), (4,4))", 
         "of \\(1, 6, 8\\)" -> "of ((1,1), (6,6), (8,8))", 
         "of \\(2, 3, 1\\)" -> "of ((2,2), (3,3), (1,1))", 
         "of \\(2, 3, 4\\)" -> "of ((2,2), (3,3), (4,4))", 
         "of \\(2, 3, 5\\)" -> "of ((2,2), (3,3), (5,5))", 
         "of \\(2, 3, 8\\)" -> "of ((2,2), (3,3), (8,8))", 
         "of \\(2, 6, 8\\)" -> "of ((2,2), (6,6), (8,8))", 
         "of \\(3, 6, 8\\)" -> "of ((3,3), (6,6), (8,8))", 
         "of \\(3, 6, 9\\)" -> "of ((3,3), (6,6), (9,9))", 
         "of \\(6, 7, 8\\)" -> "of ((6,6), (7,7), (8,8))", 
         "of \\(\\\\\"ho\\\\\"\\)" -> "of ((ho,ho))", 
         "of \\(\\\\\"hi\\\\\"\\)" -> "of ((hi,hi))", 
         "of \\(\\\\\"he\\\\\"\\)" -> "of ((he,he))", 
         "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ((hi,hi), (hello,hello))", 
         "of \\(\\\\\"HI\\\\\"\\)" -> "of ((HI,HI))", 
         "List\\(to\\)" -> "Map(to -> to)", 
         "List\\(ho\\)" -> "Map(ho -> ho)", 
         "List\\(hi\\)" -> "Map(hi -> hi)",
         "List\\(hey\\)" -> "Map(hey -> hey)",
         "\\(1, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3)", 
         "\\(1, 2, 5\\)" -> "(1 -> 1, 2 -> 2, 5 -> 5)", 
         "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
         "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
         "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
         "\\(1, 3, 8\\)" -> "(1 -> 1, 3 -> 3, 8 -> 8)", 
         "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
         "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
         "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
         "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
         "\\(2, 3, 5\\)" -> "(2 -> 2, 3 -> 3, 5 -> 5)", 
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
         "\\(5, 7, 9\\)" -> "(5 -> 5, 7 -> 7, 9 -> 9)", 
         "\\(6, 7, 8\\)" -> "(6 -> 6, 7 -> 7, 8 -> 8)", 
         "\\(8, 3, 1\\)" -> "(8 -> 8, 3 -> 3, 1 -> 1)", 
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
        "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> mapUpperCasedAreEqual,
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: java.util.Map[String, String], b: Any): Boolean = a.asScala.map(e => (e._1.toUpperCase, e._2.toUpperCase)) == b",
        "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)",
        "defaultEquality\\[String\\]" -> "defaultEquality[(String, String)]", 
        "List\\[String\\]" -> "java.util.Map[String, String]", 
        "List\\[Int\\]" -> "java.util.Map[Int, Int]", 
        "List\\(\\\"fum\\\"\\)" -> "javaMap(\"fum\" -> \"fum\")",
        "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "javaMap(\"fum\" -> \"fum\", \"fu\" -> \"fu\")",
        "List\\(\\\"to\\\"\\)" -> "javaMap(\"to\" -> \"to\")", 
        "List\\(1\\)" -> "javaMap(1 -> 1)", 
        "List\\(2\\)" -> "javaMap(2 -> 2)", 
        "List\\(3\\)" -> "javaMap(3 -> 3)", 
        "List\\(8\\)" -> "javaMap(8 -> 8)", 
        "List\\(\\\"hi\\\"\\)" -> "javaMap(\"hi\" -> \"hi\")", 
        "List\\(\\\"hey\\\"\\)" -> "javaMap(\"hey\" -> \"hey\")", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fum\" -> \"fum\")", 
        "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "(\"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fu\" -> \"fu\")", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fam\" -> \"fam\")", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")",
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fam\" -> \"fam\", \"foe\" -> \"foe\")", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")", 
        "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(\"ho\" -> \"ho\", \"hello\" -> \"hello\")", 
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
        "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"hi\" -> \"hi\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")",
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
        "\\(\\\"he\\\"\\)" -> "(\"he\" -> \"he\")", 
        "\\(\\\"HI\\\"\\)" -> "(\"HI\" -> \"HI\")", 
        "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
        "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
        "\\(\\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FAM \" -> \" FAM \")", 
        "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"HAPPY\" -> \"HAPPY\", \"BIRTHDAY\" -> \"BIRTHDAY\", \"TO\" -> \"TO\", \"YOU\" -> \"YOU\")", 
        "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" HAPPY \" -> \" HAPPY \", \" BIRTHDAY \" -> \" BIRTHDAY \", \" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
        "\\\\\"ho\\\\\", \\\\\"hello\\\\\"" -> "(ho,ho), (hello,hello)", 
        "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(hi,hi), (hey,hey), (howdy,howdy)", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fam,fam)",
        "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "(fee,fee), (fum,fum), (foe,foe), (fu,fu)",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fee,fee), (fie,fie), (fum,fum), (foe,foe)",
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fum,fum), (foe,foe)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUM,FUM)", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FUM,FUM), (FOE,FOE)",
        "of \\(1, 2, 3\\)" -> "of ((1,1), (2,2), (3,3))", 
        "of \\(1, 2, 8\\)" -> "of ((1,1), (2,2), (8,8))", 
        "of \\(1, 3, 4\\)" -> "of ((1,1), (3,3), (4,4))", 
        "of \\(1, 6, 8\\)" -> "of ((1,1), (6,6), (8,8))", 
        "of \\(2, 3, 1\\)" -> "of ((2,2), (3,3), (1,1))", 
        "of \\(2, 3, 4\\)" -> "of ((2,2), (3,3), (4,4))", 
        "of \\(2, 3, 5\\)" -> "of ((2,2), (3,3), (5,5))", 
        "of \\(2, 3, 8\\)" -> "of ((2,2), (3,3), (8,8))", 
        "of \\(2, 6, 8\\)" -> "of ((2,2), (6,6), (8,8))", 
        "of \\(3, 6, 8\\)" -> "of ((3,3), (6,6), (8,8))", 
        "of \\(3, 6, 9\\)" -> "of ((3,3), (6,6), (9,9))", 
        "of \\(\\\\\"ho\\\\\"\\)" -> "of ((ho,ho))", 
        "of \\(\\\\\"hi\\\\\"\\)" -> "of ((hi,hi))", 
        "of \\(\\\\\"he\\\\\"\\)" -> "of ((he,he))", 
        "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ((hi,hi), (hello,hello))", 
        "of \\(\\\\\"HI\\\\\"\\)" -> "of ((HI,HI))", 
        "List\\(to\\)" -> "javaMap(to -> to)", 
        "List\\(ho\\)" -> "javaMap(ho -> ho)", 
        "List\\(hi\\)" -> "javaMap(hi -> hi)",
        "List\\(hey\\)" -> "javaMap(hey -> hey)",
        "\\(1, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3)", 
        "\\(1, 2, 5\\)" -> "(1 -> 1, 2 -> 2, 5 -> 5)", 
        "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
        "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
        "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
        "\\(1, 3, 8\\)" -> "(1 -> 1, 3 -> 3, 8 -> 8)", 
        "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
        "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
        "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
        "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
        "\\(2, 3, 5\\)" -> "(2 -> 2, 3 -> 3, 5 -> 5)", 
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
        "\\(5, 7, 9\\)" -> "(5 -> 5, 7 -> 7, 9 -> 9)", 
        "\\(8, 3, 1\\)" -> "(8 -> 8, 3 -> 3, 1 -> 1)", 
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
        "def areEqual\\(a: String, b: Any\\): Boolean = a != b" -> "def areEqual(a: Char, b: Any): Boolean = a != b", 
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a != b" -> "def areEqual(a: String, b: Any): Boolean = a != b", 
        "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> "def areEqual(a: Char, b: Any): Boolean = a.toString.toUpperCase.toCharArray()(0) == b",
        //"def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b",
        //"def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: Char, b: Any)",
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_\\.toUpperCase\\) == b" -> "def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b",
        "defaultEquality\\[String\\]" -> "defaultEquality[Char]", 
        " and trimmed" -> "", 
        "//ADDITIONAL//" -> (stringLowerCased), 
        "List\\[String\\]" -> "String", 
        "List\\[Int\\]" -> "String", 
        "List\\(\\\"fum\\\"\\)" -> "\"u\"",
        "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "\"uf\"",
        "List\\(\\\"to\\\"\\)" -> "\"o\"",
        "List\\(\\\"ho\\\"\\)" -> "\"o\"",
        "List\\(1\\)" -> "\"1\"", 
        "List\\(2\\)" -> "\"2\"", 
        "List\\(3\\)" -> "\"3\"", 
        "List\\(8\\)" -> "\"8\"", 
        "List\\(\\\"hi\\\"\\)" -> "\"i\"", 
        "List\\(\\\"hey\\\"\\)" -> "\"e\"",
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "('e', 'i', 'p', 'u')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'u', 'p')", 
        "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "('e', 'u', 'p', 'f')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('e', 'i', 'u', 'p')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'a', 'p')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "('e', 'i', 'p', 'a')", 
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('o', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('i', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "('i', 'e')", 
        "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "('o', 'e')", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u')", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u')", 
        "\\(\\\"hi\\\"\\)" -> "('i')", 
        "\\(\\\"he\\\"\\)" -> "('e')", 
        "\\(\\\"ho\\\"\\)" -> "('o')", 
        "\\(\\\"HI\\\"\\)" -> "('I')", 
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "('h', 'b', 'o', 'y')",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "('h', 'a', 'n', 'd')", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u', 'u')",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "('E', 'I', 'P', 'U')",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "('I', 'E', 'U', 'P')",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "('E', 'I', 'P', 'U')",
        "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "('E', 'U', 'P', 'F')",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "('E', 'I', 'U', 'P')", 
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "('E', 'I', 'P', 'A')", 
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "('o', 'o', 'o', 'o')", 
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "('O', 'O', 'O', 'O')", 
        "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "('H', 'B', 'O', 'Y')", 
        "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "('H', 'B', 'O', 'Y')", 
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"'h', 'b', 'o', 'y'\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "'o', 'e', 'd'", 
        "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "'i', 'e', 'd'", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "'h', 'b', 'o', 'y'", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "'h', 'a', 'n', 'd'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "'e', 'i', 'p', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "'e', 'i', 'p', 'a'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'e', 'i', 'u', 'p'", 
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "'e', 'u', 'p', 'f'", 
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'i', 'e', 'u', 'p'", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "'E', 'I', 'P', 'U'", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'E', 'I', 'U', 'P'", 
        "of \\(1, 2, 8\\)" -> "of ('1', '2', '8')", 
        "of \\(1, 3, 4\\)" -> "of ('1', '3', '4')", 
        "of \\(1, 6, 8\\)" -> "of ('1', '6', '8')", 
        "of \\(2, 3, 1\\)" -> "of ('2', '3', '1')", 
        "of \\(2, 3, 4\\)" -> "of ('2', '3', '4')", 
        "of \\(2, 3, 5\\)" -> "of ('2', '3', '5')", 
        "of \\(2, 3, 8\\)" -> "of ('2', '3', '8')", 
        "of \\(2, 6, 8\\)" -> "of ('2', '6', '8')", 
        "of \\(3, 6, 8\\)" -> "of ('3', '6', '8')", 
        "of \\(\\\\\"ho\\\\\"\\)" -> "of ('o')", 
        "of \\(\\\\\"hi\\\\\"\\)" -> "of ('i')", 
        "of \\(\\\\\"he\\\\\"\\)" -> "of ('e')", 
        "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ('i', 'e')", 
        "of \\(\\\\\"ho\\\\\", \\\\\"hello\\\\\"\\)" -> "of ('o', 'e')", 
        "of \\(\\\\\"HI\\\\\"\\)" -> "of ('I')", 
        "List\\(to\\)" -> "\\\"to\\\"", 
        "List\\(ho\\)" -> "\\\"ho\\\"", 
        "List\\(hi\\)" -> "\\\"hi\\\"",
        "List\\(hey\\)" -> "\\\"hey\\\"",
        "\\(1, 2, 3\\)" -> "('1', '2', '3')", 
        "\\(1, 2, 5\\)" -> "('1', '2', '5')", 
        "\\(1, 2, 8\\)" -> "('1', '2', '8')", 
        "\\(1, 2, 9\\)" -> "('1', '2', '9')", 
        "\\(1, 3, 4\\)" -> "('1', '3', '4')", 
        "\\(1, 3, 8\\)" -> "('1', '3', '8')", 
        "\\(1, 6, 8\\)" -> "('1', '6', '8')", 
        "\\(2, 1, 5\\)" -> "('2', '1', '5')", 
        "\\(2, 3, 1\\)" -> "('2', '3', '1')", 
        "\\(2, 3, 4\\)" -> "('2', '3', '4')", 
        "\\(2, 3, 5\\)" -> "('2', '3', '5')", 
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
        "\\(5, 7, 9\\)" -> "('5', '7', '9')", 
        "\\(8, 3, 4\\)" -> "('8', '3', '4')", 
        "\\(1, 3, Nil\\)" -> "('1', '3')", 
        "listsNil" -> "listsString", 
        "Nil" -> "\\\"\\\"", 
        "List\\(\\)" -> "\\\"\\\"", 
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
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}