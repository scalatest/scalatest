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
    
    val javaMap = "def javaMap[K, V](elements: (K, V)*): java.util.LinkedHashMap[K, V] = {\n" +
    "    val m = new java.util.LinkedHashMap[K, V]\n" +
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
        "Nil" -> "Array()", 
        "LinkedArray" -> "LinkedList"
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
        "Nil" -> "new java.util.ArrayList", 
        "LinkedjavaList" -> "LinkedList"
      )
      
    val mapMapping = 
      List(
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
         "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed), 
         "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> mapUpperCasedAreEqual,
         "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: Map[String, String], b: Any): Boolean = a.map(e => (e._1.toUpperCase, e._2.toUpperCase)) == b",
         "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)", 
         "case s: String => a.toUpperCase == s.toUpperCase" -> "case (s1: String, s2: String) => a._1.toUpperCase == s1.toUpperCase && a._2.toUpperCase == s2.toUpperCase", 
         "case _ => a.toUpperCase == b" -> "case _ => (a._1.toUpperCase, a._2.toUpperCase) == b", 
         "case l: List\\[_\\] => l.map\\(upperCase\\(_\\)\\)" -> "case l: Map[_, _] => l.map(upperCase(_))", 
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
         "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")", 
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")", 
         "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
         "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(\"ho\" -> \"ho\", \"hello\" -> \"hello\")", 
         "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"hi\" -> \"hi\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")",
         "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"he\" -> \"he\")", 
         "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hello\" -> \"hello\", \"hi\" -> \"hi\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
         "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "(\"hi\" -> \"hi\", \"he\" -> \"he\")", 
         "\\(\\\"he\\\"\\)" -> "(\"he\" -> \"he\")", 
         "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "(\"he\" -> \"he\", \"hi\" -> \"hi\")", 
         "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\")",
         "\\(\\\"hello\\\", \\\"ho\\\"\\)" -> "(\"hello\" -> \"hello\", \"ho\" -> \"ho\")",
         "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
         "\\(\\\"ho\\\", \\\"hi\\\"\\)" -> "(\"ho\" -> \"ho\", \"hi\" -> \"hi\")",
         "\\(\\\"HI\\\"\\)" -> "(\"HI\" -> \"HI\")", 
         "\\(\\\"HI\\\", \\\"HELLO\\\"\\)" -> "(\"HI\" -> \"HI\", \"HELLO\" -> \"HELLO\")",
         "\\(\\\"HELLO\\\", \\\"HI\\\"\\)" -> "(\"HELLO\" -> \"HELLO\", \"HI\" -> \"HI\")",
         "\\(\\\"HELLO\\\", \\\"HO\\\"\\)" -> "(\"HELLO\" -> \"HELLO\", \"HO\" -> \"HO\")",
         "\\(\\\"HE\\\", \\\"HI\\\"\\)" -> "(\"HE\" -> \"HE\", \"HI\" -> \"HI\")",
         "\\(\\\"HI\\\", \\\"HE\\\"\\)" -> "(\"HI\" -> \"HI\", \"HE\" -> \"HE\")",
         "\\(\\\"HI\\\", \\\"HO\\\"\\)" -> "(\"HI\" -> \"HI\", \"HO\" -> \"HO\")",
         "\\(\\\"HO\\\"\\)" -> "(\"HO\" -> \"HO\")", 
         "\\(\\\"HO\\\", \\\"HI\\\"\\)" -> "(\"HO\" -> \"HO\", \"HI\" -> \"HI\")",
         "\\(\\\"HO\\\", \\\"HELLO\\\"\\)" -> "(\"HO\" -> \"HO\", \"HELLO\" -> \"HELLO\")",
         "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
         "\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"to\" -> \"to\", \"you\" -> \"you\")",
         "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
         "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
         "\\(\\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FAM \" -> \" FAM \")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FAM\" -> \"FAM\")", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\")",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUU \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUU \" -> \" FUU \")",
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUU\" -> \"FUU\")",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\")",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FOE\" -> \"FOE\", \"FAM\" -> \"FAM\")", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUU\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUU\" -> \"FUU\", \"FOE\" -> \"FOE\")", 
         "\\(\\\"FUM\\\", \\\"FOE\\\", \\\"FIE\\\", \\\"FEE\\\"\\)" -> "(\"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\")",
         "\\(\\\" FUM \\\", \\\" FOE \\\", \\\" FIE \\\", \\\" FEE \\\"\\)" -> "(\" FUM \" -> \" FUM \", \" FOE \" -> \" FOE \", \" FIE \" -> \" FIE \", \" FEE \" -> \" FEE \")",
         "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"HAPPY\" -> \"HAPPY\", \"BIRTHDAY\" -> \"BIRTHDAY\", \"TO\" -> \"TO\", \"YOU\" -> \"YOU\")", 
         "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" HAPPY \" -> \" HAPPY \", \" BIRTHDAY \" -> \" BIRTHDAY \", \" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
         "\\(\\\"nice\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"nice\" -> \"nice\", \"to\" -> \"to\", \"you\" -> \"you\")", 
         "\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "(\"nice\" -> \"nice\", \"you\" -> \"you\", \"to\" -> \"to\")", 
         "\\(\\\"to\\\", \\\"you\\\"\\)" -> "(\"to\" -> \"to\", \"you\" -> \"you\")", 
         "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
         "\\(\\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"TO\" -> \"TO\", \"YOU\" -> \"YOU\")",
         "\\(\\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
         "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
         "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"too\" -> \"too\", \"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
         "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
         "\\(\\\"you\\\", \\\"to\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\")",
         "\\(\\\"YOU\\\", \\\"TO\\\"\\)" -> "(\"YOU\" -> \"YOU\", \"TO\" -> \"TO\")",
         "\\(\\\" YOU \\\", \\\" TO \\\"\\)" -> "(\" YOU \" -> \" YOU \", \" TO \" -> \" TO \")",
         "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
         "\\\"\\\\\"HAPPY\\\\\", \\\\\"BIRTHDAY\\\\\", \\\\\"TO\\\\\", \\\\\"YOU\\\\\\\"\\\"" -> "\"(HAPPY,HAPPY), (BIRTHDAY,BIRTHDAY), (TO,TO), (YOU,YOU)\"",
         "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
         "\\\\\"ho\\\\\", \\\\\"hello\\\\\"" -> "(ho,ho), (hello,hello)", 
         "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(hi,hi), (hey,hey), (howdy,howdy)", 
         "\\\\\"hello\\\\\", \\\\\"hi\\\\\"" -> "(hello,hello), (hi,hi)", 
         "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
         "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)",
         "\\\\\"HELLO\\\\\", \\\\\"HI\\\\\"" -> "(HELLO,HELLO), (HI,HI)", 
         "\\\\\"hi\\\\\"" -> "(hi,hi)", 
         "\\\\\"ho\\\\\"" -> "(ho,ho)", 
         "\\\\\"hello\\\\\"" -> "(hello,hello)", 
         "\\\\\"HI\\\\\"" -> "(HI,HI)", 
         "\\\\\"HO\\\\\"" -> "(HO,HO)", 
         "\\\\\"HELLO\\\\\"" -> "(HELLO,HELLO)", 
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
         "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "(fee,fee), (fum,fum), (foe,foe), (fu,fu)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fam,fam)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fee,fee), (fie,fie), (fum,fum), (foe,foe)",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fum,fum), (foe,foe)",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fam,fam), (foe,foe)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUM,FUM)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FAM,FAM)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FAM,FAM), (FOE,FOE)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FUM,FUM), (FOE,FOE)",
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FUM,FUM), (FOE,FOE)", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FAM,FAM), (FOE,FOE)", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FOE,FOE), (FUM,FUM)", 
         "\\\\\"you\\\\\"" -> "(you,you)", 
         "\\\\\"to\\\\\"" -> "(to,to)", 
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
         "\\\"\\(1, 2, 3\\)\\\"" -> "\"((1,1), (2,2), (3,3))\"", 
         "\\\"\\(1, 3, 2\\)\\\"" -> "\"((1,1), (3,3), (2,2))\"", 
         "\\\"\\(1, 3, 4\\)\\\"" -> "\"((1,1), (3,3), (4,4))\"", 
         "\\\"\\(2, 3, 1\\)\\\"" -> "\"((2,2), (3,3), (1,1))\"", 
         "\\\"\\(2, 3, 4\\)\\\"" -> "\"((2,2), (3,3), (4,4))\"", 
         "\\\"\\(2, 3, 8\\)\\\"" -> "\"((2,2), (3,3), (8,8))\"", 
         "\\\"\\(3, 1, 2\\)\\\"" -> "\"((3,3), (1,1), (2,2))\"", 
         "\\\"\\(3, 6, 8\\)\\\"" -> "\"((3,3), (6,6), (8,8))\"", 
         "\\\"\\(4, 2, 3\\)\\\"" -> "\"((4,4), (2,2), (3,3))\"", 
         "List\\(to\\)" -> "Map(to -> to)", 
         "List\\(ho\\)" -> "Map(ho -> ho)", 
         "List\\(hi\\)" -> "Map(hi -> hi)",
         "List\\(hey\\)" -> "Map(hey -> hey)",
         "\\(1, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3)", 
         "\\(1, 2, 5\\)" -> "(1 -> 1, 2 -> 2, 5 -> 5)", 
         "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
         "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
         "\\(1, 3, 2\\)" -> "(1 -> 1, 3 -> 3, 2 -> 2)", 
         "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
         "\\(1, 3, 8\\)" -> "(1 -> 1, 3 -> 3, 8 -> 8)", 
         "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
         "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
         "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
         "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
         "\\(2, 3, 5\\)" -> "(2 -> 2, 3 -> 3, 5 -> 5)", 
         "\\(2, 3, 8\\)" -> "(2 -> 2, 3 -> 3, 8 -> 8)", 
         "\\(2, 6, 8\\)" -> "(2 -> 2, 6 -> 6, 8 -> 8)", 
         "\\(3, 1, 2\\)" -> "(3 -> 3, 1 -> 1, 2 -> 2)", 
         "\\(3, 1, 5\\)" -> "(3 -> 3, 1 -> 1, 5 -> 5)", 
         "\\(3, 2, 1\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1)", 
         "\\(3, 2, 1, 0\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1, 0 -> 0)", 
         "\\(3, 2, 8\\)" -> "(3 -> 3, 2 -> 2, 8 -> 8)", 
         "\\(3, 4, 2\\)" -> "(3 -> 3, 4 -> 4, 2 -> 2)", 
         "\\(3, 4, 5\\)" -> "(3 -> 3, 4 -> 4, 5 -> 5)", 
         "\\(3, 6, 8\\)" -> "(3 -> 3, 6 -> 6, 8 -> 8)", 
         "\\(3, 6, 9\\)" -> "(3 -> 3, 6 -> 6, 9 -> 9)", 
         "\\(3, 8, 5\\)" -> "(3 -> 3, 8 -> 8, 5 -> 5)", 
         "\\(4, 2, 3\\)" -> "(4 -> 4, 2 -> 2, 3 -> 3)", 
         "\\(4, 3, 2\\)" -> "(4 -> 4, 3 -> 3, 2 -> 2)", 
         "\\(5, 3, 4\\)" -> "(5 -> 5, 3 -> 3, 4 -> 4)", 
         "\\(5, 7, 9\\)" -> "(5 -> 5, 7 -> 7, 9 -> 9)", 
         "\\(6, 7, 8\\)" -> "(6 -> 6, 7 -> 7, 8 -> 8)", 
         "\\(8, 3, 1\\)" -> "(8 -> 8, 3 -> 3, 1 -> 1)", 
         "\\(8, 3, 4\\)" -> "(8 -> 8, 3 -> 3, 4 -> 4)", 
         "\\(8, 4, 3, 2\\)" -> "(8 -> 8, 4 -> 4, 3 -> 3, 2 -> 2)", 
         "\\(1, 3, Nil\\)" -> "(1 -> 1, 3 -> 3, Map())", 
         "List" -> "Map", 
         "listsNil" -> "listsMap", 
         "Nil" -> "Map()", 
         "LinkedMap" -> "LinkedList"
      )
      
    val javaMapMapping = 
      List(
        "ListShould" -> "JavaMapShould", 
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
        "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed + "\n" + javaMap), 
        "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> mapUpperCasedAreEqual,
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: java.util.Map[String, String], b: Any): Boolean = a.asScala.map(e => (e._1.toUpperCase, e._2.toUpperCase)) == b",
        "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)",
        "case s: String => a.toUpperCase == s.toUpperCase" -> "case (s1: String, s2: String) => a._1.toUpperCase == s1.toUpperCase && a._2.toUpperCase == s2.toUpperCase", 
        "case _ => a.toUpperCase == b" -> "case _ => (a._1.toUpperCase, a._2.toUpperCase) == b", 
        "case l: List\\[_\\] => l.map\\(upperCase\\(_\\)\\)" -> "case l: Map[_, _] => l.map(upperCase(_))", 
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
        "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")", 
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")",
        "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(\"ho\" -> \"ho\", \"hello\" -> \"hello\")", 
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
        "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"hi\" -> \"hi\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")",
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"he\" -> \"he\")", 
        "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hello\" -> \"hello\", \"hi\" -> \"hi\")",
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
        "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
        "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "(\"hi\" -> \"hi\", \"he\" -> \"he\")", 
        "\\(\\\"he\\\"\\)" -> "(\"he\" -> \"he\")", 
        "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "(\"he\" -> \"he\", \"hi\" -> \"hi\")", 
        "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\")",
        "\\(\\\"hello\\\", \\\"ho\\\"\\)" -> "(\"hello\" -> \"hello\", \"ho\" -> \"ho\")",
        "\\(\\\"ho\\\"\\)" -> "(\"ho\" -> \"ho\")", 
        "\\(\\\"ho\\\", \\\"hi\\\"\\)" -> "(\"ho\" -> \"ho\", \"hi\" -> \"hi\")",
        "\\(\\\"HI\\\"\\)" -> "(\"HI\" -> \"HI\")", 
        "\\(\\\"HI\\\", \\\"HELLO\\\"\\)" -> "(\"HI\" -> \"HI\", \"HELLO\" -> \"HELLO\")",
        "\\(\\\"HELLO\\\", \\\"HI\\\"\\)" -> "(\"HELLO\" -> \"HELLO\", \"HI\" -> \"HI\")",
        "\\(\\\"HELLO\\\", \\\"HO\\\"\\)" -> "(\"HELLO\" -> \"HELLO\", \"HO\" -> \"HO\")",
        "\\(\\\"HE\\\", \\\"HI\\\"\\)" -> "(\"HE\" -> \"HE\", \"HI\" -> \"HI\")",
        "\\(\\\"HI\\\", \\\"HE\\\"\\)" -> "(\"HI\" -> \"HI\", \"HE\" -> \"HE\")",
        "\\(\\\"HI\\\", \\\"HO\\\"\\)" -> "(\"HI\" -> \"HI\", \"HO\" -> \"HO\")",
        "\\(\\\"HO\\\"\\)" -> "(\"HO\" -> \"HO\")", 
        "\\(\\\"HO\\\", \\\"HI\\\"\\)" -> "(\"HO\" -> \"HO\", \"HI\" -> \"HI\")",
        "\\(\\\"HO\\\", \\\"HELLO\\\"\\)" -> "(\"HO\" -> \"HO\", \"HELLO\" -> \"HELLO\")",
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
        "\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"to\" -> \"to\", \"you\" -> \"you\")",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(\"have\" -> \"have\", \"a\" -> \"a\", \"nice\" -> \"nice\", \"day\" -> \"day\")", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUM \" -> \" FUM \")",
        "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")", 
        "\\(\\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\", \"FU\" -> \"FU\")", 
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FAM \" -> \" FAM \")", 
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FAM\" -> \"FAM\")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\")",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUU \\\"\\)" -> "(\" FEE \" -> \" FEE \", \" FIE \" -> \" FIE \", \" FOE \" -> \" FOE \", \" FUU \" -> \" FUU \")",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUU\\\"\\)" -> "(\"FEE\" -> \"FEE\", \"FIE\" -> \"FIE\", \"FOE\" -> \"FOE\", \"FUU\" -> \"FUU\")",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FAM\" -> \"FAM\", \"FOE\" -> \"FOE\")",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\")",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FOE\" -> \"FOE\", \"FAM\" -> \"FAM\")", 
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FOE\" -> \"FOE\", \"FUM\" -> \"FUM\")", 
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUU\\\", \\\"FOE\\\"\\)" -> "(\"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\", \"FUU\" -> \"FUU\", \"FOE\" -> \"FOE\")", 
        "\\(\\\"FUM\\\", \\\"FOE\\\", \\\"FIE\\\", \\\"FEE\\\"\\)" -> "(\"FUM\" -> \"FUM\", \"FOE\" -> \"FOE\", \"FIE\" -> \"FIE\", \"FEE\" -> \"FEE\")",
        "\\(\\\" FUM \\\", \\\" FOE \\\", \\\" FIE \\\", \\\" FEE \\\"\\)" -> "(\" FUM \" -> \" FUM \", \" FOE \" -> \" FOE \", \" FIE \" -> \" FIE \", \" FEE \" -> \" FEE \")",
        "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"HAPPY\" -> \"HAPPY\", \"BIRTHDAY\" -> \"BIRTHDAY\", \"TO\" -> \"TO\", \"YOU\" -> \"YOU\")", 
        "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" HAPPY \" -> \" HAPPY \", \" BIRTHDAY \" -> \" BIRTHDAY \", \" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
        "\\(\\\"nice\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"nice\" -> \"nice\", \"to\" -> \"to\", \"you\" -> \"you\")", 
        "\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "(\"nice\" -> \"nice\", \"you\" -> \"you\", \"to\" -> \"to\")", 
        "\\(\\\"to\\\", \\\"you\\\"\\)" -> "(\"to\" -> \"to\", \"you\" -> \"you\")", 
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(\"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\", \"to\" -> \"to\")", 
        "\\(\\\"TO\\\", \\\"YOU\\\"\\)" -> "(\"TO\" -> \"TO\", \"YOU\" -> \"YOU\")",
        "\\(\\\" TO \\\", \\\" YOU \\\"\\)" -> "(\" TO \" -> \" TO \", \" YOU \" -> \" YOU \")",
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(\" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \", \" TO \" -> \" TO \")", 
        "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"too\" -> \"too\", \"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
        "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
        "\\(\\\"you\\\", \\\"to\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\")",
        "\\(\\\"YOU\\\", \\\"TO\\\"\\)" -> "(\"YOU\" -> \"YOU\", \"TO\" -> \"TO\")",
         "\\(\\\" YOU \\\", \\\" TO \\\"\\)" -> "(\" YOU \" -> \" YOU \", \" TO \" -> \" TO \")",
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"(happy,happy), (birthday,birthday), (to,to), (you,you)\"",
        "\\\"\\\\\"HAPPY\\\\\", \\\\\"BIRTHDAY\\\\\", \\\\\"TO\\\\\", \\\\\"YOU\\\\\\\"\\\"" -> "\"(HAPPY,HAPPY), (BIRTHDAY,BIRTHDAY), (TO,TO), (YOU,YOU)\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(ho,ho), (hey,hey), (howdy,howdy)", 
        "\\\\\"ho\\\\\", \\\\\"hello\\\\\"" -> "(ho,ho), (hello,hello)", 
        "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "(hi,hi), (hey,hey), (howdy,howdy)", 
        "\\\\\"hello\\\\\", \\\\\"hi\\\\\"" -> "(hello,hello), (hi,hi)", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "(happy,happy), (birthday,birthday), (to,to), (you,you)", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "(have,have), (a,a), (nice,nice), (day,day)", 
        "\\\\\"HELLO\\\\\", \\\\\"HI\\\\\"" -> "(HELLO,HELLO), (HI,HI)", 
        "\\\\\"hi\\\\\"" -> "(hi,hi)", 
        "\\\\\"ho\\\\\"" -> "(ho,ho)", 
        "\\\\\"hello\\\\\"" -> "(hello,hello)", 
        "\\\\\"HI\\\\\"" -> "(HI,HI)", 
        "\\\\\"HO\\\\\"" -> "(HO,HO)", 
        "\\\\\"HELLO\\\\\"" -> "(HELLO,HELLO)", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fam,fam)",
        "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "(fee,fee), (fum,fum), (foe,foe), (fu,fu)",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fee,fee), (fie,fie), (fum,fum), (foe,foe)",
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fum,fum), (foe,foe)",
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fam,fam), (foe,foe)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUM,FUM)", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FAM,FAM)",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FAM,FAM), (FOE,FOE)",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FUM,FUM), (FOE,FOE)",
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FUM,FUM), (FOE,FOE)", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FAM,FAM), (FOE,FOE)", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FOE,FOE), (FUM,FUM)", 
        "\\\\\"you\\\\\"" -> "(you,you)", 
        "\\\\\"to\\\\\"" -> "(to,to)", 
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
        "\\\"\\(1, 2, 3\\)\\\"" -> "\"((1,1), (2,2), (3,3))\"", 
        "\\\"\\(1, 3, 2\\)\\\"" -> "\"((1,1), (3,3), (2,2))\"", 
        "\\\"\\(1, 3, 4\\)\\\"" -> "\"((1,1), (3,3), (4,4))\"", 
        "\\\"\\(2, 3, 1\\)\\\"" -> "\"((2,2), (3,3), (1,1))\"", 
        "\\\"\\(2, 3, 4\\)\\\"" -> "\"((2,2), (3,3), (4,4))\"", 
        "\\\"\\(2, 3, 8\\)\\\"" -> "\"((2,2), (3,3), (8,8))\"", 
        "\\\"\\(3, 1, 2\\)\\\"" -> "\"((3,3), (1,1), (2,2))\"", 
        "\\\"\\(3, 6, 8\\)\\\"" -> "\"((3,3), (6,6), (8,8))\"", 
        "\\\"\\(4, 2, 3\\)\\\"" -> "\"((4,4), (2,2), (3,3))\"",
        "List\\(to\\)" -> "javaMap(to -> to)", 
        "List\\(ho\\)" -> "javaMap(ho -> ho)", 
        "List\\(hi\\)" -> "javaMap(hi -> hi)",
        "List\\(hey\\)" -> "javaMap(hey -> hey)",
        "\\(1, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3)", 
        "\\(1, 2, 5\\)" -> "(1 -> 1, 2 -> 2, 5 -> 5)", 
        "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
        "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
        "\\(1, 3, 2\\)" -> "(1 -> 1, 3 -> 3, 2 -> 2)", 
        "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
        "\\(1, 3, 8\\)" -> "(1 -> 1, 3 -> 3, 8 -> 8)", 
        "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
        "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
        "\\(2, 3, 1\\)" -> "(2 -> 2, 3 -> 3, 1 -> 1)", 
        "\\(2, 3, 4\\)" -> "(2 -> 2, 3 -> 3, 4 -> 4)", 
        "\\(2, 3, 5\\)" -> "(2 -> 2, 3 -> 3, 5 -> 5)", 
        "\\(2, 3, 8\\)" -> "(2 -> 2, 3 -> 3, 8 -> 8)", 
        "\\(2, 6, 8\\)" -> "(2 -> 2, 6 -> 6, 8 -> 8)", 
        "\\(3, 1, 2\\)" -> "(3 -> 3, 1 -> 1, 2 -> 2)", 
        "\\(3, 1, 5\\)" -> "(3 -> 3, 1 -> 1, 5 -> 5)", 
        "\\(3, 2, 1\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1)", 
        "\\(3, 2, 1, 0\\)" -> "(3 -> 3, 2 -> 2, 1 -> 1, 0 -> 0)", 
        "\\(3, 2, 8\\)" -> "(3 -> 3, 2 -> 2, 8 -> 8)", 
        "\\(3, 4, 2\\)" -> "(3 -> 3, 4 -> 4, 2 -> 2)", 
        "\\(3, 4, 5\\)" -> "(3 -> 3, 4 -> 4, 5 -> 5)", 
        "\\(3, 6, 8\\)" -> "(3 -> 3, 6 -> 6, 8 -> 8)", 
        "\\(3, 6, 9\\)" -> "(3 -> 3, 6 -> 6, 9 -> 9)", 
        "\\(3, 8, 5\\)" -> "(3 -> 3, 8 -> 8, 5 -> 5)", 
        "\\(4, 2, 3\\)" -> "(4 -> 4, 2 -> 2, 3 -> 3)", 
        "\\(4, 3, 2\\)" -> "(4 -> 4, 3 -> 3, 2 -> 2)", 
        "\\(5, 3, 4\\)" -> "(5 -> 5, 3 -> 3, 4 -> 4)", 
        "\\(5, 7, 9\\)" -> "(5 -> 5, 7 -> 7, 9 -> 9)", 
        "\\(8, 3, 1\\)" -> "(8 -> 8, 3 -> 3, 1 -> 1)", 
        "\\(8, 3, 4\\)" -> "(8 -> 8, 3 -> 3, 4 -> 4)", 
        "\\(8, 4, 3, 2\\)" -> "(8 -> 8, 4 -> 4, 3 -> 3, 2 -> 2)", 
        "\\(1, 3, Nil\\)" -> "(1 -> 1, 3 -> 3, Map())", 
        "List" -> "javaMap", 
        "listsNil" -> "listsMap", 
        "Nil" -> "javaMap()", 
        "LinkedjavaMap" -> "LinkedList"
      )
      
    val stringMapping = 
      List(
        "ListShould" -> "StringShould", 
        //"List\\[String\\]" -> "List[Char]", 
        //"Vector\\[List\\[Int\\]\\]" -> "Vector[List[Char]]", 
        "new Equality\\[String\\]" -> "new Equality[Char]", 
        "def areEqual\\(a: String, b: Any\\): Boolean = a != b" -> "def areEqual(a: Char, b: Any): Boolean = a != b", 
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a != b" -> "def areEqual(a: String, b: Any): Boolean = a != b", 
        "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> "def areEqual(a: Char, b: Any): Boolean = a.toString.toUpperCase.toCharArray()(0) == b",
        //"def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b",
        //"def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: Char, b: Any)",
        "def areEqual\\(a: String, b: Any\\): Boolean = upperCase\\(a\\) == upperCase\\(b\\)" -> "def areEqual(a: Char, b: Any): Boolean = upperCase(a) == upperCase(b)",
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_\\.toUpperCase\\) == b" -> "def areEqual(a: String, b: Any): Boolean = a.toUpperCase == b",
        "defaultEquality\\[String\\]" -> "defaultEquality[Char]", 
        " and trimmed" -> "", 
        "//ADDITIONAL//" -> (stringLowerCased), 
        "LinkedList" -> "TempL", 
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
        "List\\(3, 2, 1, 0\\)" -> "\"3210\"", 
        "List\\(1, 2, 3\\)" -> "\"123\"", 
        "List\\(2, 3, 4\\)" -> "\"234\"", 
        "List\\(3, 2, 1\\)" -> "\"321\"", 
        "List\\(4, 3, 2\\)" -> "\"432\"",
        "List\\(8, 4, 3, 2\\)" -> "\"8432\"",
        "List\\(\\\"hi\\\"\\)" -> "\"i\"", 
        "List\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "\"il\"", 
        "List\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "\"dil\"", 
        "List\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "\"die\"", 
        "List\\(\\\"hi\\\", \\\"he\\\"\\)" -> "\"ie\"", 
        "List\\(\\\"to\\\", \\\"you\\\"\\)" -> "\"oy\"", 
        "List\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"hoy\"", 
        "List\\(\\\"you\\\", \\\"to\\\"\\)" -> "\"yo\"", 
        "List\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "\"nyo\"", 
        "List\\(\\\"hey\\\"\\)" -> "\"e\"",
        "List\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"upie\"", 
        "List\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"xupie\"", 
        "List\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"yobh\"", 
        "List\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"zyobh\"", 
        "List\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"hboy\"", 
        "TempL" -> "LinkedList", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "('e', 'i', 'p', 'u')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'u', 'p')", 
        "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "('e', 'u', 'p', 'f')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('e', 'i', 'u', 'p')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'a', 'p')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "('e', 'i', 'p', 'a')", 
        "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('u', 'p', 'i', 'e')", 
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('x', 'u', 'p', 'i', 'e')",
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('o', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('i', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "('i', 'l')", 
        "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "('o', 'l')", 
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "('d', 'i', 'l')", 
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "('d', 'i', 'e')", 
        "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "('d', 'l', 'i')",
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u')", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u')", 
        "\\(\\\"hi\\\"\\)" -> "('i')", 
        "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "('i', 'e')", 
        "\\(\\\"he\\\"\\)" -> "('e')", 
        "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "('e', 'i')", 
        "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "('l', 'i')",
        "\\(\\\"hello\\\", \\\"ho\\\"\\)" -> "('l', 'o')",
        "\\(\\\"ho\\\"\\)" -> "('o')", 
        "\\(\\\"ho\\\", \\\"hi\\\"\\)" -> "('o', 'i')",
        "\\(\\\"HI\\\"\\)" -> "('I')", 
        "\\(\\\"HI\\\", \\\"HELLO\\\"\\)" -> "('I', 'L')",
        "\\(\\\"HELLO\\\", \\\"HI\\\"\\)" -> "('L', 'I')",
        "\\(\\\"HELLO\\\", \\\"HO\\\"\\)" -> "('L', 'O')",
        "\\(\\\"HE\\\", \\\"HI\\\"\\)" -> "('E', 'I')",
        "\\(\\\"HI\\\", \\\"HE\\\"\\)" -> "('I', 'E')",
        "\\(\\\"HI\\\", \\\"HO\\\"\\)" -> "('I', 'O')",
        "\\(\\\"HO\\\"\\)" -> "('O')", 
        "\\(\\\"HO\\\", \\\"HI\\\"\\)" -> "('O', 'I')",
        "\\(\\\"HO\\\", \\\"HELLO\\\"\\)" -> "('O', 'L')",
        "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "('h', 'b', 'o', 'y')",
        "\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "('h', 'o', 'y')",
        "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "('h', 'a', 'n', 'd')", 
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u', 'u')",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "('E', 'I', 'P', 'U')",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "('I', 'E', 'U', 'P')",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "('E', 'I', 'P', 'U')",
        "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "('E', 'U', 'P', 'F')",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "('E', 'I', 'U', 'P')", 
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "('E', 'I', 'P', 'A')", 
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "('E', 'I', 'P', 'A')",
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "('E', 'I', 'A', 'P')",
        "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUU \\\"\\)" -> "('E', 'I', 'P', 'D')", 
        "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUU\\\"\\)" -> "('E', 'I', 'P', 'D')",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "('I', 'E', 'A', 'P')",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "('I', 'E', 'U', 'P')",
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "('I', 'E', 'P', 'A')", 
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "('I', 'E', 'P', 'U')", 
        "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUU\\\", \\\"FOE\\\"\\)" -> "('I', 'E', 'D', 'P')", 
        "\\(\\\"FUM\\\", \\\"FOE\\\", \\\"FIE\\\", \\\"FEE\\\"\\)" -> "('U', 'P', 'I', 'E')",
        "\\(\\\" FUM \\\", \\\" FOE \\\", \\\" FIE \\\", \\\" FEE \\\"\\)" -> "('U', 'P', 'I', 'E')",
        "\\(\\\"nice\\\", \\\"to\\\", \\\"you\\\"\\)" -> "('n', 'o', 'y')", 
        "\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "('n', 'y', 'o')", 
        "\\(\\\"to\\\", \\\"you\\\"\\)" -> "('o', 'y')", 
        "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "('o', 'o', 'o', 'o')", 
        "\\(\\\"TO\\\", \\\"YOU\\\"\\)" -> "('O', 'Y')",
        "\\(\\\" TO \\\", \\\" YOU \\\"\\)" -> "('O', 'Y')",
        "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "('O', 'O', 'O', 'O')", 
        "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "('H', 'B', 'O', 'Y')", 
        "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "('H', 'B', 'O', 'Y')", 
        "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "('z', 'y', 'o', 'b', 'h')",
        "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "('y', 'o', 'b', 'h')",
        "\\(\\\"you\\\", \\\"to\\\"\\)" -> "('y', 'o')",
        "\\(\\\"YOU\\\", \\\"TO\\\"\\)" -> "('Y', 'O')",
        "\\(\\\" YOU \\\", \\\" TO \\\"\\)" -> "('Y', 'O')",
        "\\(\\\"YOU\\\", \\\"TO\\\", \\\"BIRTHDAY\\\", \\\"HAPPY\\\"\\)" -> "('Y', 'O', 'B', 'H')", 
        "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"'h', 'b', 'o', 'y'\"",
        "\\\"\\\\\"HAPPY\\\\\", \\\\\"BIRTHDAY\\\\\", \\\\\"TO\\\\\", \\\\\"YOU\\\\\\\"\\\"" -> "\"'H', 'B', 'O', 'Y'\"",
        "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "'o', 'e', 'd'", 
        "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "'i', 'e', 'd'", 
        "\\\\\"hello\\\\\", \\\\\"hi\\\\\"" -> "'l', 'i'", 
        "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "'h', 'b', 'o', 'y'", 
        "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "'h', 'a', 'n', 'd'", 
        "\\\\\"HELLO\\\\\", \\\\\"HI\\\\\"" -> "'L', 'I'", 
        "\\\\\"hi\\\\\"" -> "'i'", 
        "\\\\\"ho\\\\\"" -> "'o'", 
        "\\\\\"hello\\\\\"" -> "'l'", 
        "\\\\\"HI\\\\\"" -> "'I'", 
        "\\\\\"HO\\\\\"" -> "'O'", 
        "\\\\\"HELLO\\\\\"" -> "'L'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "'e', 'i', 'p', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "'e', 'i', 'p', 'a'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'e', 'i', 'u', 'p'", 
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "'e', 'u', 'p', 'f'", 
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'i', 'e', 'u', 'p'", 
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "'i', 'e', 'a', 'p'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "'E', 'I', 'P', 'U'", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "'E', 'I', 'P', 'A'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "'E', 'I', 'A', 'P'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'E', 'I', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'A', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "'I', 'E', 'P', 'U'", 
        "\\\\\"you\\\\\"" -> "'y'", 
        "\\\\\"to\\\\\"" -> "'o'", 
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
        "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of ('i', 'l')", 
        "of \\(\\\\\"ho\\\\\", \\\\\"hello\\\\\"\\)" -> "of ('o', 'l')", 
        "of \\(\\\\\"HI\\\\\"\\)" -> "of ('I')", 
        "\\\"\\(1, 2, 3\\)\\\"" -> "\"('1', '2', '3')\"", 
        "\\\"\\(1, 3, 2\\)\\\"" -> "\"('1', '3', '2')\"", 
        "\\\"\\(1, 3, 4\\)\\\"" -> "\"('1', '3', '4')\"", 
        "\\\"\\(2, 3, 1\\)\\\"" -> "\"('2', '3', '1')\"", 
        "\\\"\\(2, 3, 4\\)\\\"" -> "\"('2', '3', '4')\"", 
        "\\\"\\(2, 3, 8\\)\\\"" -> "\"('2', '3', '8')\"", 
        "\\\"\\(3, 1, 2\\)\\\"" -> "\"('3', '1', '2')\"", 
        "\\\"\\(3, 6, 8\\)\\\"" -> "\"('3', '6', '8')\"", 
        "\\\"\\(4, 2, 3\\)\\\"" -> "\"('4', '2', '3')\"",
        "List\\(to\\)" -> "\\\"to\\\"", 
        "List\\(ho\\)" -> "\\\"ho\\\"", 
        "List\\(hi\\)" -> "\\\"hi\\\"",
        "List\\(hey\\)" -> "\\\"hey\\\"",
        "\\(1, 2, 3\\)" -> "('1', '2', '3')", 
        "\\(1, 2, 5\\)" -> "('1', '2', '5')", 
        "\\(1, 2, 8\\)" -> "('1', '2', '8')", 
        "\\(1, 2, 9\\)" -> "('1', '2', '9')", 
        "\\(1, 3, 2\\)" -> "('1', '3', '2')", 
        "\\(1, 3, 4\\)" -> "('1', '3', '4')", 
        "\\(1, 3, 8\\)" -> "('1', '3', '8')", 
        "\\(1, 6, 8\\)" -> "('1', '6', '8')", 
        "\\(2, 1, 5\\)" -> "('2', '1', '5')", 
        "\\(2, 3, 1\\)" -> "('2', '3', '1')", 
        "\\(2, 3, 4\\)" -> "('2', '3', '4')", 
        "\\(2, 3, 5\\)" -> "('2', '3', '5')", 
        "\\(2, 3, 8\\)" -> "('2', '3', '8')", 
        "\\(2, 6, 8\\)" -> "('2', '6', '8')", 
        "\\(3, 1, 2\\)" -> "('3', '1', '2')", 
        "\\(3, 1, 5\\)" -> "('3', '1', '5')", 
        "\\(3, 2, 1\\)" -> "('3', '2', '1')", 
        "\\(3, 2, 1, 0\\)" -> "('3', '2', '1', '0')", 
        "\\(3, 2, 8\\)" -> "('3', '2', '8')", 
        "\\(3, 4, 2\\)" -> "('3', '4', '2')", 
        "\\(3, 4, 5\\)" -> "('3', '4', '5')", 
        "\\(3, 6, 8\\)" -> "('3', '6', '8')", 
        "\\(3, 6, 9\\)" -> "('3', '6', '9')", 
        "\\(3, 8, 5\\)" -> "('3', '8', '5')", 
        "\\(4, 2, 3\\)" -> "('4', '2', '3')", 
        "\\(4, 3, 2\\)" -> "('4', '3', '2')", 
        "\\(5, 3, 4\\)" -> "('5', '3', '4')", 
        "\\(5, 7, 9\\)" -> "('5', '7', '9')", 
        "\\(8, 3, 4\\)" -> "('8', '3', '4')", 
        "\\(8, 4, 3, 2\\)" -> "('8', '4', '3', '2')", 
        "\\(1, 3, Nil\\)" -> "('1', '3')", 
        "listsNil" -> "listsString", 
        "Nil" -> "\\\"\\\"", 
        "List\\(\\)" -> "\\\"\\\"", 
        "Resources\\(\\\"didNotEqual\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"didNotEqual\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))", 
        //"Resources\\(\\\"equaled\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"equaled\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))",
        "Resources\\(\\\"wasNotEqualTo\\\", decorateToStringValue\\(fumList\\), decorateToStringValue\\(toList\\)\\)" -> "Resources(\"wasNotEqualTo\", decorateToStringValue(\"[\" + fumList + \"]\"), decorateToStringValue(\"[\" + toList + \"]\"))", 
        "decorateToStringValue\\(\\\"1\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"2\\\"\\)" -> "decorateToStringValue(\"[1]\") + \" was not equal to \" + decorateToStringValue(\"[2]\")",
        "decorateToStringValue\\(\\\"2\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"1\\\"\\)" -> "decorateToStringValue(\"[2]\") + \" was not equal to \" + decorateToStringValue(\"[1]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"e\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[e]\")", 
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"1\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[1]\")", 
        "decorateToStringValue\\(\\\"i\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[i]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")", 
        "decorateToStringValue\\(\\\"2\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[2]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")", 
        "decorateToStringValue\\(\\\"432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[432]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")", 
        "decorateToStringValue\\(\\\"432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"321\\\"\\)" -> "decorateToStringValue(\"[432]\") + \" was not equal to \" + decorateToStringValue(\"[321]\")", 
        "decorateToStringValue\\(\\\"8432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[8432]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(\\\"8432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3210\\\"\\)" -> "decorateToStringValue(\"[8432]\") + \" was not equal to \" + decorateToStringValue(\"[3210]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"321\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[321]\")", 
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"123\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[123]\")",
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(\\\"123\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[123]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"321\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[321]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"3210\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[3210]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"321\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[321]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"123\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[123]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3210\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[3210]\")",
        "decorateToStringValue\\(\\\"dil\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[dil]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")", 
        "decorateToStringValue\\(\\\"il\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[il]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")"
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
      
    // Generate tests for theSameElementsInOrderAs
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
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
  }
  
  def main(args: Array[String]) {
    val targetDir = args(0)
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}