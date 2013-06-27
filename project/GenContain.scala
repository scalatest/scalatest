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
    "    def normalizedAny(b: Any) =\n" +
    "      b match {\n" + 
    "        case c: Char => normalized(c)\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val mapLowerCased = "val lowerCased: Normalization[(String, String)] = new Normalization[(String, String)] {\n" + 
    "    def normalized(s: (String, String)): (String, String) = (s._1.toLowerCase, s._2.toLowerCase)\n" +
    "    def normalizedAny(b: Any) =\n" + 
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
    "    def normalizedAny(b: Any) =\n" + 
    "      b match {\n" + 
    "        case (s1: String, s2: String) => normalized((s1, s2))\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val javaMapLowerCased = "val lowerCased: Normalization[java.util.Map.Entry[String, String]] = new Normalization[java.util.Map.Entry[String, String]] {\n" + 
    "    def normalized(s: java.util.Map.Entry[String, String]): java.util.Map.Entry[String, String] = org.scalatest.Entry(s.getKey.toLowerCase, s.getValue.toLowerCase)\n" +
    "    def normalizedAny(b: Any) =\n" + 
    "      b match {\n" + 
    "        case entry: java.util.Map.Entry[_, _] => \n" +
    "          (entry.getKey, entry.getValue) match {\n" +
    "            case (k: String, v: String) => normalized(org.scalatest.Entry(k, v))\n" +
    "            case _ => b\n" +
    "        }\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val javaMapUpperCasedAreEqual = "def areEqual(a: java.util.Map.Entry[String, String], b: Any): Boolean = b match {\n" +
    "    case entry: java.util.Map.Entry[_, _] => \n" +
    "      (entry.getKey, entry.getValue) match {\n" +
    "        case (k: String, v: String) => a.getKey.toUpperCase == k && a.getValue.toUpperCase == v\n" +
    "        case _ => (a.getKey.toUpperCase, a.getValue.toUpperCase) == b\n" +
    "      }\n" +
    "    case _ => (a.getKey.toUpperCase, a.getValue.toUpperCase) == b\n" +
    "  }"
    
    val javaMapTrimmed = "val trimmed: Normalization[java.util.Map.Entry[String, String]] = new Normalization[java.util.Map.Entry[String, String]] {\n" + 
    "    def normalized(s: java.util.Map.Entry[String, String]): java.util.Map.Entry[String, String] = org.scalatest.Entry(s.getKey.trim, s.getValue.trim)\n" +
    "    def normalizedAny(b: Any) =\n" + 
    "      b match {\n" + 
    "        case entry: java.util.Map.Entry[_, _] => \n" +
    "          (entry.getKey, entry.getValue) match {\n " +
    "            case (k: String, v: String) => normalized(Entry(k, v))\n" +
    "            case _ => b\n" +
    "          }\n" + 
    "        case _ => b\n" + 
    "      }\n" + 
    "    }"
    
    val javaList = "def javaList[E](elements: E*): java.util.ArrayList[E] = {\n" +
    "    val l = new java.util.ArrayList[E]\n" +
    "    elements.foreach(l.add(_))\n" +
    "    l\n" +
    "  }"
    
    val javaMap = "def javaMap[K, V](elements: Entry[K, V]*): java.util.LinkedHashMap[K, V] = {\n" +
    "    val m = new java.util.LinkedHashMap[K, V]\n" +
    "    elements.foreach(e => m.put(e.getKey, e.getValue))\n" +
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
        "ListShould" -> "MapShould", 
        "new Equality\\[String\\]" -> "new Equality[(String, String)]", 
         "//ADDITIONAL//" -> (mapLowerCased + "\n" + mapTrimmed), 
         "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> mapUpperCasedAreEqual,
         "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: scala.collection.mutable.LinkedHashMap[String, String], b: Any): Boolean = a.map(e => (e._1.toUpperCase, e._2.toUpperCase)) == b",
         "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: (String, String), b: Any)", 
         "case s: String => a.toUpperCase == s.toUpperCase" -> "case (s1: String, s2: String) => a._1.toUpperCase == s1.toUpperCase && a._2.toUpperCase == s2.toUpperCase", 
         "case _ => a.toUpperCase == b" -> "case _ => (a._1.toUpperCase, a._2.toUpperCase) == b", 
         "case l: List\\[_\\] => l.map\\(upperCase\\(_\\)\\)" -> "case l: Map[_, _] => l.map(upperCase(_))", 
         "defaultEquality\\[String\\]" -> "defaultEquality[(String, String)]", 
         "List\\[String\\]" -> "scala.collection.mutable.LinkedHashMap[String, String]", 
         "List\\[Int\\]" -> "scala.collection.mutable.LinkedHashMap[Int, Int]", 
         "List\\(\\\"fum\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"fum\" -> \"fum\")", 
         "List\\(\\\"to\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"to\" -> \"to\")", 
         "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"fum\" -> \"fum\", \"fu\" -> \"fu\")", 
         "List\\(1\\)" -> "scala.collection.mutable.LinkedHashMap(1 -> 1)", 
         "List\\(2\\)" -> "scala.collection.mutable.LinkedHashMap(2 -> 2)", 
         "List\\(3\\)" -> "scala.collection.mutable.LinkedHashMap(3 -> 3)", 
         "List\\(8\\)" -> "scala.collection.mutable.LinkedHashMap(8 -> 8)", 
         "List\\(1, 2\\)" -> "scala.collection.mutable.LinkedHashMap(1 -> 1, 2 -> 2)", 
         "List\\(2, 3\\)" -> "scala.collection.mutable.LinkedHashMap(2 -> 2, 3 -> 3)", 
         "List\\(\\\"hi\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"hi\" -> \"hi\")", 
         "List\\(\\\"hi\\\", \\\"ho\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"hi\" -> \"hi\", \"ho\" -> \"ho\")", 
         "List\\(\\\"hey\\\"\\)" -> "scala.collection.mutable.LinkedHashMap(\"hey\" -> \"hey\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fum\" -> \"fum\")", 
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")", 
         "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "(\"fee\" -> \"fee\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fu\" -> \"fu\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"foe\" -> \"foe\", \"fam\" -> \"fam\")", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fee\" -> \"fee\", \"fie\" -> \"fie\", \"fum\" -> \"fum\", \"foe\" -> \"foe\")",
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "(\"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fam\" -> \"fam\", \"foe\" -> \"foe\")", 
         "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")", 
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fee\" -> \"fee\", \"fee\" -> \"fee\")",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(\"fex\" -> \"fex\", \"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fee\" -> \"fee\")",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fee\" -> \"fee\")",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fee\" -> \"fee\", \"fee\" -> \"fee\")",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"foe\" -> \"foe\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fie\" -> \"fie\", \"fee\" -> \"fee\", \"fee\" -> \"fee\")",
         "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"ho\" -> \"ho\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")", 
         "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(\"ho\" -> \"ho\", \"hello\" -> \"hello\")", 
         "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(\"hi\" -> \"hi\", \"hey\" -> \"hey\", \"howdy\" -> \"howdy\")",
         "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(\"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hi\" -> \"hi\", \"he\" -> \"he\")", 
         "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "(\"howdy\" -> \"howdy\", \"hello\" -> \"hello\", \"hi\" -> \"hi\")", 
         "\\(\\\"fum\\\", \\\"foe\\\"\\)" -> "(\"fum\" -> \"fum\", \"foe\" -> \"foe\")",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(\"fum\" -> \"fum\", \"fum\" -> \"fum\")", 
         "\\(\\\"hi\\\"\\)" -> "(\"hi\" -> \"hi\")", 
         "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "(\"hi\" -> \"hi\", \"he\" -> \"he\")", 
         "\\(\\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"hi\" -> \"hi\", \"hi\" -> \"hi\", \"he\" -> \"he\")", 
         "\\(\\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "(\"hi\" -> \"hi\", \"he\" -> \"he\", \"he\" -> \"he\", \"he\" -> \"he\")", 
         "\\(\\\"he\\\"\\)" -> "(\"he\" -> \"he\")", 
         "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "(\"he\" -> \"he\", \"hi\" -> \"hi\")", 
         "\\(\\\"he\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(\"he\" -> \"he\", \"hi\" -> \"hi\", \"hello\" -> \"hello\")", 
         "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\")",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\", \"he\" -> \"he\")",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\", \"he\" -> \"he\", \"he\" -> \"he\", \"he\" -> \"he\")",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(\"hello\" -> \"hello\", \"hi\" -> \"hi\", \"hi\" -> \"hi\", \"he\" -> \"he\")",
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
         "\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\", \\\"too\\\"\\)" -> "(\"happy\" -> \"happy\", \"happy\" -> \"happy\", \"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\", \"too\" -> \"too\")",
         "\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(\"happy\" -> \"happy\", \"happy\" -> \"happy\", \"happy\" -> \"happy\", \"birthday\" -> \"birthday\", \"to\" -> \"to\", \"you\" -> \"you\")",
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
         "\\(\\\"too\\\", \\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "(\"too\" -> \"too\", \"you\" -> \"you\", \"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\", \"happy\" -> \"happy\")",
         "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"too\" -> \"too\", \"you\" -> \"you\", \"to\" -> \"to\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
         "\\(\\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
         "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(\"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\")",
         "\\(\\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "(\"you\" -> \"you\", \"you\" -> \"you\", \"to\" -> \"to\", \"birthday\" -> \"birthday\", \"happy\" -> \"happy\", \"happy\" -> \"happy\")",
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
         "\\\\\"hi\\\\\", \\\\\"he\\\\\"" -> "(hi,hi), (he,he)", 
         "\\\\\"hi\\\\\"" -> "(hi,hi)", 
         "\\\\\"ho\\\\\"" -> "(ho,ho)", 
         "\\\\\"hello\\\\\"" -> "(hello,hello)", 
         "\\\\\"HE\\\\\", \\\\\"HI\\\\\"" -> "(HE,HE), (HI,HI)", 
         "\\\\\"HI\\\\\", \\\\\"HE\\\\\"" -> "(HI,HI), (HE,HE)", 
         "\\\\\"HI\\\\\"" -> "(HI,HI)", 
         "\\\\\"HO\\\\\"" -> "(HO,HO)", 
         "\\\\\"HELLO\\\\\"" -> "(HELLO,HELLO)", 
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fum,fum)",
         "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "(fee,fee), (fum,fum), (foe,foe), (fu,fu)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "(fee,fee), (fie,fie), (foe,foe), (fam,fam)",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fee,fee), (fie,fie), (fum,fum), (foe,foe)",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fum,fum), (foe,foe)",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "(fie,fie), (fee,fee), (fam,fam), (foe,foe)",
         "\\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fie\\\\\", \\\\\"fee\\\\\"" -> "(fum,fum), (foe,foe), (fie,fie), (fee,fee)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum), (fum,fum)",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "(fum,fum), (fum,fum)",
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUM,FUM)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUU\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FUU,FUU)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FOE,FOE), (FAM,FAM)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FAM,FAM), (FOE,FOE)", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FEE,FEE), (FIE,FIE), (FUM,FUM), (FOE,FOE)",
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FUM,FUM), (FOE,FOE)", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FAM,FAM), (FOE,FOE)", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "(FIE,FIE), (FEE,FEE), (FOE,FOE), (FUM,FUM)", 
         "\\\\\"FUM\\\\\", \\\\\"FOE\\\\\", \\\\\"FIE\\\\\", \\\\\"FEE\\\\\"" -> "(FUM,FUM), (FOE,FOE), (FIE,FIE), (FEE,FEE)", 
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
         "of \\(4, 3, 2\\)" -> "of ((4,4), (3,3), (2,2))", 
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
         "List\\(to\\)" -> "scala.collection.mutable.LinkedHashMap(to -> to)", 
         "List\\(ho\\)" -> "scala.collection.mutable.LinkedHashMap(ho -> ho)", 
         "List\\(hi\\)" -> "scala.collection.mutable.LinkedHashMap(hi -> hi)",
         "List\\(hey\\)" -> "scala.collection.mutable.LinkedHashMap(hey -> hey)",
         "\\(0, 1, 1, 1, 2, 3\\)" -> "(0 -> 0, 1 -> 1, 1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3)",
         "\\(0, 1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "(0 -> 0, 1 -> 1, 1 -> 1, 1 -> 1, 2 -> 2, 2 -> 2, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)",
         "\\(0, 1, 1, 2, 3, 3\\)" -> "(0 -> 0, 1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3)", 
         "\\(0, 1, 1, 2, 3, 3, 3\\)" -> "(0 -> 0, 1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)", 
         "\\(0, 1, 2, 2, 3\\)" -> "(0 -> 0, 1 -> 1, 2 -> 2, 2 -> 2, 3 -> 3)", 
         "\\(0, 1, 2, 2, 3, 3, 3\\)" -> "(0 -> 0, 1 -> 1, 2 -> 2, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)", 
         "\\(0, 1, 2, 3\\)" -> "(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3)", 
         "\\(0, 1, 2, 3, 3\\)" -> "(0 -> 0, 1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3)", 
         "\\(1, 1, 1, 2, 3\\)" -> "(1 -> 1, 1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3)", 
         "\\(1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "(1 -> 1, 1 -> 1, 1 -> 1, 2 -> 2, 2 -> 2, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)", 
         "\\(1, 1, 2, 3, 3\\)" -> "(1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3)", 
         "\\(1, 1, 2, 3, 3, 3\\)" -> "(1 -> 1, 1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)", 
         "\\(1, 2, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 2 -> 2, 3 -> 3)", 
         "\\(1, 2, 2, 3, 3, 3\\)" -> "(1 -> 1, 2 -> 2, 2 -> 2, 3 -> 3, 3 -> 3, 3 -> 3)", 
         "\\(1, 2, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3)", 
         "\\(1, 2, 3, 3\\)" -> "(1 -> 1, 2 -> 2, 3 -> 3, 3 -> 3)", 
         "\\(1, 2, 5\\)" -> "(1 -> 1, 2 -> 2, 5 -> 5)", 
         "\\(1, 2, 8\\)" -> "(1 -> 1, 2 -> 2, 8 -> 8)", 
         "\\(1, 2, 9\\)" -> "(1 -> 1, 2 -> 2, 9 -> 9)", 
         "\\(1, 3, 2\\)" -> "(1 -> 1, 3 -> 3, 2 -> 2)", 
         "\\(1, 3, 4\\)" -> "(1 -> 1, 3 -> 3, 4 -> 4)", 
         "\\(1, 3, 8\\)" -> "(1 -> 1, 3 -> 3, 8 -> 8)", 
         "\\(1, 6, 8\\)" -> "(1 -> 1, 6 -> 6, 8 -> 8)", 
         "\\(2, 1, 5\\)" -> "(2 -> 2, 1 -> 1, 5 -> 5)", 
         "\\(2, 2, 3, 4\\)" -> "(2 -> 2, 2 -> 2, 3 -> 3, 4 -> 4)", 
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
         "\\(3, 6, 5\\)" -> "(3 -> 3, 6 -> 6, 5 -> 5)", 
         "\\(3, 6, 8\\)" -> "(3 -> 3, 6 -> 6, 8 -> 8)", 
         "\\(3, 6, 9\\)" -> "(3 -> 3, 6 -> 6, 9 -> 9)", 
         "\\(3, 8, 5\\)" -> "(3 -> 3, 8 -> 8, 5 -> 5)", 
         "\\(4, 2, 3\\)" -> "(4 -> 4, 2 -> 2, 3 -> 3)", 
         "\\(4, 3, 2\\)" -> "(4 -> 4, 3 -> 3, 2 -> 2)", 
         "\\(5, 3, 4\\)" -> "(5 -> 5, 3 -> 3, 4 -> 4)", 
         "\\(5, 7, 9\\)" -> "(5 -> 5, 7 -> 7, 9 -> 9)", 
         "\\(6, 7, 8\\)" -> "(6 -> 6, 7 -> 7, 8 -> 8)", 
         "\\(8, 2, 3, 4\\)" -> "(8 -> 8, 2 -> 2, 3 -> 3, 4 -> 4)", 
         "\\(8, 2, 2, 3, 4\\)" -> "(8 -> 8, 2 -> 2, 2 -> 2, 3 -> 3, 4 -> 4)", 
         "\\(8, 3, 1\\)" -> "(8 -> 8, 3 -> 3, 1 -> 1)", 
         "\\(8, 3, 4\\)" -> "(8 -> 8, 3 -> 3, 4 -> 4)", 
         "\\(8, 4, 3, 2\\)" -> "(8 -> 8, 4 -> 4, 3 -> 3, 2 -> 2)", 
         "\\(1, 3, Nil\\)" -> "(1 -> 1, 3 -> 3, Map())", 
         "List\\(" -> "scala.collection.mutable.LinkedHashMap(",
         //"List" -> "Map", 
         "Linkedscala.collection.mutable.LinkedHashMap" -> "scala.collection.mutable.LinkedList", 
         //"LinkedMap" -> "LinkedHashMap", 
         "listsNil" -> "listsMap", 
         "Nil" -> "scala.collection.mutable.LinkedHashMap()"
      )
      
    val javaMapMapping = 
      List(
        "ListShould" -> "JavaMapShould", 
        //"Entry\\(" -> "org.scalatest.Entry(", 
        "new Equality\\[String\\]" -> "new Equality[java.util.Map.Entry[String, String]]", 
        "//ADDITIONAL//" -> (javaMapLowerCased + "\n" + javaMapTrimmed + "\n" + javaMap + "\n" + "import org.scalatest.Entry"), 
        "def areEqual\\(a: String, b: Any\\): Boolean = a.toUpperCase == b" -> javaMapUpperCasedAreEqual,
        "def areEqual\\(a: List\\[String\\], b: Any\\): Boolean = a.map\\(\\_.toUpperCase\\) == b" -> "def areEqual(a: java.util.Map[String, String], b: Any): Boolean = a.asScala.map(e => upperCase(e)) == b",
        "def areEqual\\(a: String, b: Any\\)" -> "def areEqual(a: java.util.Map.Entry[String, String], b: Any)",
        "case s: String => a.toUpperCase == s.toUpperCase" -> "case java.util.Map.Entry[_, _] => toUpperCase(a) == toUpperCase(s)", 
        "case _ => a.toUpperCase == b" -> "case _ => (a.getKey.toUpperCase, a.getValue.toUpperCase) == b", 
        "case l: List\\[_\\] => l.map\\(upperCase\\(_\\)\\)" -> "case l: Map[_, _] => l.map(upperCase(_))", 
        "defaultEquality\\[String\\]" -> "defaultEquality[java.util.Map.Entry[String, String]]", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"))",
        "List\\[String\\]" -> "java.util.Map[String, String]", 
        "List\\[Int\\]" -> "java.util.Map[Int, Int]", 
        "List\\(\\\"fum\\\"\\)" -> "javaMap(Entry(\"fum\", \"fum\"))",
        "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "javaMap(Entry(\"fum\", \"fum\"), Entry(\"fu\", \"fu\"))",
        "List\\(\\\"to\\\"\\)" -> "javaMap(Entry(\"to\", \"to\"))", 
        "List\\(1\\)" -> "javaMap(Entry(1, 1))", 
        "List\\(2\\)" -> "javaMap(Entry(2, 2))", 
        "List\\(3\\)" -> "javaMap(Entry(3, 3))", 
        "List\\(8\\)" -> "javaMap(Entry(8, 8))", 
        "List\\(1, 2\\)" -> "javaMap(Entry(1, 1), Entry(2, 2))", 
        "List\\(2, 3\\)" -> "javaMap(Entry(2, 2), Entry(3, 3))", 
        "List\\(\\\"hi\\\"\\)" -> "javaMap(Entry(\"hi\", \"hi\"))", 
        "List\\(\\\"hi\\\", \\\"ho\\\"\\)" -> "javaMap(Entry(\"hi\", \"hi\"), Entry(\"ho\", \"ho\"))", 
        "List\\(\\\"hey\\\"\\)" -> "javaMap(Entry(\"hey\", \"hey\"))", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "(Entry(\"fee\", \"fee\"), Entry(\"fie\", \"fie\"), Entry(\"foe\", \"foe\"), Entry(\"fum\", \"fum\"))", 
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"))", 
         "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "(Entry(\"fee\", \"fee\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fu\", \"fu\"))", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "(Entry(\"fee\", \"fee\"), Entry(\"fie\", \"fie\"), Entry(\"foe\", \"foe\"), Entry(\"fam\", \"fam\"))", 
         "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "(Entry(\"fee\", \"fee\"), Entry(\"fie\", \"fie\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"))",
         "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "(Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fam\", \"fam\"), Entry(\"foe\", \"foe\"))", 
         "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"))", 
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(Entry(\"fex\", \"fex\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(Entry(\"fex\", \"fex\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(Entry(\"fex\", \"fex\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(Entry(\"fex\", \"fex\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fie\", \"fie\"), Entry(\"fee\", \"fee\"), Entry(\"fee\", \"fee\"))",
         "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(Entry(\"ho\", \"ho\"), Entry(\"hey\", \"hey\"), Entry(\"howdy\", \"howdy\"))", 
         "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "(Entry(\"ho\", \"ho\"), Entry(\"hello\", \"hello\"))", 
         "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "(Entry(\"hi\", \"hi\"), Entry(\"hey\", \"hey\"), Entry(\"howdy\", \"howdy\"))",
         "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "(Entry(\"hi\", \"hi\"), Entry(\"hello\", \"hello\"))", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(Entry(\"howdy\", \"howdy\"), Entry(\"hi\", \"hi\"), Entry(\"hello\", \"hello\"))", 
         "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(Entry(\"howdy\", \"howdy\"), Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"))", 
         "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "(Entry(\"howdy\", \"howdy\"), Entry(\"hello\", \"hello\"), Entry(\"hi\", \"hi\"))", 
         "\\(\\\"fum\\\", \\\"foe\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"foe\", \"foe\"))",
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"))", 
         "\\(\\\"hi\\\"\\)" -> "(Entry(\"hi\", \"hi\"))", 
         "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "(Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"))", 
         "\\(\\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(Entry(\"hi\", \"hi\"), Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"))", 
         "\\(\\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "(Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"), Entry(\"he\", \"he\"), Entry(\"he\", \"he\"))", 
         "\\(\\\"he\\\"\\)" -> "(Entry(\"he\", \"he\"))", 
         "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "(Entry(\"he\", \"he\"), Entry(\"hi\", \"hi\"))", 
         "\\(\\\"he\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "(Entry(\"he\", \"he\"), Entry(\"hi\", \"hi\"), Entry(\"hello\", \"hello\"))", 
         "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "(Entry(\"hello\", \"hello\"), Entry(\"hi\", \"hi\"))",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(Entry(\"hello\", \"hello\"), Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"))",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "(Entry(\"hello\", \"hello\"), Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"), Entry(\"he\", \"he\"), Entry(\"he\", \"he\"))",
         "\\(\\\"hello\\\", \\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "(Entry(\"hello\", \"hello\"), Entry(\"hi\", \"hi\"), Entry(\"hi\", \"hi\"), Entry(\"he\", \"he\"))",
         "\\(\\\"hello\\\", \\\"ho\\\"\\)" -> "(Entry(\"hello\", \"hello\"), Entry(\"ho\", \"ho\"))",
         "\\(\\\"ho\\\"\\)" -> "(Entry(\"ho\", \"ho\"))", 
         "\\(\\\"ho\\\", \\\"hi\\\"\\)" -> "(Entry(\"ho\", \"ho\"), Entry(\"hi\", \"hi\"))",
         "\\(\\\"HI\\\"\\)" -> "(Entry(\"HI\", \"HI\"))", 
         "\\(\\\"HI\\\", \\\"HELLO\\\"\\)" -> "(Entry(\"HI\", \"HI\"), Entry(\"HELLO\", \"HELLO\"))",
         "\\(\\\"HELLO\\\", \\\"HI\\\"\\)" -> "(Entry(\"HELLO\", \"HELLO\"), Entry(\"HI\", \"HI\"))",
         "\\(\\\"HELLO\\\", \\\"HO\\\"\\)" -> "(Entry(\"HELLO\", \"HELLO\"), Entry(\"HO\", \"HO\"))",
         "\\(\\\"HE\\\", \\\"HI\\\"\\)" -> "(Entry(\"HE\", \"HE\"), Entry(\"HI\", \"HI\"))",
         "\\(\\\"HI\\\", \\\"HE\\\"\\)" -> "(Entry(\"HI\", \"HI\"), Entry(\"HE\", \"HE\"))",
         "\\(\\\"HI\\\", \\\"HO\\\"\\)" -> "(Entry(\"HI\", \"HI\"), Entry(\"HO\", \"HO\"))",
         "\\(\\\"HO\\\"\\)" -> "(Entry(\"HO\", \"HO\"))", 
         "\\(\\\"HO\\\", \\\"HI\\\"\\)" -> "(Entry(\"HO\", \"HO\"), Entry(\"HI\", \"HI\"))",
         "\\(\\\"HO\\\", \\\"HELLO\\\"\\)" -> "(Entry(\"HO\", \"HO\"), Entry(\"HELLO\", \"HELLO\"))",
         "\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(Entry(\"happy\", \"happy\"), Entry(\"birthday\", \"birthday\"), Entry(\"to\", \"to\"), Entry(\"you\", \"you\"))",
         "\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\", \\\"too\\\"\\)" -> "(Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"), Entry(\"birthday\", \"birthday\"), Entry(\"to\", \"to\"), Entry(\"you\", \"you\"), Entry(\"too\", \"too\"))",
         "\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"), Entry(\"birthday\", \"birthday\"), Entry(\"to\", \"to\"), Entry(\"you\", \"you\"))",
         "\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(Entry(\"happy\", \"happy\"), Entry(\"to\", \"to\"), Entry(\"you\", \"you\"))",
         "\\(\\\"have\\\", \\\"a\\\", \\\"nice\\\", \\\"day\\\"\\)" -> "(Entry(\"have\", \"have\"), Entry(\"a\", \"a\"), Entry(\"nice\", \"nice\"), Entry(\"day\", \"day\"))", 
         "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "(Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"), Entry(\"fum\", \"fum\"))",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUM \\\"\\)" -> "(Entry(\" FEE \", \" FEE \"), Entry(\" FIE \", \" FIE \"), Entry(\" FOE \", \" FOE \"), Entry(\" FUM \", \" FUM \"))",
         "\\(\\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FUM\", \"FUM\"), Entry(\"FOE\", \"FOE\"), Entry(\"FU\", \"FU\"))", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FOE\", \"FOE\"), Entry(\"FUM\", \"FUM\"))", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FUM\", \"FUM\"), Entry(\"FOE\", \"FOE\"))", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FUM\", \"FUM\"), Entry(\"FOE\", \"FOE\"))", 
         "\\(\\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\", \\\"FU\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FAM\", \"FAM\"), Entry(\"FOE\", \"FOE\"), Entry(\"FU\", \"FU\"))", 
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FAM \\\"\\)" -> "(Entry(\" FEE \", \" FEE \"), Entry(\" FIE \", \" FIE \"), Entry(\" FOE \", \" FOE \"), Entry(\" FAM \", \" FAM \"))", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FOE\", \"FOE\"), Entry(\"FAM\", \"FAM\"))", 
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FAM\", \"FAM\"), Entry(\"FOE\", \"FOE\"))",
         "\\(\\\" FEE \\\", \\\" FIE \\\", \\\" FOE \\\", \\\" FUU \\\"\\)" -> "(Entry(\" FEE \", \" FEE \"), Entry(\" FIE \", \" FIE \"), Entry(\" FOE \", \" FOE \"), Entry(\" FUU \", \" FUU \"))",
         "\\(\\\"FEE\\\", \\\"FIE\\\", \\\"FOE\\\", \\\"FUU\\\"\\)" -> "(Entry(\"FEE\", \"FEE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FOE\", \"FOE\"), Entry(\"FUU\", \"FUU\"))",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FAM\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FAM\", \"FAM\"), Entry(\"FOE\", \"FOE\"))",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUM\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FUM\", \"FUM\"), Entry(\"FOE\", \"FOE\"))",
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FAM\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FOE\", \"FOE\"), Entry(\"FAM\", \"FAM\"))", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FOE\\\", \\\"FUM\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FOE\", \"FOE\"), Entry(\"FUM\", \"FUM\"))", 
         "\\(\\\"FIE\\\", \\\"FEE\\\", \\\"FUU\\\", \\\"FOE\\\"\\)" -> "(Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"), Entry(\"FUU\", \"FUU\"), Entry(\"FOE\", \"FOE\"))", 
         "\\(\\\"FUM\\\", \\\"FOE\\\", \\\"FIE\\\", \\\"FEE\\\"\\)" -> "(Entry(\"FUM\", \"FUM\"), Entry(\"FOE\", \"FOE\"), Entry(\"FIE\", \"FIE\"), Entry(\"FEE\", \"FEE\"))",
         "\\(\\\" FUM \\\", \\\" FOE \\\", \\\" FIE \\\", \\\" FEE \\\"\\)" -> "(Entry(\" FUM \", \" FUM \"), Entry(\" FOE \", \" FOE \"), Entry(\" FIE \", \" FIE \"), Entry(\" FEE \", \" FEE \"))",
         "\\(\\\"HAPPY\\\", \\\"BIRTHDAY\\\", \\\"TO\\\", \\\"YOU\\\"\\)" -> "(Entry(\"HAPPY\", \"HAPPY\"), Entry(\"BIRTHDAY\", \"BIRTHDAY\"), Entry(\"TO\", \"TO\"), Entry(\"YOU\", \"YOU\"))", 
         "\\(\\\" HAPPY \\\", \\\" BIRTHDAY \\\", \\\" TO \\\", \\\" YOU \\\"\\)" -> "(Entry(\" HAPPY \", \" HAPPY \"), Entry(\" BIRTHDAY \", \" BIRTHDAY \"), Entry(\" TO \", \" TO \"), Entry(\" YOU \", \" YOU \"))",
         "\\(\\\"nice\\\", \\\"to\\\", \\\"you\\\"\\)" -> "(Entry(\"nice\", \"nice\"), Entry(\"to\", \"to\"), Entry(\"you\", \"you\"))", 
         "\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "(Entry(\"nice\", \"nice\"), Entry(\"you\", \"you\"), Entry(\"to\", \"to\"))", 
         "\\(\\\"to\\\", \\\"you\\\"\\)" -> "(Entry(\"to\", \"to\"), Entry(\"you\", \"you\"))", 
         "\\(\\\"to\\\", \\\"to\\\", \\\"to\\\", \\\"to\\\"\\)"  -> "(Entry(\"to\", \"to\"), Entry(\"to\", \"to\"), Entry(\"to\", \"to\"), Entry(\"to\", \"to\"))", 
         "\\(\\\"TO\\\", \\\"YOU\\\"\\)" -> "(Entry(\"TO\", \"TO\"), Entry(\"YOU\", \"YOU\"))",
         "\\(\\\" TO \\\", \\\" YOU \\\"\\)" -> "(Entry(\" TO \", \" TO \"), Entry(\" YOU \", \" YOU \"))",
         "\\(\\\" TO \\\", \\\" TO \\\", \\\" TO \\\", \\\" TO \\\"\\)" -> "(Entry(\" TO \", \" TO \"), Entry(\" TO \", \" TO \"), Entry(\" TO \", \" TO \"), Entry(\" TO \", \" TO \"))", 
         "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(Entry(\"too\", \"too\"), Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"too\\\", \\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "(Entry(\"too\", \"too\"), Entry(\"you\", \"you\"), Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(Entry(\"too\", \"too\"), Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "(Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "(Entry(\"you\", \"you\"), Entry(\"you\", \"you\"), Entry(\"to\", \"to\"), Entry(\"birthday\", \"birthday\"), Entry(\"happy\", \"happy\"), Entry(\"happy\", \"happy\"))",
         "\\(\\\"you\\\", \\\"to\\\"\\)" -> "(Entry(\"you\", \"you\"), Entry(\"to\", \"to\"))",
         "\\(\\\"YOU\\\", \\\"TO\\\"\\)" -> "(Entry(\"YOU\", \"YOU\"), Entry(\"TO\", \"TO\"))",
         "\\(\\\" YOU \\\", \\\" TO \\\"\\)" -> "(Entry(\" YOU \", \" YOU \"), Entry(\" TO \", \" TO \"))",
         "\\\"\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\\\"\\\"" -> "\"happy=happy, birthday=birthday, to=to, you=you\"",
         "\\\"\\\\\"HAPPY\\\\\", \\\\\"BIRTHDAY\\\\\", \\\\\"TO\\\\\", \\\\\"YOU\\\\\\\"\\\"" -> "\"HAPPY=HAPPY, BIRTHDAY=BIRTHDAY, TO=TO, YOU=YOU\"",
         "\\\\\"ho\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "ho=ho, hey=hey, howdy=howdy", 
         "\\\\\"ho\\\\\", \\\\\"hello\\\\\"" -> "ho=ho, hello=hello", 
         "\\\\\"hi\\\\\", \\\\\"hey\\\\\", \\\\\"howdy\\\\\"" -> "hi=hi, hey=hey, howdy=howdy", 
         "\\\\\"hello\\\\\", \\\\\"hi\\\\\"" -> "hello=hello, hi=hi", 
         "\\\\\"happy\\\\\", \\\\\"birthday\\\\\", \\\\\"to\\\\\", \\\\\"you\\\\\"" -> "happy=happy, birthday=birthday, to=to, you=you", 
         "\\\\\"have\\\\\", \\\\\"a\\\\\", \\\\\"nice\\\\\", \\\\\"day\\\\\"" -> "have=have, a=a, nice=nice, day=day",
         "\\\\\"HELLO\\\\\", \\\\\"HI\\\\\"" -> "HELLO=HELLO, HI=HI", 
         "\\\\\"hi\\\\\", \\\\\"he\\\\\"" -> "hi=hi, he=he", 
         "\\\\\"hi\\\\\"" -> "hi=hi", 
         "\\\\\"ho\\\\\"" -> "ho=ho", 
         "\\\\\"hello\\\\\"" -> "hello=hello", 
         "\\\\\"HE\\\\\", \\\\\"HI\\\\\"" -> "HE=HE, HI=HI", 
         "\\\\\"HI\\\\\", \\\\\"HE\\\\\"" -> "HI=HI, HE=HE", 
         "\\\\\"HI\\\\\"" -> "HI=HI", 
         "\\\\\"HO\\\\\"" -> "HO=HO", 
         "\\\\\"HELLO\\\\\"" -> "HELLO=HELLO", 
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "fee=fee, fie=fie, foe=foe, fum=fum",
         "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "fee=fee, fum=fum, foe=foe, fu=fu",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "fee=fee, fie=fie, foe=foe, fam=fam",
         "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "fee=fee, fie=fie, fum=fum, foe=foe",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "fie=fie, fee=fee, fum=fum, foe=foe",
         "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "fie=fie, fee=fee, fam=fam, foe=foe",
         "\\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fie\\\\\", \\\\\"fee\\\\\"" -> "fum=fum, foe=foe, fie=fie, fee=fee",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "fum=fum, fum=fum, fum=fum, fum=fum",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "fum=fum, fum=fum, fum=fum",
         "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "fum=fum, fum=fum",
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "FEE=FEE, FIE=FIE, FOE=FOE, FUM=FUM", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUU\\\\\"" -> "FEE=FEE, FIE=FIE, FOE=FOE, FUU=FUU", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "FEE=FEE, FIE=FIE, FOE=FOE, FAM=FAM", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "FEE=FEE, FIE=FIE, FAM=FAM, FOE=FOE", 
         "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "FEE=FEE, FIE=FIE, FUM=FUM, FOE=FOE",
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "FIE=FIE, FEE=FEE, FUM=FUM, FOE=FOE", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "FIE=FIE, FEE=FEE, FAM=FAM, FOE=FOE", 
         "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "FIE=FIE, FEE=FEE, FOE=FOE, FUM=FUM", 
         "\\\\\"FUM\\\\\", \\\\\"FOE\\\\\", \\\\\"FIE\\\\\", \\\\\"FEE\\\\\"" -> "FUM=FUM, FOE=FOE, FIE=FIE, FEE=FEE", 
         "\\\\\"you\\\\\"" -> "you=you", 
         "\\\\\"to\\\\\"" -> "to=to", 
         "of \\(1, 2, 3\\)" -> "of (1=1, 2=2, 3=3)", 
         "of \\(1, 2, 8\\)" -> "of (1=1, 2=2, 8=8)", 
         "of \\(1, 3, 4\\)" -> "of (1=1, 3=3, 4=4)", 
         "of \\(1, 6, 8\\)" -> "of (1=1, 6=6, 8=8)", 
         "of \\(2, 3, 1\\)" -> "of (2=2, 3=3, 1=1)", 
         "of \\(2, 3, 4\\)" -> "of (2=2, 3=3, 4=4)", 
         "of \\(2, 3, 5\\)" -> "of (2=2, 3=3, 5=5)", 
         "of \\(2, 3, 8\\)" -> "of (2=2, 3=3, 8=8)", 
         "of \\(2, 6, 8\\)" -> "of (2=2, 6=6, 8=8)", 
         "of \\(3, 6, 8\\)" -> "of (3=3, 6=6, 8=8)", 
         "of \\(3, 6, 9\\)" -> "of (3=3, 6=6, 9=9)", 
         "of \\(4, 3, 2\\)" -> "of (4=4, 3=3, 2=2)", 
         "of \\(6, 7, 8\\)" -> "of (6=6, 7=7, 8=8)", 
         "of \\(\\\\\"ho\\\\\"\\)" -> "of (ho=ho)", 
         "of \\(\\\\\"hi\\\\\"\\)" -> "of (hi=hi)", 
         "of \\(\\\\\"he\\\\\"\\)" -> "of (he=he)", 
         "of \\(\\\\\"hi\\\\\", \\\\\"hello\\\\\"\\)" -> "of (hi=hi, hello=hello)", 
         "of \\(\\\\\"HI\\\\\"\\)" -> "of (HI=HI)", 
         "\\\"\\(1, 2, 3\\)\\\"" -> "\"(1=1, 2=2, 3=3)\"", 
         "\\\"\\(1, 3, 2\\)\\\"" -> "\"(1=1, 3=3, 2=2)\"", 
         "\\\"\\(1, 3, 4\\)\\\"" -> "\"(1=1, 3=3, 4=4)\"", 
         "\\\"\\(2, 3, 1\\)\\\"" -> "\"(2=2, 3=3, 1=1)\"", 
         "\\\"\\(2, 3, 4\\)\\\"" -> "\"(2=2, 3=3, 4=4)\"", 
         "\\\"\\(2, 3, 8\\)\\\"" -> "\"(2=2, 3=3, 8=8)\"", 
         "\\\"\\(3, 1, 2\\)\\\"" -> "\"(3=3, 1=1, 2=2)\"", 
         "\\\"\\(3, 6, 8\\)\\\"" -> "\"(3=3, 6=6, 8=8)\"", 
         "\\\"\\(4, 2, 3\\)\\\"" -> "\"(4=4, 2=2, 3=3)\"", 
        "List\\(to\\)" -> "javaMap(Entry(to, to))", 
        "List\\(ho\\)" -> "javaMap(Entry(ho, ho))", 
        "List\\(hi\\)" -> "javaMap(Entry(hi, hi))",
        "List\\(hey\\)" -> "javaMap(Entry(hey, hey))",
        "\\(0, 1, 1, 1, 2, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3))",
        "\\(0, 1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))",
        "\\(0, 1, 1, 2, 3, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3))", 
        "\\(0, 1, 1, 2, 3, 3, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))", 
        "\\(0, 1, 2, 2, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(3, 3))", 
        "\\(0, 1, 2, 2, 3, 3, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))", 
        "\\(0, 1, 2, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(2, 2), Entry(3, 3))", 
        "\\(0, 1, 2, 3, 3\\)" -> "(Entry(0, 0), Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3))",
        "\\(1, 1, 1, 2, 3\\)" -> "(Entry(1, 1), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3))",
        "\\(1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "(Entry(1, 1), Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))", 
        "\\(1, 1, 2, 3, 3\\)" -> "(Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3))", 
        "\\(1, 1, 2, 3, 3, 3\\)" -> "(Entry(1, 1), Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))", 
        "\\(1, 2, 2, 3\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(3, 3))", 
        "\\(1, 2, 2, 3, 3, 3\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(3, 3), Entry(3, 3))", 
        "\\(1, 2, 3\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(3, 3))",
        "\\(1, 2, 3, 3\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(3, 3), Entry(3, 3))", 
        "\\(1, 2, 5\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(5, 5))", 
        "\\(1, 2, 8\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(8, 8))", 
        "\\(1, 2, 9\\)" -> "(Entry(1, 1), Entry(2, 2), Entry(9, 9))", 
        "\\(1, 3, 2\\)" -> "(Entry(1, 1), Entry(3, 3), Entry(2, 2))", 
        "\\(1, 3, 4\\)" -> "(Entry(1, 1), Entry(3, 3), Entry(4, 4))", 
        "\\(1, 3, 8\\)" -> "(Entry(1, 1), Entry(3, 3), Entry(8, 8))", 
        "\\(1, 6, 8\\)" -> "(Entry(1, 1), Entry(6, 6), Entry(8, 8))", 
        "\\(2, 1, 5\\)" -> "(Entry(2, 2), Entry(1, 1), Entry(5, 5))", 
        "\\(2, 2, 3, 4\\)" -> "(Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(4, 4))", 
        "\\(2, 3, 1\\)" -> "(Entry(2, 2), Entry(3, 3), Entry(1, 1))", 
        "\\(2, 3, 4\\)" -> "(Entry(2, 2), Entry(3, 3), Entry(4, 4))", 
        "\\(2, 3, 5\\)" -> "(Entry(2, 2), Entry(3, 3), Entry(5, 5))", 
        "\\(2, 3, 8\\)" -> "(Entry(2, 2), Entry(3, 3), Entry(8, 8))", 
        "\\(2, 6, 8\\)" -> "(Entry(2, 2), Entry(6, 6), Entry(8, 8))", 
        "\\(3, 1, 2\\)" -> "(Entry(3, 3), Entry(1, 1), Entry(2, 2))", 
        "\\(3, 1, 5\\)" -> "(Entry(3, 3), Entry(1, 1), Entry(5, 5))", 
        "\\(3, 2, 1\\)" -> "(Entry(3, 3), Entry(2, 2), Entry(1, 1))", 
        "\\(3, 2, 1, 0\\)" -> "(Entry(3, 3), Entry(2, 2), Entry(1, 1), Entry(0, 0))", 
        "\\(3, 2, 8\\)" -> "(Entry(3, 3), Entry(2, 2), Entry(8, 8))", 
        "\\(3, 4, 2\\)" -> "(Entry(3, 3), Entry(4, 4), Entry(2, 2))", 
        "\\(3, 4, 5\\)" -> "(Entry(3, 3), Entry(4, 4), Entry(5, 5))", 
        "\\(3, 6, 5\\)" -> "(Entry(3, 3), Entry(6, 6), Entry(5, 5))", 
        "\\(3, 6, 8\\)" -> "(Entry(3, 3), Entry(6, 6), Entry(8, 8))", 
        "\\(3, 6, 9\\)" -> "(Entry(3, 3), Entry(6, 6), Entry(9, 9))", 
        "\\(3, 8, 5\\)" -> "(Entry(3, 3), Entry(8, 8), Entry(5, 5))", 
        "\\(4, 2, 3\\)" -> "(Entry(4, 4), Entry(2, 2), Entry(3, 3))", 
        "\\(4, 3, 2\\)" -> "(Entry(4, 4), Entry(3, 3), Entry(2, 2))", 
        "\\(5, 3, 4\\)" -> "(Entry(5, 5), Entry(3, 3), Entry(4, 4))", 
        "\\(5, 7, 9\\)" -> "(Entry(5, 5), Entry(7, 7), Entry(9, 9))", 
        "\\(6, 7, 8\\)" -> "(Entry(6, 6), Entry(7, 7), Entry(8, 8))", 
        "\\(8, 2, 3, 4\\)" -> "(Entry(8, 8), Entry(2, 2), Entry(3, 3), Entry(4, 4))", 
        "\\(8, 2, 2, 3, 4\\)" -> "(Entry(8, 8), Entry(2, 2), Entry(2, 2), Entry(3, 3), Entry(4, 4))", 
        "\\(8, 3, 1\\)" -> "(Entry(8, 8), Entry(3, 3), Entry(1, 1))", 
        "\\(8, 3, 4\\)" -> "(Entry(8, 8), Entry(3, 3), Entry(4, 4))", 
        "\\(8, 4, 3, 2\\)" -> "(Entry(8, 8), Entry(4, 4), Entry(3, 3), Entry(2, 2))", 
        "\\(1, 3, Nil\\)" -> "(Entry(1, 1), Entry(3, 3), Map())", 
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
        "List\\(\\\"fum\\\", \\\"foe\\\"\\)" -> "\"up\"",
        "List\\(\\\"fum\\\", \\\"fu\\\"\\)" -> "\"uf\"",
        "List\\(\\\"to\\\"\\)" -> "\"o\"",
        "List\\(\\\"ho\\\"\\)" -> "\"o\"",
        "List\\(1\\)" -> "\"1\"", 
        "List\\(2\\)" -> "\"2\"", 
        "List\\(3\\)" -> "\"3\"", 
        "List\\(8\\)" -> "\"8\"",
        "List\\(1, 2\\)" -> "\"12\"",
        "List\\(2, 3\\)" -> "\"23\"", 
        "List\\(0, 1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "\"0111222333\"", 
        "List\\(0, 1, 2, 2, 3\\)" -> "\"01223\"", 
        "List\\(0, 1, 2, 3, 3\\)" -> "\"01233\"", 
        "List\\(0, 1, 2, 3\\)" -> "\"0123\"", 
        "List\\(0, 1, 1, 1, 2, 3\\)" -> "\"011123\"", 
        "List\\(0, 1, 1, 2, 3, 3\\)" -> "\"011233\"", 
        "List\\(0, 1, 1, 2, 3, 3, 3\\)" -> "\"0112333\"", 
        "List\\(0, 1, 2, 2, 3, 3, 3\\)" -> "\"0122333\"", 
        "List\\(1, 1, 1, 2, 3\\)" -> "\"11123\"", 
        "List\\(1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "\"111222333\"", 
        "List\\(1, 1, 2, 3, 3\\)" -> "\"11233\"", 
        "List\\(1, 1, 2, 3, 3, 3\\)" -> "\"112333\"", 
        "List\\(1, 2, 3\\)" -> "\"123\"", 
        "List\\(1, 2, 3, 3\\)" -> "\"1233\"", 
        "List\\(1, 2, 2, 3\\)" -> "\"1223\"", 
        "List\\(1, 2, 2, 3, 3, 3\\)" -> "\"122333\"", 
        "List\\(2, 2, 3, 4\\)" -> "\"2234\"",
        "List\\(2, 3, 4\\)" -> "\"234\"", 
        "List\\(3, 2, 1\\)" -> "\"321\"", 
        "List\\(3, 2, 1, 0\\)" -> "\"3210\"",
        "List\\(4, 3, 2\\)" -> "\"432\"",
        "List\\(8, 2, 3, 4\\)" -> "\"8234\"", 
        "List\\(8, 2, 2, 3, 4\\)" -> "\"82234\"", 
        "List\\(8, 4, 3, 2\\)" -> "\"8432\"",
        "List\\(\\\"hi\\\"\\)" -> "\"i\"", 
        "List\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "\"il\"", 
        "List\\(\\\"he\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "\"eil\"", 
        "List\\(\\\"hello\\\", \\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "\"liie\"", 
        "List\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "\"lieee\"", 
        "List\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "\"lie\"", 
        "List\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "\"dil\"", 
        "List\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "\"die\"", 
        "List\\(\\\"hi\\\", \\\"he\\\"\\)" -> "\"ie\"", 
        "List\\(\\\"hi\\\", \\\"ho\\\"\\)" -> "\"io\"", 
        "List\\(\\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "\"iie\"", 
        "List\\(\\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "\"ieee\"",
        "List\\(\\\"to\\\", \\\"you\\\"\\)" -> "\"oy\"", 
        "List\\(\\\"happy\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"hoy\"", 
        "List\\(\\\"you\\\", \\\"to\\\"\\)" -> "\"yo\"", 
        "List\\(\\\"nice\\\", \\\"you\\\", \\\"to\\\"\\)" -> "\"nyo\"", 
        "List\\(\\\"nice\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"noy\"", 
        "List\\(\\\"hey\\\"\\)" -> "\"e\"",
        "List\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"upie\"", 
        "List\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"uupiie\"", 
        "List\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "\"uupieee\"", 
        "List\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "\"uupiiiee\"", 
        "List\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"xupie\"", 
        "List\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "\"xuupiie\"", 
        "List\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "\"xuupieee\"", 
        "List\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "\"xuupiiiee\"", 
        "List\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"yobh\"", 
        "List\\(\\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"yoobbh\"", 
        "List\\(\\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "\"yyobhh\"", 
        "List\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"zyobh\"", 
        "List\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "\"zyoobbh\"", 
        "List\\(\\\"too\\\", \\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "\"zyyobhh\"", 
        "List\\(\\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"hboy\"", 
        "List\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\", \\\"too\\\"\\)" -> "\"hhhboyz\"", 
        "List\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\"\\)" -> "\"hhhboy\"", 
        "TempL" -> "LinkedList", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fum\\\"\\)" -> "('e', 'i', 'p', 'u')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'u', 'p')", 
        "\\(\\\"fee\\\", \\\"fum\\\", \\\"foe\\\", \\\"fu\\\"\\)" -> "('e', 'u', 'p', 'f')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"fum\\\", \\\"foe\\\"\\)" -> "('e', 'i', 'u', 'p')", 
        "\\(\\\"fie\\\", \\\"fee\\\", \\\"fam\\\", \\\"foe\\\"\\)" -> "('i', 'e', 'a', 'p')", 
        "\\(\\\"fee\\\", \\\"fie\\\", \\\"foe\\\", \\\"fam\\\"\\)" -> "('e', 'i', 'p', 'a')", 
        "\\(\\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('u', 'p', 'i', 'e')", 
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('x', 'u', 'p', 'i', 'e')",
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('x', 'u', 'u', 'p', 'i', 'i', 'e')",
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "('x', 'u', 'u', 'p', 'i', 'e', 'e', 'e')",
        "\\(\\\"fex\\\", \\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\", \\\"fee\\\"\\)" -> "('x', 'u', 'u', 'p', 'i', 'i', 'i', 'e', 'e')",
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"foe\\\", \\\"fie\\\", \\\"fie\\\", \\\"fee\\\"\\)" -> "('u', 'u', 'p', 'i', 'i', 'e')",
        "\\(\\\"ho\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('o', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hey\\\", \\\"howdy\\\"\\)" -> "('i', 'e', 'd')", 
        "\\(\\\"hi\\\", \\\"hello\\\"\\)" -> "('i', 'l')", 
        "\\(\\\"ho\\\", \\\"hello\\\"\\)" -> "('o', 'l')", 
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "('d', 'i', 'l')", 
        "\\(\\\"howdy\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "('d', 'i', 'e')", 
        "\\(\\\"howdy\\\", \\\"hello\\\", \\\"hi\\\"\\)" -> "('d', 'l', 'i')",
        "\\(\\\"fum\\\", \\\"foe\\\"\\)" -> "('u', 'p')",
        "\\(\\\"fum\\\", \\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u', 'u')", 
        "\\(\\\"fum\\\", \\\"fum\\\"\\)" -> "('u', 'u')", 
        "\\(\\\"hi\\\"\\)" -> "('i')", 
        "\\(\\\"hi\\\", \\\"he\\\"\\)" -> "('i', 'e')", 
        "\\(\\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "('i', 'e', 'e', 'e')", 
        "\\(\\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "('i', 'i', 'e')", 
        "\\(\\\"he\\\"\\)" -> "('e')", 
        "\\(\\\"he\\\", \\\"hi\\\"\\)" -> "('e', 'i')", 
        "\\(\\\"he\\\", \\\"hi\\\", \\\"hello\\\"\\)" -> "('e', 'i', 'l')", 
        "\\(\\\"hello\\\", \\\"hi\\\"\\)" -> "('l', 'i')",
        "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "('l', 'i', 'e')",
        "\\(\\\"hello\\\", \\\"hi\\\", \\\"he\\\", \\\"he\\\", \\\"he\\\"\\)" -> "('l', 'i', 'e', 'e', 'e')",
        "\\(\\\"hello\\\", \\\"hi\\\", \\\"hi\\\", \\\"he\\\"\\)" -> "('l', 'i', 'i', 'e')",
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
        "\\(\\\"happy\\\", \\\"happy\\\", \\\"happy\\\", \\\"birthday\\\", \\\"to\\\", \\\"you\\\", \\\"too\\\"\\)" -> "('h', 'h', 'h', 'b', 'o', 'y', 'z')",
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
        "\\(\\\"too\\\", \\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "('z', 'y', 'y', 'o', 'b', 'h', 'h')",
        "\\(\\\"too\\\", \\\"you\\\", \\\"to\\\", \\\"to\\\", \\\"birthday\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "('z', 'y', 'o', 'o', 'b', 'b', 'h')",
        "\\(\\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\"\\)" -> "('y', 'o', 'b', 'h')",
        "\\(\\\"you\\\", \\\"you\\\", \\\"to\\\", \\\"birthday\\\", \\\"happy\\\", \\\"happy\\\"\\)" -> "('y', 'y', 'o', 'b', 'h', 'h')",
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
        "\\\\\"hi\\\\\", \\\\\"he\\\\\"" -> "'i', 'e'", 
        "\\\\\"hi\\\\\"" -> "'i'", 
        "\\\\\"ho\\\\\"" -> "'o'", 
        "\\\\\"hello\\\\\"" -> "'l'", 
        "\\\\\"HE\\\\\", \\\\\"HI\\\\\"" -> "'E', 'I'", 
        "\\\\\"HI\\\\\", \\\\\"HE\\\\\"" -> "'I', 'E'", 
        "\\\\\"HI\\\\\"" -> "'I'", 
        "\\\\\"HO\\\\\"" -> "'O'", 
        "\\\\\"HELLO\\\\\"" -> "'L'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fum\\\\\"" -> "'e', 'i', 'p', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"foe\\\\\", \\\\\"fam\\\\\"" -> "'e', 'i', 'p', 'a'", 
        "\\\\\"fee\\\\\", \\\\\"fie\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'e', 'i', 'u', 'p'", 
        "\\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fie\\\\\", \\\\\"fee\\\\\"" -> "'u', 'p', 'i', 'e'", 
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u', 'u'",
        "\\\\\"fum\\\\\", \\\\\"fum\\\\\"" -> "'u', 'u'",
        "\\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\", \\\\\"fu\\\\\"" -> "'e', 'u', 'p', 'f'", 
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fum\\\\\", \\\\\"foe\\\\\"" -> "'i', 'e', 'u', 'p'", 
        "\\\\\"fie\\\\\", \\\\\"fee\\\\\", \\\\\"fam\\\\\", \\\\\"foe\\\\\"" -> "'i', 'e', 'a', 'p'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "'E', 'I', 'P', 'U'", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUU\\\\\"" -> "'E', 'I', 'P', 'D'", 
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FOE\\\\\", \\\\\"FAM\\\\\"" -> "'E', 'I', 'P', 'A'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "'E', 'I', 'A', 'P'",
        "\\\\\"FEE\\\\\", \\\\\"FIE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'E', 'I', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FUM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'U', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FAM\\\\\", \\\\\"FOE\\\\\"" -> "'I', 'E', 'A', 'P'", 
        "\\\\\"FIE\\\\\", \\\\\"FEE\\\\\", \\\\\"FOE\\\\\", \\\\\"FUM\\\\\"" -> "'I', 'E', 'P', 'U'", 
        "\\\\\"FUM\\\\\", \\\\\"FOE\\\\\", \\\\\"FIE\\\\\", \\\\\"FEE\\\\\"" -> "'U', 'P', 'I', 'E'", 
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
        "of \\(4, 3, 2\\)" -> "of ('4', '3', '2')", 
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
        "\\(0, 1, 1, 1, 2, 3\\)" -> "('0', '1', '1', '1', '2', '3')",
        "\\(0, 1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "('0', '1', '1', '1', '2', '2', '2', '3', '3', '3')",
        "\\(0, 1, 1, 2, 3, 3\\)" -> "('0', '1', '1', '2', '3', '3')", 
        "\\(0, 1, 1, 2, 3, 3, 3\\)" -> "('0', '1', '1', '2', '3', '3', '3')", 
        "\\(0, 1, 2, 2, 3\\)" -> "('0', '1', '2', '2', '3')", 
        "\\(0, 1, 2, 2, 3, 3, 3\\)" -> "('0', '1', '2', '2', '3', '3', '3')", 
        "\\(0, 1, 2, 3\\)" -> "('0', '1', '2', '3')", 
        "\\(0, 1, 2, 3, 3\\)" -> "('0', '1', '2', '3', '3')",
        "\\(1, 1, 1, 2, 3\\)" -> "('1', '1', '1', '2', '3')",
        "\\(1, 1, 1, 2, 2, 2, 3, 3, 3\\)" -> "('1', '1', '1', '2', '2', '2', '3', '3', '3')", 
        "\\(1, 1, 2, 3, 3\\)" -> "('1', '1', '2', '3', '3')", 
        "\\(1, 1, 2, 3, 3, 3\\)" -> "('1', '1', '2', '3', '3', '3')", 
        "\\(1, 2, 2, 3\\)" -> "('1', '2', '2', '3')", 
        "\\(1, 2, 2, 3, 3, 3\\)" -> "('1', '2', '2', '3', '3', '3')", 
        "\\(1, 2, 3\\)" -> "('1', '2', '3')", 
        "\\(1, 2, 3, 3\\)" -> "('1', '2', '3', '3')", 
        "\\(1, 2, 5\\)" -> "('1', '2', '5')", 
        "\\(1, 2, 8\\)" -> "('1', '2', '8')", 
        "\\(1, 2, 9\\)" -> "('1', '2', '9')", 
        "\\(1, 3, 2\\)" -> "('1', '3', '2')", 
        "\\(1, 3, 4\\)" -> "('1', '3', '4')", 
        "\\(1, 3, 8\\)" -> "('1', '3', '8')", 
        "\\(1, 6, 8\\)" -> "('1', '6', '8')", 
        "\\(2, 1, 5\\)" -> "('2', '1', '5')", 
        "\\(2, 2, 3, 4\\)" -> "('2', '2', '3', '4')", 
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
        "\\(3, 6, 5\\)" -> "('3', '6', '5')",
        "\\(3, 6, 8\\)" -> "('3', '6', '8')", 
        "\\(3, 6, 9\\)" -> "('3', '6', '9')", 
        "\\(3, 8, 5\\)" -> "('3', '8', '5')", 
        "\\(4, 2, 3\\)" -> "('4', '2', '3')", 
        "\\(4, 3, 2\\)" -> "('4', '3', '2')", 
        "\\(5, 3, 4\\)" -> "('5', '3', '4')", 
        "\\(5, 7, 9\\)" -> "('5', '7', '9')", 
        "\\(8, 2, 3, 4\\)" -> "('8', '2', '3', '4')", 
        "\\(8, 2, 2, 3, 4\\)" -> "('8', '2', '2', '3', '4')", 
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
        "decorateToStringValue\\(\\\"8234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[8234]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(\\\"8432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[8432]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(\\\"8432\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3210\\\"\\)" -> "decorateToStringValue(\"[8432]\") + \" was not equal to \" + decorateToStringValue(\"[3210]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"321\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[321]\")", 
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"123\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[123]\")",
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"1233\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[1233]\")",
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(\\\"123\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[123]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"1223\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[1223]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"321\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[321]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"3210\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(\"[3210]\") + \" was not equal to \" + decorateToStringValue(\"[234]\")",
        "decorateToStringValue\\(\\\"234\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"321\\\"\\)" -> "decorateToStringValue(\"[234]\") + \" was not equal to \" + decorateToStringValue(\"[321]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"123\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[123]\")",
        "decorateToStringValue\\(\\\"\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3210\\\"\\)" -> "decorateToStringValue(\"[]\") + \" was not equal to \" + decorateToStringValue(\"[3210]\")",
        "decorateToStringValue\\(\\\"dil\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[dil]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")",
        "decorateToStringValue\\(\\\"eil\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[eil]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")",
        "decorateToStringValue\\(\\\"il\\\"\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[il]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")", 
        "decorateToStringValue\\(nils\\(0\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"e\\\"\\)" -> "decorateToStringValue(\"[\" + nils(0) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[e]\")",
        "decorateToStringValue\\(listsString\\(2\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"0123\\\"\\)" -> "decorateToStringValue(\"[\" + listsString(2) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[0123]\")",
        "decorateToStringValue\\(lists\\(2\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"3\\\"\\)" -> "decorateToStringValue(\"[\" + lists(2) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[3]\")",
        "decorateToStringValue\\(lists\\(2\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"12\\\"\\)" -> "decorateToStringValue(\"[\" + lists(2) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[12]\")",
        "decorateToStringValue\\(lists\\(2\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"234\\\"\\)" -> "decorateToStringValue(lists(2) + \"[]\") + \" was not equal to \" + decorateToStringValue(\"23[4]\")",
        "decorateToStringValue\\(hiLists\\(0\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"o\\\"\\)" -> "decorateToStringValue(\"[\" + hiLists(0) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[o]\")",
        "decorateToStringValue\\(hiLists\\(0\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"io\\\"\\)" -> "decorateToStringValue(\"i[e]\") + \" was not equal to \" + decorateToStringValue(\"i[o]\")",
        "decorateToStringValue\\(list1s\\(0\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"8234\\\"\\)" -> "decorateToStringValue(\"[\" + list1s(0) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[8234]\")",
        "decorateToStringValue\\(list1s\\(0\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"23\\\"\\)" -> "decorateToStringValue(\"[\" + list1s(0) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[23]\")",
        "decorateToStringValue\\(lists\\(2\\)\\) \\+ \\\" was not equal to \\\" \\+ decorateToStringValue\\(\\\"01233\\\"\\)" -> "decorateToStringValue(\"[\" + lists(2) + \"]\") + \" was not equal to \" + decorateToStringValue(\"[01233]\")"
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
    // generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    // generateFile("ListShouldContainTheSameElementsInOrderAsSpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainTheSameElementsInOrderAsLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainTheSameElementsInOrderAsLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
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
      
    // Generate tests for inOrderOnly
    generateFile("ListShouldContainInOrderOnlySpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "Array", arrayMapping: _*)
    // generateFile("ListShouldContainInOrderOnlySpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    // generateFile("ListShouldContainInOrderOnlySpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
    generateFile("ListShouldContainInOrderOnlySpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalAndSpec.scala", "String", stringMapping: _*)
    generateFile("ListShouldContainInOrderOnlyLogicalOrSpec.scala", "String", stringMapping: _*)
      
    // Generate tests for inOrder
    generateFile("ListShouldContainInOrderSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "Array", arrayMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "Array", arrayMapping: _*)
    // generateFile("ListShouldContainInOrderSpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "Map", mapMapping: _*)
    // generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "Map", mapMapping: _*)
    generateFile("ListShouldContainInOrderSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "JavaCol", javaColMapping: _*)
    generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "JavaCol", javaColMapping: _*)
    // generateFile("ListShouldContainInOrderSpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainInOrderLogicalAndSpec.scala", "JavaMap", javaMapMapping: _*)
    // generateFile("ListShouldContainInOrderLogicalOrSpec.scala", "JavaMap", javaMapMapping: _*)
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
    val scalaVersion = args(1)
    genTest(new File(targetDir + "/org/scalatest/"), scalaVersion)
  }
}
