/*
 * Copyright 2001-2024 Artima, Inc.
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
package org.scalatest

import matchers._
import SharedHelpers._
import FailureMessages.decorateToStringValue
import should.Matchers._
import org.scalactic.Prettifier

class AMatcherSpec extends funspec.AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("AMatcher ") {
    
    describe("when used with integer") {
      
      val positiveNumber = AMatcher[Int]("positive number") { _ > 0 }
      
      it("should work correctly with 'should be a'") {
        1 should be a positiveNumber
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          0 should be a positiveNumber
        }
        e.message should be (Some("0 was not a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be a'") {
        0 should not be a (positiveNumber)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          1 should not be a (positiveNumber)
        }
        e.message should be (Some("1 was a positive number"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when used with string") {
      
      val positiveLengthString = AMatcher[String]("positive length string") { _.length > 0 }
      
      it("should work correctly with 'should be'") {
        "hello" should be a positiveLengthString
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          "" should be a positiveLengthString
        }
        e.message should be (Some("\"\" was not a positive length string"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        "" should not be a (positiveLengthString)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          "hai" should not be a (positiveLengthString)
        }
        e.message should be (Some("\"hai\" was a positive length string"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with list") {

      val positiveLengthList = AMatcher[List[Int]]("positive length list") { _.length > 0 }
      
      it("should work correctly with 'should be'") {
        List(1, 2, 3) should be a positiveLengthList
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          List.empty[Int] should be a positiveLengthList
        }
        e.message should be (Some(s"${List.empty[Int].toString()} was not a positive length list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        List.empty[Int] should not be a (positiveLengthList)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should not be a (positiveLengthList)
        }
        e.message should be (Some(s"${List(1, 2, 3).toString()} was a positive length list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with set") {

      val positiveSizeSet = AMatcher[Set[Int]]("positive size set") { _.size > 0 }
      
      it("should work correctly with 'should be'") {
        Set(1, 2, 3) should be a positiveSizeSet
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Set.empty[Int] should be a positiveSizeSet
        }
        e.message should be (Some(s"${Set.empty[Int].toString()} was not a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Set.empty[Int] should not be a (positiveSizeSet)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 3) should not be a (positiveSizeSet)
        }
        e.message should be (Some(s"${Set(1, 2, 3).toString()} was a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with array") {

      val positiveSizeArray = AMatcher[Array[Int]]("positive size array") { _.size > 0 }
      
      it("should work correctly with 'should be'") {
        Array(1, 2, 3) should be a positiveSizeArray
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Array.empty[Int] should be a positiveSizeArray
        }
        e.message should be (Some("Array() was not a positive size array"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Array.empty[Int] should not be a (positiveSizeArray)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 3) should not be a (positiveSizeArray)
        }
        e.message should be (Some("Array(1, 2, 3) was a positive size array"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }

    describe("when used with map") {

      val positiveSizeMap = AMatcher[Map[Int, String]]("positive size map") { _.size > 0 }
      
      it("should work correctly with 'should be'") {
        Map(1 -> "one", 2 -> "two", 3 -> "three") should be a positiveSizeMap
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Map.empty[Int, String] should be a positiveSizeMap
        }
        e.message should be (Some(s"${Map.empty[Int, String].toString()} was not a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Map.empty[Int, String] should not be a (positiveSizeMap)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", 3 -> "three") should not be a (positiveSizeMap)
        }
        e.message should be (Some(decorateToStringValue(prettifier, Map(1 -> "one", 2 -> "two", 3 -> "three")) + " was a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    describe("when used with java list") {

      val positiveSizeList = AMatcher[java.util.List[Int]]("positive size list") { _.size > 0 }
      
      def javaList(values: Int*): java.util.List[Int] = {
        val javaList = new java.util.ArrayList[Int]()
        values.foreach(javaList.add(_))
        javaList
      }
      
      it("should work correctly with 'should be'") {
        javaList(1, 2, 3) should be a positiveSizeList
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaList()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeList
        }
        e.message should be (Some(s"${left.toString()} was not a positive size list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaList() should not be a (positiveSizeList)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaList(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeList)
        }
        e.message should be (Some(s"${left.toString()} was a positive size list"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with java set") {

      val positiveSizeSet = AMatcher[java.util.Set[Int]]("positive size set") { _.size > 0 }
      
      def javaSet(values: Int*): java.util.Set[Int] = {
        val javaSet = new java.util.HashSet[Int]()
        values.foreach(javaSet.add(_))
        javaSet
      }
      
      it("should work correctly with 'should be'") {
        javaSet(1, 2, 3) should be a positiveSizeSet
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaSet()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeSet
        }
        e.message should be (Some(s"${left.toString()} was not a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaSet() should not be a (positiveSizeSet)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaSet(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeSet)
        }
        e.message should be (Some(s"${left.toString()} was a positive size set"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with java map") {
      
      def javaMap(values: (Int, String)*): java.util.Map[Int, String] = {
        val javaMap = new java.util.LinkedHashMap[Int, String]()
        values.foreach(e => javaMap.put(e._1, e._2))
        javaMap
      }

      val positiveSizeMap = AMatcher[java.util.Map[Int, String]]("positive size map") { _.size > 0 }
      
      it("should work correctly with 'should be'") {
        javaMap(1 -> "one", 2 -> "two", 3 -> "three") should be a positiveSizeMap
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaMap()
        val e = intercept[exceptions.TestFailedException] {
          left should be a positiveSizeMap
        }
        e.message should be (Some(s"${left.toString()} was not a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaMap() should not be a (positiveSizeMap)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
        val e = intercept[exceptions.TestFailedException] {
          left should not be a (positiveSizeMap)
        }
        e.message should be (Some(s"${decorateToStringValue(prettifier, left)} was a positive size map"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    
    describe("when used with custom object") {

      case class Person(name: String, age: Int)
      
      val youngMan = AMatcher[Person]("young man") { _.age < 60 }
      
      it("should work correctly with 'should be'") {
        Person("Tom", 30) should be a youngMan
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val tom = Person("Tom", 60)
        val tomPrettified = "Person(\"Tom\", 60)"
        val e = intercept[exceptions.TestFailedException] {
          tom should be a youngMan
        }
        e.message should be (Some(s"$tomPrettified was not a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Person("Tom", 60) should not be a (youngMan)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val tom = Person("Tom", 30)
        val tomPrettified = "Person(\"Tom\", 30)"
        val e = intercept[exceptions.TestFailedException] {
          tom should not be a (youngMan)
        }
        e.message should be (Some(s"$tomPrettified was a young man"))
        e.failedCodeFileName should be (Some("AMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
  }
}
