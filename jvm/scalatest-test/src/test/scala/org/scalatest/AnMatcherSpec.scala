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

import SharedHelpers._
import matchers.should.Matchers._
import matchers.AnMatcher
import FailureMessages.decorateToStringValue
import org.scalactic.Prettifier

class AnMatcherSpec extends funspec.AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("AnMatcher ") {
    
    describe("when used with integer") {
      
      val oddNumber = AnMatcher[Int]("odd number") { _ % 2 != 0 }
      
      it("should work correctly with 'should be a'") {
        1 should be an oddNumber
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          0 should be an oddNumber
        }
        e.message should be (Some("0 was not an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be a'") {
        0 should not be an (oddNumber)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          1 should not be an (oddNumber)
        }
        e.message should be (Some("1 was an odd number"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
    }
    
    describe("when used with string") {
      
      val oddLengthString = AnMatcher[String]("odd length string") { _.length % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        "hello" should be an oddLengthString
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          "" should be an oddLengthString
        }
        e.message should be (Some("\"\" was not an odd length string"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        "" should not be an (oddLengthString)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          "hai" should not be an (oddLengthString)
        }
        e.message should be (Some("\"hai\" was an odd length string"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with list") {

      val oddLengthList = AnMatcher[List[Int]]("odd length list") { _.length % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        List(1, 2, 3) should be an oddLengthList
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          List.empty[Int] should be an oddLengthList
        }
        e.message should be (Some(s"${List.empty[Int].toString()} was not an odd length list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        List.empty[Int] should not be an (oddLengthList)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          List(1, 2, 3) should not be an (oddLengthList)
        }
        e.message should be (Some(s"${List(1, 2, 3).toString()} was an odd length list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with set") {

      val oddSizeSet = AnMatcher[Set[Int]]("odd size set") { _.size % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        Set(1, 2, 3) should be an oddSizeSet
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Set.empty[Int] should be an oddSizeSet
        }
        e.message should be (Some(s"${Set.empty[Int].toString()} was not an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Set.empty[Int] should not be an (oddSizeSet)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Set(1, 2, 3) should not be an (oddSizeSet)
        }
        e.message should be (Some(s"${Set(1, 2, 3).toString()} was an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with array") {

      val oddSizeArray = AnMatcher[Array[Int]]("odd size array") { _.size % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        Array(1, 2, 3) should be an oddSizeArray
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Array.empty[Int] should be an oddSizeArray
        }
        e.message should be (Some("Array() was not an odd size array"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Array.empty[Int] should not be an (oddSizeArray)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Array(1, 2, 3) should not be an (oddSizeArray)
        }
        e.message should be (Some("Array(1, 2, 3) was an odd size array"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    
    describe("when used with map") {

      val oddSizeMap = AnMatcher[Map[Int, String]]("odd size map") { _.size % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        Map(1 -> "one", 2 -> "two", 3 -> "three") should be an oddSizeMap
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Map.empty[Int, String] should be an oddSizeMap
        }
        e.message should be (Some(s"${Map.empty[Int, String].toString()} was not an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Map.empty[Int, String] should not be an (oddSizeMap)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val e = intercept[exceptions.TestFailedException] {
          Map(1 -> "one", 2 -> "two", 3 -> "three") should not be an (oddSizeMap)
        }
        e.message should be (Some(decorateToStringValue(prettifier, Map(1 -> "one", 2 -> "two", 3 -> "three")) + " was an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }

    // SKIP-SCALATESTJS,NATIVE-START
    describe("when used with java list") {

      val oddSizeList = AnMatcher[java.util.List[Int]]("odd size list") { _.size % 2 != 0 }
      
      def javaList(values: Int*): java.util.List[Int] = {
        val javaList = new java.util.ArrayList[Int]()
        values.foreach(javaList.add(_))
        javaList
      }
      
      it("should work correctly with 'should be'") {
        javaList(1, 2, 3) should be an oddSizeList
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaList()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeList
        }
        e.message should be (Some(s"${left.toString()} was not an odd size list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaList() should not be an (oddSizeList)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaList(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeList)
        }
        e.message should be (Some(s"${left.toString()} was an odd size list"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with java set") {

      val oddSizeSet = AnMatcher[java.util.Set[Int]]("odd size set") { _.size % 2 != 0 }
      
      def javaSet(values: Int*): java.util.Set[Int] = {
        val javaSet = new java.util.HashSet[Int]()
        values.foreach(javaSet.add(_))
        javaSet
      }
      
      it("should work correctly with 'should be'") {
        javaSet(1, 2, 3) should be an oddSizeSet
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaSet()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeSet
        }
        e.message should be (Some(s"${left.toString()} was not an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaSet() should not be an (oddSizeSet)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaSet(1, 2, 3)
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeSet)
        }
        e.message should be (Some(s"${left.toString()} was an odd size set"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    
    describe("when used with java map") {
      
      def javaMap(values: (Int, String)*): java.util.Map[Int, String] = {
        val javaMap = new java.util.LinkedHashMap[Int, String]()
        values.foreach(e => javaMap.put(e._1, e._2))
        javaMap
      }

      val oddSizeMap = AnMatcher[java.util.Map[Int, String]]("odd size map") { _.size  % 2 != 0 }
      
      it("should work correctly with 'should be'") {
        javaMap(1 -> "one", 2 -> "two", 3 -> "three") should be an oddSizeMap
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val left = javaMap()
        val e = intercept[exceptions.TestFailedException] {
          left should be an oddSizeMap
        }
        e.message should be (Some(s"${left.toString()} was not an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        javaMap() should not be an (oddSizeMap)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val left = javaMap(1 -> "one", 2 -> "two", 3 -> "three")
        val e = intercept[exceptions.TestFailedException] {
          left should not be an (oddSizeMap)
        }
        e.message should be (Some(s"${decorateToStringValue(prettifier, left)} was an odd size map"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
    // SKIP-SCALATESTJS,NATIVE-END
    
    describe("when used with custom object") {

      case class Person(name: String, age: Int)
      
      val oldMan = AnMatcher[Person]("old man") { _.age > 59 }
      
      it("should work correctly with 'should be'") {
        Person("Tom", 60) should be an oldMan
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should be a' assertion failed") {
        val tom = Person("Tom", 30)
        val tomPrettified = "Person(\"Tom\", 30)"
        val e = intercept[exceptions.TestFailedException] {
          tom should be an oldMan
        }
        e.message should be (Some(s"$tomPrettified was not an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
      
      it("should work correctly with 'should not be'") {
        Person("Tom", 30) should not be an (oldMan)
      }
      
      it("should throw TestFailedException with correct stack depth and message when 'should not be a' assertion failed") {
        val tom = Person("Tom", 60)
        val tomPrettified = "Person(\"Tom\", 60)"
        val e = intercept[exceptions.TestFailedException] {
          tom should not be an (oldMan)
        }
        e.message should be (Some(s"$tomPrettified was an old man"))
        e.failedCodeFileName should be (Some("AnMatcherSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 4))
      }
    }
  }
}