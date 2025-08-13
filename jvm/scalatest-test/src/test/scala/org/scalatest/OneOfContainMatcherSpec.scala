/*
 * Copyright 2001-2025 Artima, Inc.
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

import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._
import org.scalactic.Prettifier

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OneOfContainMatcherSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("oneOf ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      e.message should be (Some(FailureMessages.didNotContainOneOfElements(prettifier, left, UnquotedString(right.map(prettifier.apply).mkString(", ")))))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeeded when left List contains same elements in same order as right List") {
      List(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      Array(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      Set(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      Map(1 -> "one", 3 -> "three", 5 -> "five", 8 -> "eight") should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      javaSet(1, 3, 5, 8) should contain oneOf (7, 8, 9)
      javaMap(Entry(1, "one"), Entry(3, "three"), Entry(5, "five"), Entry(8, "eight")) should contain oneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should succeeded when right List contains at least one element in right List") {
      List(1, 2, 3) should contain oneOf (5, 3, 8)
      Array(1, 2, 3) should contain oneOf (5, 3, 8)
      Set(1, 2, 3) should contain oneOf (5, 3, 8)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain oneOf (5 -> "five", 3 -> "three", 8 -> "eight")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain oneOf (5, 3, 8)
      javaSet(1, 2, 3) should contain oneOf (5, 3, 8)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain oneOf (Entry(5, "five"), Entry(3, "three"), Entry(8, "eight"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should succeeded when used with atLeastOneOf and right List contains more than one element in right List") {
      List(1, 2, 3) should contain atLeastOneOf (5, 3, 2)
      Array(1, 2, 3) should contain atLeastOneOf (5, 3, 2)
      Set(1, 2, 3) should contain atLeastOneOf (5, 3, 2)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain atLeastOneOf (5 -> "five", 3 -> "three", 2 -> "two")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain atLeastOneOf (5, 3, 2)
      javaSet(1, 2, 3) should contain atLeastOneOf (5, 3, 2)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain atLeastOneOf (Entry(5, "five"), Entry(3, "three"), Entry(2, "two"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should succeeded when used with atLeastOneOf and right List contains all elements in left List in different order") {
      List(1, 2, 3) should contain atLeastOneOf (1, 3, 2)
      Array(1, 2, 3) should contain atLeastOneOf (1, 3, 2)
      Set(1, 2, 3) should contain atLeastOneOf (1, 3, 2)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain atLeastOneOf (1 -> "one", 3 -> "three", 2 -> "twp")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain atLeastOneOf (1, 3, 2)
      javaSet(1, 2, 3) should contain atLeastOneOf (1, 3, 2)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain atLeastOneOf (Entry(1, "one"), Entry(3, "three"), Entry(2, "twp"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should succeeded when used with atLeastOneOf and right List contains all elements in left List in same order") {
      List(1, 2, 3) should contain atLeastOneOf (1, 2, 3)
      Array(1, 2, 3) should contain atLeastOneOf (1, 2, 3)
      Set(1, 2, 3) should contain atLeastOneOf (1, 2, 3)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain atLeastOneOf (1 -> "one", 2 -> "two", 3 -> "three")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain atLeastOneOf (1, 2, 3)
      javaSet(1, 2, 3) should contain atLeastOneOf (1, 2, 3)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain atLeastOneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw NotAllowedException when oneOf contains duplicate element") {
      val e1 = intercept[exceptions.NotAllowedException] {
        List(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e1.getMessage() should be (FailureMessages.oneOfDuplicate)
      
      val e2 = intercept[exceptions.NotAllowedException] {
        Set(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e2.getMessage() should be (FailureMessages.oneOfDuplicate)
      
      val e3 = intercept[exceptions.NotAllowedException] {
        Array(1, 2, 3) should contain oneOf(6, 7, 6)
      }
      e3.getMessage() should be (FailureMessages.oneOfDuplicate)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List are same size but does not contain any same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e1, left1, deep(Array(7, 8, 9)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e2, left2, deep(Array(7, 8, 9)), thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e3, left3, Array(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (7, 8, 9)
      }
      checkStackDepth(e4, left4, deep(Array(7, 8, 9)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
      }
      checkStackDepth(e5, left5, Array(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List is shorter than right List and does not contain any same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e1, left1, deep(Array(6, 7, 8, 9)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e2, left2, deep(Array(6, 7, 8, 9)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine")
      }
      checkStackDepth(e3, left3, Array(6 -> "six", 7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (6, 7, 8, 9)
      }
      checkStackDepth(e4, left4, deep(Array(6, 7, 8, 9)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
      }
      checkStackDepth(e5, left5, Array(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List is longer than right List and does not contain any same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (8, 5)
      }
      checkStackDepth(e1, left1, deep(Array(8, 5)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (8, 5)
      }
      checkStackDepth(e2, left2, deep(Array(8, 5)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (8 -> "eight", 5 -> "five")
      }
      checkStackDepth(e3, left3, Array(8 -> "eight", 5 -> "five"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (8, 5)
      }
      checkStackDepth(e4, left4, deep(Array(8, 5)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(8, "eight"), Entry(5, "five"))
      }
      checkStackDepth(e5, left5, Array(Entry(8, "eight"), Entry(5, "five")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List contain all same element in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (3, 2, 1)
      }
      checkStackDepth(e1, left1, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (3, 2, 1)
      }
      checkStackDepth(e2, left2, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e3, left3, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (3, 2, 1)
      }
      checkStackDepth(e4, left4, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(3, "three"), Entry(2, "two"), Entry(1, "one"))
      }
      checkStackDepth(e5, left5, Array(Entry(3, "three"), Entry(2, "two"), Entry(1, "one")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List contain all same element in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (1, 2, 3)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (1, 2, 3)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (1, 2, 3)
      }
      checkStackDepth(e4, left4, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      }
      checkStackDepth(e5, left5, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left and right List contain more than one same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain oneOf (5, 1, 2)
      }
      checkStackDepth(e1, left1, deep(Array(5, 1, 2)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain oneOf (5, 1, 2)
      }
      checkStackDepth(e2, left2, deep(Array(5, 1, 2)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain oneOf (5 -> "five", 1 -> "one", 2 -> "two")
      }
      checkStackDepth(e3, left3, Array(5 -> "five", 1 -> "one", 2 -> "two"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain oneOf (5, 1, 2)
      }
      checkStackDepth(e4, left4, deep(Array(5, 1, 2)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain oneOf (Entry(5, "five"), Entry(1, "one"), Entry(2, "two"))
      }
      checkStackDepth(e5, left5, Array(Entry(5, "five"), Entry(1, "one"), Entry(2, "two")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
  
  describe("not oneOf ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(FailureMessages.containedOneOfElements(prettifier, left, UnquotedString(right.map(e => FailureMessages.decorateToStringValue(prettifier, e)).mkString(", ")))))
      e.failedCodeFileName should be (Some("OneOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeed when left List contains different elements as right List") {
      List(1, 2, 3) should not contain oneOf (7, 8, 9)
      Array(1, 2, 3) should not contain oneOf (7, 8, 9)
      Set(1, 2, 3) should not contain oneOf (7, 8, 9)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (7 -> "seven", 8 -> "eight", 9 -> "nine")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain oneOf (7, 8, 9)
      javaSet(1, 2, 3) should not contain oneOf (7, 8, 9)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain oneOf (Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left and right List contain at least one same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e1, left1, deep(Array(5, 1, 7)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e2, left2, deep(Array(5, 1, 7)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain oneOf (5 -> "five", 1 -> "one", 7 -> "seven")
      }
      checkStackDepth(e3, left3, Array(5 -> "five", 1 -> "one", 7 -> "seven"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain oneOf (5, 1, 7)
      }
      checkStackDepth(e4, left4, deep(Array(5, 1, 7)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain oneOf (Entry(5, "five"), Entry(1, "one"), Entry(7, "seven"))
      }
      checkStackDepth(e5, left5, Array(Entry(5, "five"), Entry(1, "one"), Entry(7, "seven")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    ignore("should throw TestFailedException from atLeastOneOf with correct stack depth and message when left and right List contain more than one same element") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain atLeastOneOf (5, 1, 2)
      }
      checkStackDepth(e1, left1, deep(Array(5, 1, 2)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain atLeastOneOf (5, 1, 2)
      }
      checkStackDepth(e2, left2, deep(Array(5, 1, 2)), thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain atLeastOneOf (5 -> "five", 1 -> "one", 2 -> "two")
      }
      checkStackDepth(e3, left3, Array(5 -> "five", 1 -> "one", 2 -> "two"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain atLeastOneOf (5, 1, 2)
      }
      checkStackDepth(e4, left4, deep(Array(5, 1, 2)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain atLeastOneOf (Entry(5, "five"), Entry(1, "one"), Entry(2, "two"))
      }
      checkStackDepth(e5, left5, Array(Entry(5, "five"), Entry(1, "one"), Entry(2, "two")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    ignore("should throw TestFailedException from atLeastOneOf with correct stack depth and message when left and right List contain all same element in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain atLeastOneOf (3, 2, 1)
      }
      checkStackDepth(e1, left1, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain atLeastOneOf (3, 2, 1)
      }
      checkStackDepth(e2, left2, deep(Array(3, 2, 1)), thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain atLeastOneOf (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e3, left3, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain atLeastOneOf (3, 2, 1)
      }
      checkStackDepth(e4, left4, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain atLeastOneOf (Entry(3, "three"), Entry(2, "two"), Entry(1, "one"))
      }
      checkStackDepth(e5, left5, Array(Entry(3, "three"), Entry(2, "two"), Entry(1, "one")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    ignore("should throw TestFailedException from atLeastOneOf with correct stack depth and message when left and right List contain all same element in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain atLeastOneOf (1, 2, 3)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain atLeastOneOf (1, 2, 3)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain atLeastOneOf (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain atLeastOneOf (1, 2, 3)
      }
      checkStackDepth(e4, left4, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain atLeastOneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      }
      checkStackDepth(e5, left5, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should succeeded when right List contains all elements in left List in different order") {
      List(1, 2, 3) should not contain oneOf (1, 3, 2)
      Array(1, 2, 3) should not contain oneOf (1, 3, 2)
      Set(1, 2, 3) should not contain oneOf (1, 3, 2)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 3 -> "three", 2 -> "twp")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain oneOf (1, 3, 2)
      javaSet(1, 2, 3) should not contain oneOf (1, 3, 2)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain oneOf (Entry(1, "one"), Entry(3, "three"), Entry(2, "twp"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should succeeded when right List contains all elements in left List in same order") {
      List(1, 2, 3) should not contain oneOf (1, 2, 3)
      Array(1, 2, 3) should not contain oneOf (1, 2, 3)
      Set(1, 2, 3) should not contain oneOf (1, 2, 3)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (1 -> "one", 2 -> "two", 3 -> "three")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain oneOf (1, 2, 3)
      javaSet(1, 2, 3) should not contain oneOf (1, 2, 3)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain oneOf (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should succeeded when right List contains more than one element in right List") {
      List(1, 2, 3) should not contain oneOf (5, 3, 2)
      Array(1, 2, 3) should not contain oneOf (5, 3, 2)
      Set(1, 2, 3) should not contain oneOf (5, 3, 2)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain oneOf (5 -> "five", 3 -> "three", 2 -> "two")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain oneOf (5, 3, 2)
      javaSet(1, 2, 3) should not contain oneOf (5, 3, 2)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain oneOf (Entry(5, "five"), Entry(3, "three"), Entry(2, "two"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
