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

import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._
import org.scalatest.exceptions.TestFailedException
import org.scalactic.Prettifier

import org.scalactic.ArrayHelper.deep
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class OnlyContainMatcherSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  describe("only ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " did not contain only (" + right.map(e => FailureMessages.decorateToStringValue(prettifier, e)).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeed when left List contains elements available in right List") {
      List(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      Array(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      Set(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain only (1 -> "one", 2 -> "two", 3 -> "three")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      javaSet(1, 2, 2, 3, 3, 3) should contain only (1, 2, 3)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain only (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left list contains part of right list") {
      val left1 = List(1, 2, 2, 3, 3, 3)
      val e1 = intercept[TestFailedException] {
        left1 should contain only (1, 2, 3, 4, 5)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3, 4, 5)), thisLineNumber - 2)
      
      val left2 = Array(1, 2, 2, 3, 3, 3)
      val e2 = intercept[TestFailedException] {
        left2 should contain only (1, 2, 3, 4, 5)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3, 4, 5)), thisLineNumber - 2)
      
      val left3 = Set(1, 2, 2, 3, 3, 3)
      val e3 = intercept[TestFailedException] {
        left3 should contain only (1, 2, 3, 4, 5)
      }
      checkStackDepth(e3, left3, deep(Array(1, 2, 3, 4, 5)), thisLineNumber - 2)
      
      val left4 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e4 = intercept[TestFailedException] {
        left4 should contain only (1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")
      }
      checkStackDepth(e4, left4, deep(Array(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five")), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left5 = javaList(1, 2, 2, 3, 3, 3)
      val e5 = intercept[TestFailedException] {
        left5 should contain only (1, 2, 3, 4, 5)
      }
      checkStackDepth(e5, left5, deep(Array(1, 2, 3, 4, 5)), thisLineNumber - 2)

      val left6 = javaSet(1, 2, 2, 3, 3, 3)
      val e6 = intercept[TestFailedException] {
        left6 should contain only (1, 2, 3, 4, 5)
      }
      checkStackDepth(e6, left6, deep(Array(1, 2, 3, 4, 5)), thisLineNumber - 2)

      val left7 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e7 = intercept[TestFailedException] {
        left7 should contain only (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"), Entry(5, "five"))
      }
      checkStackDepth(e7, left7, deep(Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"), Entry(5, "five"))), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw NotAllowedException when only contains duplicate element") {
      val e1 = intercept[exceptions.NotAllowedException] {
        List(1, 2, 3) should contain only (1, 2, 1)
      }
      e1.getMessage() should be (FailureMessages.onlyDuplicate)
      
      val e2 = intercept[exceptions.NotAllowedException] {
        Set(1, 2, 3) should contain only (1, 2, 1)
      }
      e2.getMessage() should be (FailureMessages.onlyDuplicate)
      
      val e3 = intercept[exceptions.NotAllowedException] {
        Array(1, 2, 3) should contain only (1, 2, 1)
      }
      e3.getMessage() should be (FailureMessages.onlyDuplicate)
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List contains element not in right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain only (1, 2)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain only (1, 2)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain only (1 -> "one", 2 -> "two")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain only (1, 2)
      }
      checkStackDepth(e4, left4, deep(Array(1, 2)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain only (Entry(1, "one"), Entry(2, "two"))
      }
      checkStackDepth(e5, left5, Array(Entry(1, "one"), Entry(2, "two")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
  
  describe("not only ") {
    
    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      e.message should be (Some(leftText + " contained only (" + right.map(e => FailureMessages.decorateToStringValue(prettifier, e)).mkString(", ") + ")"))
      e.failedCodeFileName should be (Some("OnlyContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }
    
    it("should succeed when left List contains element not in right List") {
      List(1, 2, 3) should not contain only (1, 2)
      Array(1, 2, 3) should not contain only (1, 2)
      Set(1, 2, 3) should not contain only (1, 2)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain only (1 -> "one", 2 -> "two")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain only (1, 2)
      javaSet(1, 2, 3) should not contain only (1, 2)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain only (Entry(1, "one"), Entry(2, "two"))
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain only (1, 2, 3)
      }
      checkStackDepth(e1, left1, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain only (1, 2, 3)
      }
      checkStackDepth(e2, left2, deep(Array(1, 2, 3)), thisLineNumber - 2)
      
      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain only (1 -> "one", 2 -> "two", 3 -> "three")
      }
      checkStackDepth(e3, left3, Array(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain only (1, 2, 3)
      }
      checkStackDepth(e4, left4, deep(Array(1, 2, 3)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain only (Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      }
      checkStackDepth(e5, left5, Array(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
    
    it("should throw TestFailedException with correct stack depth and message when left List contains only element in right List in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain only (3, 2, 1)
      }
      checkStackDepth(e1, left1, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left2 = Array(1, 2, 3)
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain only (3, 2, 1)
      }
      checkStackDepth(e2, left2, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left3 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain only (3 -> "three", 2 -> "two", 1 -> "one")
      }
      checkStackDepth(e3, left3, Array(3 -> "three", 2 -> "two", 1 -> "one"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain only (3, 2, 1)
      }
      checkStackDepth(e4, left4, deep(Array(3, 2, 1)), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain only (Entry(3, "three"), Entry(2, "two"), Entry(1, "one"))
      }
      checkStackDepth(e5, left5, Array(Entry(3, "three"), Entry(2, "two"), Entry(1, "one")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
