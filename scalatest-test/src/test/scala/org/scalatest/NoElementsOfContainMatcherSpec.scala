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
package org.scalatest

import collection.GenTraversable
import SharedHelpers._
import Matchers._

class NoElementsOfContainMatcherSpec extends FunSpec {

  describe("noElementsOf ") {

    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      e.message should be (Some(FailureMessages.containedAtLeastOneOf(left, right)))
      e.failedCodeFileName should be (Some("NoElementsOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should succeed when left List contains elements available in right List") {
      List(1, 2, 3, 4, 5) should contain noElementsOf Seq(6, 7, 8)
      Array(1, 2, 3, 4, 5) should contain noElementsOf Seq(6, 7, 8)
      Set(1, 2, 3, 4, 5) should contain noElementsOf Seq(6, 7, 8)
      Map(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four", 5 -> "five") should contain noElementsOf Seq(6 -> "six", 7 -> "seven", 8 -> "eight")

      // SKIP-SCALATESTJS-START
      javaList(1, 2, 3, 4, 5) should contain noElementsOf Seq(6, 7, 8)
      javaSet(1, 2, 3, 4, 5) should contain noElementsOf Seq(6, 7, 8)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"), Entry(5, "five")) should contain noElementsOf Seq(Entry(6, "six"), Entry(7, "seven"), Entry(8, "eight"))
      // SKIP-SCALATESTJS-END
    }

    it("should succeed when left list contains none of right list") {
      List(1, 2, 3) should contain noElementsOf Seq(7, 8)
      Array(1, 2, 3) should contain noElementsOf Seq(7, 8)
      Set(1, 2, 3) should contain noElementsOf Seq(7, 8)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain noElementsOf Seq(7 -> "seven", 8 -> "eight")

      // SKIP-SCALATESTJS-START
      javaList(1, 2, 3) should contain noElementsOf Seq(7, 8)
      javaSet(1, 2, 3) should contain noElementsOf Seq(7, 8)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain noElementsOf Seq(Entry(7, "seven"), Entry(8, "eight"))
      // SKIP-SCALATESTJS-END
    }

    it("should allow Seq passed to noElementsOf to contains duplicate element") {
      List(1, 2, 3) should contain noElementsOf Seq(6, 8, 6)
      Set(1, 2, 3) should contain noElementsOf Seq(6, 8, 6)
      Array(1, 2, 3) should contain noElementsOf Seq(6, 8, 6)
    }

    it("should throw TestFailedException with correct stack depth and message when left List contains element in right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain noElementsOf Seq(0, 3, 8)
      }
      checkStackDepth(e1, left1, Seq(0, 3, 8), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain noElementsOf Seq(0 -> "zero", 3 -> "three", 8 -> "eight")
      }
      checkStackDepth(e2, left2, Seq(0 -> "zero", 3 -> "three", 8 -> "eight"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain noElementsOf Seq(0, 3, 8)
      }
      checkStackDepth(e3, left3, Seq(0, 3, 8), thisLineNumber - 2)

      // SKIP-SCALATESTJS-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain noElementsOf Seq(0, 3, 8)
      }
      checkStackDepth(e4, left4, Seq(0, 3, 8), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain noElementsOf Seq(Entry(0, "zero"), Entry(3, "three"), Entry(8, "eight"))
      }
      checkStackDepth(e5, left5, Seq(Entry(0, "zero"), Entry(3, "three"), Entry(8, "eight")), thisLineNumber - 2)
      // SKIP-SCALATESTJS-END
    }
  }

  describe("not noElementsOf ") {

    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: GenTraversable[Any], lineNumber: Int) {
      val leftText = FailureMessages.decorateToStringValue(left)
      e.message should be (Some(FailureMessages.didNotContainAtLeastOneOf(left, right)))
      e.failedCodeFileName should be (Some("NoElementsOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should succeed when left List contains element in right List") {
      List(1, 2, 3) should not contain noElementsOf (Seq(0, 2, 8))
      Array(1, 2, 3) should not contain noElementsOf (Seq(0, 2, 8))
      Set(1, 2, 3) should not contain noElementsOf (Seq(0, 2, 8))
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain noElementsOf (Seq(0 -> "zero", 2 -> "two", 8 -> "eight"))

      // SKIP-SCALATESTJS-START
      javaList(1, 2, 3) should not contain noElementsOf (Seq(0, 2, 8))
      javaSet(1, 2, 3) should not contain noElementsOf (Seq(0, 2, 8))
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain noElementsOf (Seq(Entry(0, "zero"), Entry(2, "two"), Entry(8, "eight")))
      // SKIP-SCALATESTJS-END
    }

    it("should throw TestFailedException with correct stack depth and message when left List contains only element in right List in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain noElementsOf (Seq(7, 8, 9))
      }
      checkStackDepth(e1, left1, Seq(7, 8, 9), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain noElementsOf (Seq(7 -> "seven", 8 -> "eight", 9 -> "nine"))
      }
      checkStackDepth(e2, left2, Seq(7 -> "seven", 8 -> "eight", 9 -> "nine"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain noElementsOf (Seq(7, 8, 9))
      }
      checkStackDepth(e3, left3, Seq(7, 8, 9), thisLineNumber - 2)

      // SKIP-SCALATESTJS-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain noElementsOf (Seq(7, 8, 9))
      }
      checkStackDepth(e4, left4, Seq(7, 8, 9), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain noElementsOf (Seq(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")))
      }
      checkStackDepth(e5, left5, Seq(Entry(7, "seven"), Entry(8, "eight"), Entry(9, "nine")), thisLineNumber - 2)
      // SKIP-SCALATESTJS-END
    }
  }
}
