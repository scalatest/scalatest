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

import org.scalactic.{Prettifier}

import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._
import matchers.should.Matchers._

class AllElementsOfContainMatcherSpec extends funspec.AnyFunSpec {

  describe("allElementsOf ") {

    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = Prettifier.default(left)
      val rightText = Prettifier.default(right)
      e.message should be (Some(leftText + " did not contain all elements of " + rightText))
      e.failedCodeFileName should be (Some("AllElementsOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should succeeded when left List contains same elements in same order as right List") {
      List(5, 4, 3, 2, 1) should contain allElementsOf Seq(1, 3, 5)
      Array(5, 4, 3, 2, 1) should contain allElementsOf Seq(1, 3, 5)
      Map(5 -> "five", 4 -> "four", 3 -> "three", 2 -> "two", 1 -> "one") should contain allElementsOf Seq(1 -> "one", 3 -> "three", 5 -> "five")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(5, 4, 3, 2, 1) should contain allElementsOf Seq(1, 3, 5)
      javaMap(Entry(5, "five"), Entry(4, "four"), Entry(3, "three"), Entry(2, "two"), Entry(1, "one")) should contain allElementsOf Seq(Entry(1, "one"), Entry(3, "three"), Entry(5, "five"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should succeeded when left List contains same elements in different order as right List") {
      List(1, 2, 3) should contain allElementsOf Seq(2, 1, 3)
      Array(1, 2, 3) should contain allElementsOf Seq(2, 1, 3)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allElementsOf Seq(2 -> "two", 1 -> "one", 3 -> "three")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain allElementsOf Seq(2, 1, 3)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain allElementsOf Seq(Entry(2, "two"), Entry(1, "one"), Entry(3, "three"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should succeeded when left List contains same elements in same order as right Set") {
      List(1, 2, 3) should contain allElementsOf Seq(1, 2, 3)
      Array(1, 2, 3) should contain allElementsOf Seq(1, 2, 3)
      Map(1 -> "one", 2 -> "two", 3 -> "three") should contain allElementsOf Seq(1 -> "one", 2 -> "two", 3 -> "three")

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain allElementsOf Seq(1, 2, 3)
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should contain allElementsOf Seq(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should allow duplicate element") {
      List(1, 2, 3) should contain allElementsOf Seq(1, 2, 1)
      Array(1, 2, 3) should contain allElementsOf Seq(1, 2, 1)

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should contain allElementsOf Seq(1, 2, 1)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left and right List are same size but contain different elements") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allElementsOf Seq(2, 5, 3)
      }
      checkStackDepth(e1, left1, Seq(2, 5, 3), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allElementsOf Seq(2 -> "two", 5 -> "five", 3 -> "three")
      }
      checkStackDepth(e2, left2, Seq(2 -> "two", 5 -> "five", 3 -> "three"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allElementsOf Seq(2, 5, 3)
      }
      checkStackDepth(e3, left3, Seq(2, 5, 3), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allElementsOf Seq(2, 5, 3)
      }
      checkStackDepth(e4, left4, Seq(2, 5, 3), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allElementsOf Seq(Entry(2, "two"), Entry(5, "five"), Entry(3, "three"))
      }
      checkStackDepth(e5, left5, Seq(Entry(2, "two"), Entry(5, "five"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left List is shorter than right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allElementsOf Seq(1, 2, 3, 4)
      }
      checkStackDepth(e1, left1, Seq(1, 2, 3, 4), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allElementsOf Seq(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four")
      }
      checkStackDepth(e2, left2, Seq(1 -> "one", 2 -> "two", 3 -> "three", 4 -> "four"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allElementsOf Seq(1, 2, 3, 4)
      }
      checkStackDepth(e3, left3, Seq(1, 2, 3, 4), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allElementsOf Seq(1, 2, 3, 4)
      }
      checkStackDepth(e4, left4, Seq(1, 2, 3, 4), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allElementsOf Seq(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four"))
      }
      checkStackDepth(e5, left5, Seq(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"), Entry(4, "four")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left List is longer than right List") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain allElementsOf Seq(1, 5)
      }
      checkStackDepth(e1, left1, Seq(1, 5), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain allElementsOf Seq(1 -> "one", 5 -> "five")
      }
      checkStackDepth(e2, left2, Seq(1 -> "one", 5 -> "five"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain allElementsOf Seq(1, 5)
      }
      checkStackDepth(e3, left3, Seq(1, 5), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should contain allElementsOf Seq(1, 5)
      }
      checkStackDepth(e4, left4, Seq(1, 5), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should contain allElementsOf Seq(Entry(1, "one"), Entry(5, "five"))
      }
      checkStackDepth(e5, left5, Seq(Entry(1, "one"), Entry(5, "five")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }

  describe("not allElementsOf ") {

    def checkStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = Prettifier.default(left)
      val rightText = Prettifier.default(right)
      e.message should be (Some(leftText + " contained all elements of " + rightText))
      e.failedCodeFileName should be (Some("AllElementsOfContainMatcherSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should succeeded when left List contains different elements as right List") {
      List(1, 2, 3) should not contain allElementsOf (Seq(1, 2, 8))
      Array(1, 2, 3) should not contain allElementsOf (Seq(1, 2, 8))
      Map(1 -> "one", 2 -> "two", 3 -> "three") should not contain allElementsOf (Seq(1 -> "one", 2 -> "two", 8 -> "eight"))

      // SKIP-SCALATESTJS,NATIVE-START
      javaList(1, 2, 3) should not contain allElementsOf (Seq(1, 2, 8))
      javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")) should not contain allElementsOf (Seq(Entry(1, "one"), Entry(2, "two"), Entry(8, "eight")))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left and right List contain same elements in different order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allElementsOf (Seq(2, 1, 3))
      }
      checkStackDepth(e1, left1, Seq(2, 1, 3), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allElementsOf (Seq(2 -> "two", 1 -> "one", 3 -> "three"))
      }
      checkStackDepth(e2, left2, Seq(2 -> "two", 1 -> "one", 3 -> "three"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allElementsOf (Seq(2, 1, 3))
      }
      checkStackDepth(e3, left3, Seq(2, 1, 3), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allElementsOf (Seq(2, 1, 3))
      }
      checkStackDepth(e4, left4, Seq(2, 1, 3), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allElementsOf (Seq(Entry(2, "two"), Entry(1, "one"), Entry(3, "three")))
      }
      checkStackDepth(e5, left5, Seq(Entry(2, "two"), Entry(1, "one"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when left and right List contain same elements in same order") {
      val left1 = List(1, 2, 3)
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain allElementsOf (Seq(1, 2, 3))
      }
      checkStackDepth(e1, left1, Seq(1, 2, 3), thisLineNumber - 2)

      val left2 = Map(1 -> "one", 2 -> "two", 3 -> "three")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain allElementsOf (Seq(1 -> "one", 2 -> "two", 3 -> "three"))
      }
      checkStackDepth(e2, left2, Seq(1 -> "one", 2 -> "two", 3 -> "three"), thisLineNumber - 2)

      val left3 = Array(1, 2, 3)
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain allElementsOf (Seq(1, 2, 3))
      }
      checkStackDepth(e3, left3, Seq(1, 2, 3), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left4 = javaList(1, 2, 3)
      val e4 = intercept[exceptions.TestFailedException] {
        left4 should not contain allElementsOf (Seq(1, 2, 3))
      }
      checkStackDepth(e4, left4, Seq(1, 2, 3), thisLineNumber - 2)

      val left5 = javaMap(Entry(1, "one"), Entry(2, "two"), Entry(3, "three"))
      val e5 = intercept[exceptions.TestFailedException] {
        left5 should not contain allElementsOf (Seq(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")))
      }
      checkStackDepth(e5, left5, Seq(Entry(1, "one"), Entry(2, "two"), Entry(3, "three")), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}