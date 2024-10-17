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

import org.scalactic.Equality
import org.scalactic.Explicitly
import org.scalactic.Prettifier
import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class InOrderElementsOfContainMatcherEqualitySpec extends AnyFunSpec with Explicitly {

  private val prettifier = Prettifier.default

  class CustomEquality extends Equality[String] {
    def areEqual(left: String, right: Any) =
      left.trim == (right match {
        case s: String => s.trim
        case other => other
      })
  }

  describe("inOrderElementsOf ") {

    implicit val equality = new CustomEquality

    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      val rightText = FailureMessages.decorateToStringValue(prettifier, right)
      e.message should be (Some(leftText + " did not contain all elements of " + rightText + " in order"))
      e.failedCodeFileName should be (Some("InOrderElementsOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      val rightText = FailureMessages.decorateToStringValue(prettifier, right)
      e.message should be (Some(leftText + " contained all elements of " + rightText + " in order"))
      e.failedCodeFileName should be (Some("InOrderElementsOfContainMatcherEqualitySpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should take custom implicit equality in scope when 'should contain' is used") {
      List("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")
      Array("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")
      // SKIP-SCALATESTJS,NATIVE-START
      javaList("1", "2 ", "3") should contain inOrderElementsOf Seq("1", "2 ", "3")
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take custom implicit equality in scope when 'should not contain' is used") {
      List("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))
      Array("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))
      // SKIP-SCALATESTJS,NATIVE-START
      javaList("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom implicit equality in scope") {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should contain inOrderElementsOf Seq("3", "2 ", "1")
      }
      checkShouldContainStackDepth(e1, left1, Seq("3", "2 ", "1"), thisLineNumber - 2)

      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should contain inOrderElementsOf Seq("3", "2 ", "1")
      }
      checkShouldContainStackDepth(e2, left2, Seq("3", "2 ", "1"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should contain inOrderElementsOf Seq("3", "2 ", "1")
      }
      checkShouldContainStackDepth(e3, left3, Seq("3", "2 ", "1"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom implicit equality in scope") {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        left1 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))
      }
      checkShouldNotContainStackDepth(e1, left1, Seq("1", "2 ", "3"), thisLineNumber - 2)

      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        left2 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))
      }
      checkShouldNotContainStackDepth(e2, left2, Seq("1", "2 ", "3"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        left3 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))
      }
      checkShouldNotContainStackDepth(e3, left3, Seq("1", "2 ", "3"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take passed in custom explicit equality when 'should contain' is used") {
      (List("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")) (equality)
      (Array("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")) (equality)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")) (equality)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take passed in custom explicit equality when 'should not contain' is used") {
      (List("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))) (equality)
      (Array("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))) (equality)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("3", "2 ", "1"))) (equality)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with custom explicit equality") {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderElementsOf Seq("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e1, left1, Seq("3", "2 ", "1"), thisLineNumber - 2)

      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderElementsOf Seq("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e2, left2, Seq("3", "2 ", "1"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderElementsOf Seq("3", "2 ", "1")) (equality)
      }
      checkShouldContainStackDepth(e3, left3, Seq("3", "2 ", "1"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with custom explicit equality") {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (equality)
      }
      checkShouldNotContainStackDepth(e1, left1, Seq("1", "2 ", "3"), thisLineNumber - 2)

      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (equality)
      }
      checkShouldNotContainStackDepth(e2, left2, Seq("1", "2 ", "3"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (equality)
      }
      checkShouldNotContainStackDepth(e3, left3, Seq("1", "2 ", "3"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
