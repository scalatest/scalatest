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

import org.scalactic.Equality
import org.scalactic.Explicitly
import org.scalactic.StringNormalizations
import org.scalactic.Uniformity
import org.scalactic.Prettifier
import org.scalactic.ColCompatHelper.Iterable
import SharedHelpers._
import StringNormalizations._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class InOrderElementsOfContainMatcherDeciderSpec extends AnyFunSpec with Explicitly {

  private val prettifier = Prettifier.default

  val mapTrimmed: Uniformity[(Int, String)] =
    new Uniformity[(Int, String)] {
      def normalized(s: (Int, String)): (Int, String) = (s._1, s._2.trim)
      def normalizedCanHandle(b: Any) =
        b match {
          case (_: Int, _: String) => true
          case _ => false
        }
      def normalizedOrSame(b: Any): Any =
        b match {
          case (k: Int, v: String) => normalized((k, v))
          case _ => b
        }
    }

  val incremented: Uniformity[Int] =
    new Uniformity[Int] {
      var count = 0
      def normalized(s: Int): Int = {
        count += 1
        s + count
      }
      def normalizedCanHandle(b: Any) = b.isInstanceOf[Int]
      def normalizedOrSame(b: Any): Any =
        b match {
          case i: Int => normalized(i)
          case _ => b
        }
    }

  val mapIncremented: Uniformity[(Int, String)] =
    new Uniformity[(Int, String)] {
      var count = 0
      def normalized(s: (Int, String)): (Int, String) = {
        count += 1
        (s._1 + count, s._2)
      }
      def normalizedCanHandle(b: Any) =
        b match {
          case (_: Int, _: String) => true
          case _ => false
        }
      def normalizedOrSame(b: Any): Any =
        b match {
          case (k: Int, v: String) => normalized((k, v))
          case _ => b
        }
    }

  val appended: Uniformity[String] =
    new Uniformity[String] {
      var count = 0
      def normalized(s: String): String = {
        count += 1
        s + count
      }
      def normalizedCanHandle(b: Any) =
        b match {
          case _: String => true
          case _ => false
        }
      def normalizedOrSame(b: Any): Any =
        b match {
          case s: String => normalized(s)
          case _ => b
        }
    }

  class Translated(map: Map[String, String]) extends Uniformity[String] {
    def normalized(s: String): String =
      map.get(s) match {
        case Some(translated) => translated
        case None => s
      }
    def normalizedCanHandle(b: Any) =
      b match {
        case _: String => true
        case _ => false
      }
    def normalizedOrSame(b: Any): Any =
      b match {
        case s: String => normalized(s)
        case _ => b
      }
  }

  val lowerCaseEquality =
    new Equality[String] {
      def areEqual(left: String, right: Any) =
        left.toLowerCase == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }

  val reverseEquality =
    new Equality[String] {
      def areEqual(left: String, right: Any) =
        left.reverse == (right match {
          case s: String => s.toLowerCase
          case other => other
        })
    }

  describe("inOrderElementsOf ") {

    def checkShouldContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      val rightText = FailureMessages.decorateToStringValue(prettifier, right)
      e.message should be (Some(leftText + " did not contain all elements of " + rightText + " in order"))
      e.failedCodeFileName should be (Some("InOrderElementsOfContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    def checkShouldNotContainStackDepth(e: exceptions.StackDepthException, left: Any, right: Iterable[Any], lineNumber: Int): Unit = {
      val leftText = FailureMessages.decorateToStringValue(prettifier, left)
      val rightText = FailureMessages.decorateToStringValue(prettifier, right)
      e.message should be (Some(leftText + " contained all elements of " + rightText + " in order"))
      e.failedCodeFileName should be (Some("InOrderElementsOfContainMatcherDeciderSpec.scala"))
      e.failedCodeLineNumber should be (Some(lineNumber))
    }

    it("should take specified equality when 'should contain' is used") {
      (List("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being trimmed)
      (Array("1 ", "2", "3 ") should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being trimmed)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1", "2 ", "3") should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being trimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take specified equality when 'should not contain' is used") {
      (List("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (after being appended)
      (Array("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (after being appended)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("1 ", "2", "3 ") should not contain inOrderElementsOf (Seq("1", "2 ", "3"))) (after being appended)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified normalization") {

      val left1 = List("1 ", "2", "3 ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e1, left1, Seq("1", "2 ", "3"), thisLineNumber - 2)

      val left2 = Array("1 ", "2", "3 ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e2, left2, Seq("1", "2 ", "3"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("1 ", "2", "3 ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderElementsOf Seq("1", "2 ", "3")) (after being appended)
      }
      checkShouldContainStackDepth(e3, left3, Seq("1", "2 ", "3"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified normalization") {
      val translated = new Translated(Map("eno" -> "one"))

      val left1 = List("one", "two", "three")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderElementsOf (Seq("eno", "two", "three"))) (after being translated)
      }
      checkShouldNotContainStackDepth(e1, left1, Seq("eno", "two", "three"), thisLineNumber - 2)

      val left2 = Array("one", "two", "three")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderElementsOf (Seq("eno", "two", "three"))) (after being translated)
      }
      checkShouldNotContainStackDepth(e2, left2, Seq("eno", "two", "three"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("one", "two", "three")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderElementsOf (Seq("eno", "two", "three"))) (after being translated)
      }
      checkShouldNotContainStackDepth(e3, left3, Seq("eno", "two", "three"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take specified equality and normalization when 'should contain' is used") {
      (List("A ", "B", "C ") should contain inOrderElementsOf Seq("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      (Array("A ", "B", "C ") should contain inOrderElementsOf Seq("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("A ", "B", "C ") should contain inOrderElementsOf Seq("a", "b ", "c")) (decided by lowerCaseEquality afterBeing trimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should take specified equality and normalization when 'should not contain' is used") {
      (List("one ", "two", "three ") should not contain inOrderElementsOf (Seq("one", "two ", "three"))) (decided by reverseEquality afterBeing trimmed)
      (Array("one ", "two", "three ") should not contain inOrderElementsOf (Seq("one", "two ", "three"))) (decided by reverseEquality afterBeing trimmed)
      // SKIP-SCALATESTJS,NATIVE-START
      (javaList("one ", "two", "three ") should not contain inOrderElementsOf (Seq("one", "two ", "three"))) (decided by reverseEquality afterBeing trimmed)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should contain custom matcher' failed with specified equality and normalization") {

      val left1 = List("one ", "two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should contain inOrderElementsOf Seq("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e1, left1, Seq("one", "two ", "three"), thisLineNumber - 2)

      val left2 = Array("one ", "two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should contain inOrderElementsOf Seq("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e2, left2, Seq("one", "two ", "three"), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("one ", "two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should contain inOrderElementsOf Seq("one", "two ", "three")) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldContainStackDepth(e3, left3, Seq("one", "two ", "three"), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }

    it("should throw TestFailedException with correct stack depth and message when 'should not contain custom matcher' failed with specified equality and normalization") {

      val left1 = List("one ", "two", "three ")
      val e1 = intercept[exceptions.TestFailedException] {
        (left1 should not contain inOrderElementsOf (Seq("eno ", "owt", "eerht "))) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e1, left1, Seq("eno ", "owt", "eerht "), thisLineNumber - 2)

      val left2 = Array("one ", "two", "three ")
      val e2 = intercept[exceptions.TestFailedException] {
        (left2 should not contain inOrderElementsOf (Seq("eno ", "owt", "eerht "))) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e2, left2, Seq("eno ", "owt", "eerht "), thisLineNumber - 2)

      // SKIP-SCALATESTJS,NATIVE-START
      val left3 = javaList("one ", "two", "three ")
      val e3 = intercept[exceptions.TestFailedException] {
        (left3 should not contain inOrderElementsOf (Seq("eno ", "owt", "eerht "))) (decided by reverseEquality afterBeing trimmed)
      }
      checkShouldNotContainStackDepth(e3, left3, Seq("eno ", "owt", "eerht "), thisLineNumber - 2)
      // SKIP-SCALATESTJS,NATIVE-END
    }
  }
}
