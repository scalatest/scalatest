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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import scala.util.Failure
import scala.util.Success
import org.scalatest.exceptions.TestCanceledException
import OptionValues._
import java.util.Date
import org.scalactic.Prettifier
import org.scalactic.exceptions.NullArgumentException
import org.scalatest.funspec.AnyFunSpec

class DirectAssertionsSpec extends AnyFunSpec {

  val fileName: String = "DirectAssertionsSpec.scala"

  private val prettifier = Prettifier.default

  describe("The === method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      org.scalatest.Assertions.assert(npe.getMessage === null)
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assert(a1 === a2)
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a1 === a3)
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assert(a1 === a2)
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a1 === a3)
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assert(a1 === a2)
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a1 === a3)
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a3 === a1)
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      org.scalatest.Assertions.assert(n1 === n2)
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(n1 === "hi")
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert("hi" === n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(n1 === a1)
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a1 === n1)
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a === null)
      }
      org.scalatest.Assertions.assert(e1.message === Some(FailureMessages.didNotEqual(prettifier, a, null)))
    }
  }
  describe("The intercept method") {
    it("should catch subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      intercept[MyException] {
        throw new MyException
      }
      intercept[MyException] {
        throw new MyExceptionSubClass
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod(): Unit = {}
      }
      class AnotherException extends RuntimeException with MyTrait
      val caught = intercept[MyTrait] {
        throw new AnotherException
      }
      // Make sure the result type is the type passed in, so I can
      // not cast and still invoke any method on it I want
      caught.someRandomMethod()
    }

    it("should throw TFE if no exception is thrown") {
      assertThrows[TestFailedException] {
        intercept[IllegalArgumentException] { "hi" }
      }
    }

    it("should return the caught exception") {
      val e = new RuntimeException
      val result = intercept[RuntimeException] {
        throw e
      }
      org.scalatest.Assertions.assert(result eq e)
    }

    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val caught =
          intercept[TestFailedException] {
            intercept[IllegalArgumentException] {
              throw wrongException
            }
          }
        org.scalatest.Assertions.assert(caught.cause.value eq wrongException)
      }
    }
  }
  describe("The assertThrows method") {
    it("should catch subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      assertThrows[MyException] {
        throw new MyException
      }
      assertThrows[MyException] {
        throw new MyExceptionSubClass
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod(): Unit = {}
      }
      class AnotherException extends RuntimeException with MyTrait
      assertThrows[MyTrait] {
        throw new AnotherException
      }
    }

    it("should return Succeeded") {
      val e = new RuntimeException
      val result = assertThrows[RuntimeException] {
        throw e
      }
      org.scalatest.Assertions.assert(result eq Succeeded)
    }

    it("should throw TFE if no exception is thrown") {
      val caught =
        intercept[TestFailedException] {
          assertThrows[Exception] { "hi" }
        }
      org.scalatest.Assertions.assert(caught.isInstanceOf[TestFailedException])
    }

    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") {
        val wrongException = new RuntimeException("oops!")
        val caught =
          intercept[TestFailedException] {
            assertThrows[IllegalArgumentException] {
              throw wrongException
            }
          }
        org.scalatest.Assertions.assert(caught.cause.value eq wrongException)
      }
    }
  }

  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages.didNotEqual(prettifier, leftee, rightee)
  }

  def equaled(left: Any, right: Any): String =
    FailureMessages.equaled(prettifier, left, right)

  def expressionFailed(left: String): String =
    FailureMessages.expressionFailed(prettifier, UnquotedString(left))

  def wasNotGreaterThan(left: Any, right: Any): String =
    FailureMessages.wasNotGreaterThan(prettifier, left, right)

  def wasGreaterThan(left: Any, right: Any): String =
    FailureMessages.wasGreaterThan(prettifier, left, right)

  def wasNotGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotGreaterThanOrEqualTo(prettifier, left, right)

  def wasGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasGreaterThanOrEqualTo(prettifier, left, right)

  def wasNotLessThan(left: Any, right: Any): String =
    FailureMessages.wasNotLessThan(prettifier, left, right)

  def wasLessThan(left: Any, right: Any): String =
    FailureMessages.wasLessThan(prettifier, left, right)

  def wasNotLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasNotLessThanOrEqualTo(prettifier, left, right)

  def wasLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages.wasLessThanOrEqualTo(prettifier, left, right)

  def commaAnd(left: String, right: String): String =
    FailureMessages.commaAnd(prettifier, UnquotedString(left), UnquotedString(right))

  def commaBut(left: String, right: String): String =
    FailureMessages.commaBut(prettifier, UnquotedString(left), UnquotedString(right))

  def wasFalse(left: String): String =
    left + " was false"

  def wasTrue(left: String): String =
    left + " was true"

  def quoteString(value: Any): String =
    value match {
      case s: String => "\"" + value + "\""
      case _ => value.toString
    }

  def didNotStartWith(left: Any, right: Any): String =
    quoteString(left) + " did not start with " + quoteString(right)

  def startedWith(left: Any, right: Any): String =
    quoteString(left) + " started with " + quoteString(right)

  def didNotEndWith(left: Any, right: Any): String =
    quoteString(left) + " did not end with " + quoteString(right)

  def endedWith(left: Any, right: Any): String =
    quoteString(left) + " ended with " + quoteString(right)

  def didNotContain(left: Any, right: Any): String =
    quoteString(left) + " did not contain " + quoteString(right)

  def contained(left: Any, right: Any): String =
    quoteString(left) + " contained " + quoteString(right)

  def didNotContainKey(left: Any, right: Any): String =
    Prettifier.default(left) + " did not contain key " + quoteString(right)

  def containedKey(left: Any, right: Any): String =
    Prettifier.default(left) + " contained key " + quoteString(right)

  def wasNotTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    quoteString(left) + " was not the same instance as " + quoteString(right)

  def wasTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    quoteString(left) + " was the same instance as " + quoteString(right)

  def wasNotEmpty(left: Any): String =
    quoteString(left) + " was not empty"

  def wasEmpty(left: Any): String =
    quoteString(left) + " was empty"

  def wasNotInstanceOf(left: Any, className: String) =
    quoteString(left) + " was not instance of " + className

  def wasInstanceOf(left: Any, className: String) =
    quoteString(left) + " was instance of " + className

  def hadLengthInsteadOfExpectedLength(left: Any, actual: Any, expected: Any): String =
    FailureMessages.hadLengthInsteadOfExpectedLength(prettifier, left, actual, expected)

  def hadLength(left: Any, actual: Long): String =
    FailureMessages.hadLength(prettifier, left, actual)

  def hadSizeInsteadOfExpectedSize(left: Any, actual: Any, expected: Any): String =
    FailureMessages.hadSizeInsteadOfExpectedSize(prettifier, left, actual, expected)

  def hadSize(left: Any, actual: Long): String =
    FailureMessages.hadSize(prettifier, left, actual)

  class Stateful {
    var state = false
    def changeState: Boolean = {
      state = true
      state
    }
  }

  class CustomInt(value: Int) {

    def startsWith(v: Int): Boolean = {
      value.toString.startsWith(v.toString)
    }

    def endsWith(v: Int): Boolean = {
      value.toString.endsWith(v.toString)
    }

    def contains(v: Int): Boolean = {
      value.toString.contains(v.toString)
    }

    def exists(v: Int): Boolean = {
      value == v
    }

    override def toString: String = value.toString
  }

  class CustomContainer[+E](e: E) {
    val element: E = e

    def contains[E1 >: E](elem: E1): Boolean = elem == element
  }

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true

  class FloatLengthSize(value: Float) {
    val length: Float = value
    val size: Float = value
  }

  val floatLengthSize = new FloatLengthSize(2.0f)

  describe("The org.scalatest.Assertions.assert(boolean) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should do nothing when is used to check a == 3") {
      org.scalatest.Assertions.assert(a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalatest.Assertions.assert(5 == b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == b)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      org.scalatest.Assertions.assert(a != 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a != 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalatest.Assertions.assert(3 != b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 != b)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalatest.Assertions.assert(3 == 3)
    }

    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == 5)
      }
      org.scalatest.Assertions.assert(e1.message === None)
      org.scalatest.Assertions.assert(e1.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == 5, "3 did not equal 5")
      }
      org.scalatest.Assertions.assert(e2.message === Some("3 did not equal 5"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == b)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // SKIP-DOTTY-START
    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == null)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, null)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(null == a)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(null, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    // SKIP-DOTTY-END

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 != a)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalatest.Assertions.assert(5 != a)
    }

    it("should do nothing when is used to check a > 2") {
      org.scalatest.Assertions.assert(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalatest.Assertions.assert(5 > a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a > 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 > a)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalatest.Assertions.assert(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalatest.Assertions.assert(3 >= a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a >= 4)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(2 >= a)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalatest.Assertions.assert(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalatest.Assertions.assert(3 < b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(b < 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 < b)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalatest.Assertions.assert(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalatest.Assertions.assert(5 <= b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(b <= 4)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(6 <= b)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      org.scalatest.Assertions.assert(bob == "bob")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      org.scalatest.Assertions.assert(bob != "alice")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      org.scalatest.Assertions.assert(alice == "alice")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      org.scalatest.Assertions.assert(alice != "bob")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(bob == "alice")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(bob, "alice")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(bob != "bob")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(bob, "bob")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(alice == "bob")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(alice, "bob")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(alice != "alice")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(alice, "alice")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      org.scalatest.Assertions.assert(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a === 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalatest.Assertions.assert(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 === a)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(5, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalatest.Assertions.assert(a !== 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a !== 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalatest.Assertions.assert(5 !== a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 !== a)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalatest.Assertions.assert(a == 3 && b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 && b == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 && b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalatest.Assertions.assert(a == 3 & b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 & b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 & b == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 & b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalatest.Assertions.assert(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalatest.Assertions.assert(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalatest.Assertions.assert(a == 2 || b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 || b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalatest.Assertions.assert(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalatest.Assertions.assert(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalatest.Assertions.assert(a == 2 | b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 | b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalatest.Assertions.assert(a == 3 && (b == 5 && b > 3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && (b == 5 && b > 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5)))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalatest.Assertions.assert(!(a == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(a == 3))
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && !(b == 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalatest.Assertions.assert((a == 3) == (b == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert((a == 3) == (b != 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(true, false)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5 && s.changeState)
      }
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5 & s.changeState)
      }
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assert(a == 3 || s.changeState)
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assert(a == 3 | s.changeState)
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalatest.Assertions.assert(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && { println("hi"); b == 3})
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else // for dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      org.scalatest.Assertions.assert({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert({ println("hi"); b == 5} && a == 5)
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalatest.Assertions.assert(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assert(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assert(neverRuns3(sys.error("Sad times 3"))(0))
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalatest.Assertions.assert(s1 startsWith "hi")
      org.scalatest.Assertions.assert(s1.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s2 startsWith "hi")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(s2, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s2.startsWith("hi"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(s2, "hi")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalatest.Assertions.assert(ci1 startsWith 1)
      org.scalatest.Assertions.assert(ci1.startsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci2 startsWith 1)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(ci2, 1)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci2.startsWith(1))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(ci2, 1)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalatest.Assertions.assert(!s2.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s1.startsWith("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(startedWith(s1, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalatest.Assertions.assert(s2 endsWith "hi")
      org.scalatest.Assertions.assert(s2.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1 endsWith "hi")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(s1, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.endsWith("hi"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(s1, "hi")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalatest.Assertions.assert(ci2 endsWith 1)
      org.scalatest.Assertions.assert(ci2.endsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 endsWith 1)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(ci1, 1)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.endsWith(1))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(ci1, 1)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalatest.Assertions.assert(!s1.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s2.endsWith("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(endedWith(s2, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalatest.Assertions.assert(s3 contains "hi")
      org.scalatest.Assertions.assert(s3.contains("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3 contains "hello")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(s3, "hello")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3.contains("hello"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(s3, "hello")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalatest.Assertions.assert(ci2 contains 2)
      org.scalatest.Assertions.assert(ci2.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ci1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ci1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalatest.Assertions.assert(!s3.contains("hello"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s3.contains("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(s3, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalatest.Assertions.assert(l1 contains 2)
      org.scalatest.Assertions.assert(l1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalatest.Assertions.assert(!(l1 contains 5))
      org.scalatest.Assertions.assert(!l1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1 contains 2))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(l1, 2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.contains(2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(contained(l1, 2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalatest.Assertions.assert(m1 contains 2)
      org.scalatest.Assertions.assert(m1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(m1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContainKey(m1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(m1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContainKey(m1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalatest.Assertions.assert(!(m1 contains 5))
      org.scalatest.Assertions.assert(!m1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(m1 contains 2))
      }
      org.scalatest.Assertions.assert(e1.message == Some(containedKey(m1, 2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!m1.contains(2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(containedKey(m1, 2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalatest.Assertions.assert(ct1 contains 8)
      org.scalatest.Assertions.assert(ct1.contains(8))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ct1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ct1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ct1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ct1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalatest.Assertions.assert(!ct1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ct1.contains(8))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(ct1, 8)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalatest.Assertions.assert(ci1 eq ci3)
      org.scalatest.Assertions.assert(ci1.eq(ci3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 eq ci2)
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.eq(ci2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalatest.Assertions.assert(!ci1.eq(ci2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ci1.eq(ci3))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalatest.Assertions.assert(ci1 ne ci2)
      org.scalatest.Assertions.assert(ci1.ne(ci2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 ne ci3)
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.ne(ci3))
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalatest.Assertions.assert(!ci1.ne(ci3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ci1.ne(ci2))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalatest.Assertions.assert(s4.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalatest.Assertions.assert(!s3.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s4.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalatest.Assertions.assert(l2.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalatest.Assertions.assert(!l1.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l2.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalatest.Assertions.assert(s3.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s4.nonEmpty ") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s4.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalatest.Assertions.assert(!s4.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s3.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalatest.Assertions.assert(l1.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l2.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.isEmpty") {
      org.scalatest.Assertions.assert(!l2.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalatest.Assertions.assert(s1.isInstanceOf[String])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isInstanceOf[String])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assert(l1.isInstanceOf[List[Int]])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalatest.Assertions.assert(date.isInstanceOf[Date])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isInstanceOf[Date])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalatest.Assertions.assert(!l1.isInstanceOf[String])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s1.isInstanceOf[String])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assert(!s1.isInstanceOf[List[Int]])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalatest.Assertions.assert(!l1.isInstanceOf[Date])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!date.isInstanceOf[Date])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(date, "java.util.Date")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalatest.Assertions.assert(s1.length == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.length == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalatest.Assertions.assert(l1.length == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.length == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalatest.Assertions.assert(!(s1.length == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(s1.length == 12))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(s1, 12)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalatest.Assertions.assert(!(l1.length == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1.length == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalatest.Assertions.assert(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(floatLengthSize.length == 1.0f)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalatest.Assertions.assert(s1.size == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.size == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalatest.Assertions.assert(l1.size == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.size == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalatest.Assertions.assert(!(s1.size == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(s1.size == 12))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(s1, 12)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalatest.Assertions.assert(!(l1.size == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1.size == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalatest.Assertions.assert(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(floatLengthSize.size == 1.0f)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalatest.Assertions.assert(l1.exists(_ == 3))
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      org.scalatest.Assertions.assert(l1.exists(3 == _))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(_ == 5))
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(5 == _))
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalatest.Assertions.assert(!l1.exists(_ == 5))
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      org.scalatest.Assertions.assert(!l1.exists(5 == _))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.exists(_ == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.exists(3 == _))
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$9: scala.Int) => _$9.>(3)))" else "l1.exists(((x$10: Int) => x$10.>(3)))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(3 < _))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$10: scala.Int) => 3.<(_$10)))" else "l1.exists(((x$11: Int) => 3.<(x$11)))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$11: java.lang.String) => _$11.isEmpty()))" else "l3.exists(((x$12: String) => x$12.isEmpty()))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.exists(321))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasFalse("ci1.exists(321)")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalatest.Assertions.assert(woof { meow(y = 5) } == "woof")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(woof { meow(y = 5) } == "meow")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotEqual("woof", "meow")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assert(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assert(org === "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalatest.Assertions.assert(org === "test")
          |  }
          |}
        """.stripMargin)
    }

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalatest.Assertions.assert(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalatest.Assertions.assert(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalatest.Assertions.assert(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalatest.Assertions.assert(org.exists(_ == 'b'))
        """.stripMargin)
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(org.scalatest.Assertions.assert(x + 1 == 2) eq Succeeded)
    }
  }

  describe("The org.scalatest.Assertions.assert(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should throw NullArgumentException when null is passed in as clue") {
      val e = intercept[NullArgumentException] {
        org.scalatest.Assertions.assert(a == 3, null)
      }
      org.scalatest.Assertions.assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      org.scalatest.Assertions.assert(a == 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalatest.Assertions.assert(5 == b, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == b, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      org.scalatest.Assertions.assert(a != 5, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a != 3, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalatest.Assertions.assert(3 != b, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 != b, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(5, 5) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalatest.Assertions.assert(3 == 3, "dude")
    }

    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == 5, "dude")
      }
      org.scalatest.Assertions.assert(e1.message === Some("dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 == 5, "3 did not equal 5")
      }
      org.scalatest.Assertions.assert(e2.message === Some("3 did not equal 5"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == b, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // SKIP-DOTTY-START
    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == null, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, null) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(null == a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(null, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    // SKIP-DOTTY-END

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 != a, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalatest.Assertions.assert(5 != a, ". dude")
    }

    it("should do nothing when is used to check a > 2") {
      org.scalatest.Assertions.assert(a > 2, ". dude")
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalatest.Assertions.assert(5 > a, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a > 3, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 > a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalatest.Assertions.assert(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalatest.Assertions.assert(3 >= a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a >= 4, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(2 >= a, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalatest.Assertions.assert(b < 6, "; dude")
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalatest.Assertions.assert(3 < b, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(b < 5, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 < b, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalatest.Assertions.assert(b <= 5, ". dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalatest.Assertions.assert(5 <= b, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(b <= 4, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(6 <= b, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      org.scalatest.Assertions.assert(bob == "bob", "dude")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      org.scalatest.Assertions.assert(bob != "alice", "dude")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      org.scalatest.Assertions.assert(alice == "alice", "dude")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      org.scalatest.Assertions.assert(alice != "bob", "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(bob == "alice", "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(bob, "alice") + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(bob != "bob", ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(bob, "bob") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(alice == "bob", ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(alice, "bob") + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(alice != "alice", "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(alice, "alice") + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      org.scalatest.Assertions.assert(a === 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a === 5, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalatest.Assertions.assert(3 === a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(5 === a, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(5, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalatest.Assertions.assert(a !== 5, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a !== 3, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalatest.Assertions.assert(5 !== a, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(3 !== a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalatest.Assertions.assert(a == 3 && b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 && b == 5, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 && b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalatest.Assertions.assert(a == 3 & b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 & b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 & b == 5, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 & b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalatest.Assertions.assert(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalatest.Assertions.assert(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalatest.Assertions.assert(a == 2 || b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 || b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalatest.Assertions.assert(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalatest.Assertions.assert(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalatest.Assertions.assert(a == 2 | b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 2 | b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalatest.Assertions.assert(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && (b == 5 && b > 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalatest.Assertions.assert(!(a == 5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(a == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && !(b == 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalatest.Assertions.assert((a == 3) == (b == 5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert((a == 3) == (b != 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(true, false) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5 && s.changeState, ", dude")
      }
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 5 & s.changeState, ", dude")
      }
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assert(a == 3 || s.changeState, ", dude")
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assert(a == 3 | s.changeState, ", dude")
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalatest.Assertions.assert(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalatest.Assertions.assert({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalatest.Assertions.assert(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assert(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assert(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalatest.Assertions.assert(s1 startsWith "hi", ", dude")
      org.scalatest.Assertions.assert(s1.startsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s2 startsWith "hi", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s2.startsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalatest.Assertions.assert(ci1 startsWith 1, ", dude")
      org.scalatest.Assertions.assert(ci1.startsWith(1), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci2 startsWith 1, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci2.startsWith(1), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalatest.Assertions.assert(!s2.startsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s1.startsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(startedWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalatest.Assertions.assert(s2 endsWith "hi", ", dude")
      org.scalatest.Assertions.assert(s2.endsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1 endsWith "hi", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.endsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalatest.Assertions.assert(ci2 endsWith 1, ", dude")
      org.scalatest.Assertions.assert(ci2.endsWith(1), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 endsWith 1, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.endsWith(1), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalatest.Assertions.assert(!s1.endsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s2.endsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(endedWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalatest.Assertions.assert(s3 contains "hi", ", dude")
      org.scalatest.Assertions.assert(s3.contains("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3 contains "hello", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(s3, "hello") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3.contains("hello"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(s3, "hello") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalatest.Assertions.assert(ci2 contains 2, ", dude")
      org.scalatest.Assertions.assert(ci2.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ci1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ci1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalatest.Assertions.assert(!s3.contains("hello"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s3.contains("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(s3, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalatest.Assertions.assert(l1 contains 2, ", dude")
      org.scalatest.Assertions.assert(l1.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalatest.Assertions.assert(!(l1 contains 5), ", dude")
      org.scalatest.Assertions.assert(!l1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1 contains 2), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(l1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.contains(2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(contained(l1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalatest.Assertions.assert(m1 contains 2, ", dude")
      org.scalatest.Assertions.assert(m1.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(m1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContainKey(m1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(m1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContainKey(m1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalatest.Assertions.assert(!(m1 contains 5), ", dude")
      org.scalatest.Assertions.assert(!m1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(m1 contains 2), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(containedKey(m1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!m1.contains(2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(containedKey(m1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalatest.Assertions.assert(ct1 contains 8, ", dude")
      org.scalatest.Assertions.assert(ct1.contains(8), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ct1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ct1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ct1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ct1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalatest.Assertions.assert(!ct1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ct1.contains(8), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(ct1, 8) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalatest.Assertions.assert(ci1 eq ci3, ", dude")
      org.scalatest.Assertions.assert(ci1.eq(ci3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 eq ci2, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.eq(ci2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalatest.Assertions.assert(!ci1.eq(ci2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ci1.eq(ci3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalatest.Assertions.assert(ci1 ne ci2, ", dude")
      org.scalatest.Assertions.assert(ci1.ne(ci2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1 ne ci3, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.ne(ci3), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalatest.Assertions.assert(!ci1.ne(ci3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!ci1.ne(ci2), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalatest.Assertions.assert(s4.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s3.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalatest.Assertions.assert(!s3.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s4.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalatest.Assertions.assert(l2.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalatest.Assertions.assert(!l1.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l2.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalatest.Assertions.assert(s3.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s4.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalatest.Assertions.assert(!s4.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s3.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalatest.Assertions.assert(l1.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l2.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalatest.Assertions.assert(!l2.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalatest.Assertions.assert(s1.isInstanceOf[String], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isInstanceOf[String], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assert(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalatest.Assertions.assert(date.isInstanceOf[Date], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.isInstanceOf[Date], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalatest.Assertions.assert(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!s1.isInstanceOf[String], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assert(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalatest.Assertions.assert(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!date.isInstanceOf[Date], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(date, "java.util.Date") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalatest.Assertions.assert(s1.length == 12, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.length == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalatest.Assertions.assert(l1.length == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.length == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalatest.Assertions.assert(!(s1.length == 10), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(s1.length == 12), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(s1, 12) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalatest.Assertions.assert(!(l1.length == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1.length == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalatest.Assertions.assert(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(floatLengthSize.length == 1.0f, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalatest.Assertions.assert(s1.size == 12, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(s1.size == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalatest.Assertions.assert(l1.size == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.size == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalatest.Assertions.assert(!(s1.size == 10), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(s1.size == 12), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(s1, 12) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalatest.Assertions.assert(!(l1.size == 2), ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalatest.Assertions.assert(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(floatLengthSize.size == 1.0f, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!(l1.size == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalatest.Assertions.assert(l1.exists(_ == 3), ", dude")
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      org.scalatest.Assertions.assert(l1.exists(3 == _), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(_ == 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(5 == _), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalatest.Assertions.assert(!l1.exists(_ == 5), ", dude")
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      org.scalatest.Assertions.assert(!l1.exists(5 == _), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.exists(_ == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(!l1.exists(3 == _), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$20: scala.Int) => _$20.>(3)))" else "l1.exists(((x$21: Int) => x$21.>(3)))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l1.exists(3 < _), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$21: scala.Int) => 3.<(_$21)))" else "l1.exists(((x$22: Int) => 3.<(x$22)))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$22: java.lang.String) => _$22.isEmpty()))" else "l3.exists(((x$23: String) => x$23.isEmpty()))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(ci1.exists(321), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasFalse("ci1.exists(321)") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalatest.Assertions.assert(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestFailedException] {
        org.scalatest.Assertions.assert(woof { meow(y = 5) } == "meow", ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotEqual("woof", "meow") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assert(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assert(org === "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalatest.Assertions.assert(org === "test", ", dude")
          |  }
          |}
        """.stripMargin)
    }

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalatest.Assertions.assert(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalatest.Assertions.assert(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalatest.Assertions.assert(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assert(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalatest.Assertions.assert(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(org.scalatest.Assertions.assert(x + 1 == 2, "clue") eq Succeeded)
    }
  }

  describe("The org.scalatest.Assertions.assume(boolean) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should do nothing when is used to check a == 3") {
      org.scalatest.Assertions.assume(a == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalatest.Assertions.assume(5 == b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == b)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      org.scalatest.Assertions.assume(a != 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a != 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalatest.Assertions.assume(3 != b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 != b)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalatest.Assertions.assume(3 == 3)
    }

    it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == 5)
      }
      org.scalatest.Assertions.assert(e1.message === None)
      org.scalatest.Assertions.assert(e1.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == 5, "3 did not equal 5")
      }
      org.scalatest.Assertions.assert(e2.message === Some("3 did not equal 5"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == b)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // SKIP-DOTTY-START
    it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == null)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, null)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(null == a)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(null, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    // SKIP-DOTTY-END

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 != a)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalatest.Assertions.assume(5 != a)
    }

    it("should do nothing when is used to check a > 2") {
      org.scalatest.Assertions.assume(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalatest.Assertions.assume(5 > a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a > 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 > a)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalatest.Assertions.assume(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalatest.Assertions.assume(3 >= a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a >= 4)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(2 >= a)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalatest.Assertions.assume(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalatest.Assertions.assume(3 < b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(b < 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 < b)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalatest.Assertions.assume(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalatest.Assertions.assume(5 <= b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(b <= 4)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(6 <= b)
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      org.scalatest.Assertions.assume(bob == "bob")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      org.scalatest.Assertions.assume(bob != "alice")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      org.scalatest.Assertions.assume(alice == "alice")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      org.scalatest.Assertions.assume(alice != "bob")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(bob == "alice")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(bob, "alice")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(bob != "bob")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(bob, "bob")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(alice == "bob")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(alice, "bob")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(alice != "alice")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(alice, "alice")))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      org.scalatest.Assertions.assume(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a === 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalatest.Assertions.assume(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 === a)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(5, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalatest.Assertions.assume(a !== 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a !== 3)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalatest.Assertions.assume(5 !== a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 !== a)
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalatest.Assertions.assume(a == 3 && b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 && b == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 && b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalatest.Assertions.assume(a == 3 & b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 & b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 & b == 5)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 & b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalatest.Assertions.assume(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalatest.Assertions.assume(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalatest.Assertions.assume(a == 2 || b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 || b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalatest.Assertions.assume(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalatest.Assertions.assume(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalatest.Assertions.assume(a == 2 | b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 | b == 6)
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalatest.Assertions.assume(a == 3 && (b == 5 && b > 3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && (b == 5 && b > 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5)))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalatest.Assertions.assume(!(a == 5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(a == 3))
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && !(b == 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalatest.Assertions.assume((a == 3) == (b == 5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume((a == 3) == (b != 5))
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(true, false)))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5 && s.changeState)
      }
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5 & s.changeState)
      }
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assume(a == 3 || s.changeState)
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assume(a == 3 | s.changeState)
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalatest.Assertions.assume(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && { println("hi"); b == 3})
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      org.scalatest.Assertions.assume({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume({ println("hi"); b == 5} && a == 5)
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalatest.Assertions.assume(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assume(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assume(neverRuns3(sys.error("Sad times 3"))(0))
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalatest.Assertions.assume(s1 startsWith "hi")
      org.scalatest.Assertions.assume(s1.startsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s2 startsWith "hi")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(s2, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s2.startsWith("hi"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(s2, "hi")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalatest.Assertions.assume(ci1 startsWith 1)
      org.scalatest.Assertions.assume(ci1.startsWith(1))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci2 startsWith 1)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(ci2, 1)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci2.startsWith(1))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(ci2, 1)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalatest.Assertions.assume(!s2.startsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s1.startsWith("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(startedWith(s1, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalatest.Assertions.assume(s2 endsWith "hi")
      org.scalatest.Assertions.assume(s2.endsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1 endsWith "hi")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(s1, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.endsWith("hi"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(s1, "hi")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalatest.Assertions.assume(ci2 endsWith 1)
      org.scalatest.Assertions.assume(ci2.endsWith(1))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 endsWith 1)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(ci1, 1)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.endsWith(1))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(ci1, 1)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalatest.Assertions.assume(!s1.endsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s2.endsWith("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(endedWith(s2, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalatest.Assertions.assume(s3 contains "hi")
      org.scalatest.Assertions.assume(s3.contains("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3 contains "hello")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(s3, "hello")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3.contains("hello"))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(s3, "hello")))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalatest.Assertions.assume(ci2 contains 2)
      org.scalatest.Assertions.assume(ci2.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ci1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ci1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalatest.Assertions.assume(!s3.contains("hello"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s3.contains("hi"))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(s3, "hi")))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalatest.Assertions.assume(l1 contains 2)
      org.scalatest.Assertions.assume(l1.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalatest.Assertions.assume(!(l1 contains 5))
      org.scalatest.Assertions.assume(!l1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1 contains 2))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(l1, 2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.contains(2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(contained(l1, 2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalatest.Assertions.assume(m1 contains 2)
      org.scalatest.Assertions.assume(m1.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(m1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContainKey(m1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(m1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContainKey(m1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalatest.Assertions.assume(!(m1 contains 5))
      org.scalatest.Assertions.assume(!m1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(m1 contains 2))
      }
      org.scalatest.Assertions.assert(e1.message == Some(containedKey(m1, 2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!m1.contains(2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(containedKey(m1, 2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalatest.Assertions.assume(ct1 contains 8)
      org.scalatest.Assertions.assume(ct1.contains(8))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ct1 contains 5)
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ct1, 5)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ct1.contains(5))
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ct1, 5)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalatest.Assertions.assume(!ct1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ct1.contains(8))
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(ct1, 8)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalatest.Assertions.assume(ci1 eq ci3)
      org.scalatest.Assertions.assume(ci1.eq(ci3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 eq ci2)
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.eq(ci2))
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalatest.Assertions.assume(!ci1.eq(ci2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ci1.eq(ci3))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalatest.Assertions.assume(ci1 ne ci2)
      org.scalatest.Assertions.assume(ci1.ne(ci2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 ne ci3)
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.ne(ci3))
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalatest.Assertions.assume(!ci1.ne(ci3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ci1.ne(ci2))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalatest.Assertions.assume(s4.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalatest.Assertions.assume(!s3.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s4.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalatest.Assertions.assume(l2.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalatest.Assertions.assume(!l1.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l2.isEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalatest.Assertions.assume(s3.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s4.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalatest.Assertions.assume(!s4.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s3.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalatest.Assertions.assume(l1.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l2.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalatest.Assertions.assume(!l2.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.nonEmpty)
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalatest.Assertions.assume(s1.isInstanceOf[String])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isInstanceOf[String])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assume(l1.isInstanceOf[List[Int]])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalatest.Assertions.assume(date.isInstanceOf[Date])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isInstanceOf[Date])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalatest.Assertions.assume(!l1.isInstanceOf[String])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s1.isInstanceOf[String])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assume(!s1.isInstanceOf[List[Int]])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalatest.Assertions.assume(!l1.isInstanceOf[Date])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!date.isInstanceOf[Date])
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(date, "java.util.Date")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalatest.Assertions.assume(s1.length == 12)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.length == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalatest.Assertions.assume(l1.length == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.length == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalatest.Assertions.assume(!(s1.length == 10))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(s1.length == 12))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(s1, 12)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalatest.Assertions.assume(!(l1.length == 2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1.length == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalatest.Assertions.assume(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(floatLengthSize.length == 1.0f)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalatest.Assertions.assume(s1.size == 12)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.size == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalatest.Assertions.assume(l1.size == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.size == 10)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalatest.Assertions.assume(!(s1.size == 10))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(s1.size == 12))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(s1, 12)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalatest.Assertions.assume(!(l1.size == 2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1.size == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalatest.Assertions.assume(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(floatLengthSize.size == 1.0f)
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalatest.Assertions.assume(l1.exists(_ == 3))
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      org.scalatest.Assertions.assume(l1.exists(3 == _))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(_ == 5))
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(5 == _))
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalatest.Assertions.assume(!l1.exists(_ == 5))
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      org.scalatest.Assertions.assume(!l1.exists(5 == _))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.exists(_ == 3))
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.exists(3 == _))
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3)))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$31: scala.Int) => _$31.>(3)))" else "l1.exists(((x$32: Int) => x$32.>(3)))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(3 < _))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$32: scala.Int) => 3.<(_$32)))" else "l1.exists(((x$33: Int) => 3.<(x$33)))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$33: java.lang.String) => _$33.isEmpty()))" else "l3.exists(((x$34: String) => x$34.isEmpty()))")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.exists(321))
      }
      org.scalatest.Assertions.assert(e.message == Some(wasFalse("ci1.exists(321)")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalatest.Assertions.assume(woof { meow(y = 5) } == "woof")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(woof { meow(y = 5) } == "meow")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotEqual("woof", "meow")))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assume(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assume(org === "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalatest.Assertions.assume(org === "test")
          |  }
          |}
        """.stripMargin)
    }

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalatest.Assertions.assume(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalatest.Assertions.assume(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalatest.Assertions.assume(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalatest.Assertions.assume(org.exists(_ == 'b'))
        """.stripMargin)
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(org.scalatest.Assertions.assume(x + 1 == 2) eq Succeeded)
    }
  }

  describe("The org.scalatest.Assertions.assume(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should throw NullArgumentException when null is passed in as clue") {
      val e = intercept[NullArgumentException] {
        org.scalatest.Assertions.assume(a == 3, null)
      }
      org.scalatest.Assertions.assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      org.scalatest.Assertions.assume(a == 3, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      org.scalatest.Assertions.assume(5 == b, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == b, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      org.scalatest.Assertions.assume(a != 5, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a != 3, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      org.scalatest.Assertions.assume(3 != b, "; dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 != b, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(5, 5) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      org.scalatest.Assertions.assume(3 == 3, "dude")
    }

    it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == 5, "dude")
      }
      org.scalatest.Assertions.assert(e1.message === Some("dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 == 5, "3 did not equal 5")
      }
      org.scalatest.Assertions.assert(e2.message === Some("3 did not equal 5"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == b, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // SKIP-DOTTY-START
    it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == null, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, null) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(null == a, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(null, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    // SKIP-DOTTY-END

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 != a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      org.scalatest.Assertions.assume(5 != a, "dude")
    }

    it("should do nothing when is used to check a > 2") {
      org.scalatest.Assertions.assume(a > 2, "dude")
    }

    it("should do nothing when is used to check 5 > a") {
      org.scalatest.Assertions.assume(5 > a, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a > 3, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 > a, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThan(3, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      org.scalatest.Assertions.assume(a >= 3, ". dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      org.scalatest.Assertions.assume(3 >= a, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a >= 4, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(2 >= a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      org.scalatest.Assertions.assume(b < 6, "dude")
    }

    it("should do nothing when is used to check 3 < b") {
      org.scalatest.Assertions.assume(3 < b, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(b < 5, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 < b, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThan(5, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      org.scalatest.Assertions.assume(b <= 5, ". dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      org.scalatest.Assertions.assume(5 <= b, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(b <= 4, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(6 <= b, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      org.scalatest.Assertions.assume(bob == "bob", "dude")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      org.scalatest.Assertions.assume(bob != "alice", "dude")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      org.scalatest.Assertions.assume(alice == "alice", "dude")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      org.scalatest.Assertions.assume(alice != "bob", "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(bob == "alice", "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(bob, "alice") + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(bob != "bob", ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(bob, "bob") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(alice == "bob", ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(alice, "bob") + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(alice != "alice", "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(alice, "alice") + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      org.scalatest.Assertions.assume(a === 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a === 5, "dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      org.scalatest.Assertions.assume(3 === a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(5 === a, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(5, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      org.scalatest.Assertions.assume(a !== 5, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a !== 3, ". dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ". dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      org.scalatest.Assertions.assume(5 !== a, "; dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(3 !== a, "; dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + "; dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      org.scalatest.Assertions.assume(a == 3 && b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 && b == 5, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 && b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      org.scalatest.Assertions.assume(a == 3 & b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 & b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 & b == 5, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 & b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      org.scalatest.Assertions.assume(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      org.scalatest.Assertions.assume(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      org.scalatest.Assertions.assume(a == 2 || b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 || b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      org.scalatest.Assertions.assume(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      org.scalatest.Assertions.assume(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      org.scalatest.Assertions.assume(a == 2 | b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 2 | b == 6, ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      org.scalatest.Assertions.assume(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && (b == 5 && b > 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      org.scalatest.Assertions.assume(!(a == 5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(a == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(equaled(3, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && !(b == 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      org.scalatest.Assertions.assume((a == 3) == (b == 5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume((a == 3) == (b != 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message === Some(didNotEqual(true, false) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5 && s.changeState, ", dude")
      }
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 5 & s.changeState, ", dude")
      }
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assume(a == 3 || s.changeState, ", dude")
      org.scalatest.Assertions.assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      org.scalatest.Assertions.assume(a == 3 | s.changeState, ", dude")
      org.scalatest.Assertions.assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      org.scalatest.Assertions.assume(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      org.scalatest.Assertions.assume({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      org.scalatest.Assertions.assume(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assume(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      org.scalatest.Assertions.assume(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
    }

    val s1 = "hi ScalaTest"
    val s2 = "ScalaTest hi"
    val s3 = "Say hi to ScalaTest"
    val s4 = ""

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)
    val l2 = List.empty[Int]
    val l3 = List("one", "two", "three")

    val m1 = Map(1 -> "one", 2 -> "two", 3 -> "three")
    val m2 = Map.empty[Int, String]

    val ct1 = new CustomContainer(8)

    val date = new Date

    it("should do nothing when is used to check s1 startsWith \"hi\"") {
      org.scalatest.Assertions.assume(s1 startsWith "hi", ", dude")
      org.scalatest.Assertions.assume(s1.startsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s2 startsWith "hi", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s2.startsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      org.scalatest.Assertions.assume(ci1 startsWith 1, ", dude")
      org.scalatest.Assertions.assume(ci1.startsWith(1), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci2 startsWith 1, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci2.startsWith(1), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      org.scalatest.Assertions.assume(!s2.startsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s1.startsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(startedWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      org.scalatest.Assertions.assume(s2 endsWith "hi", ", dude")
      org.scalatest.Assertions.assume(s2.endsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1 endsWith "hi", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.endsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      org.scalatest.Assertions.assume(ci2 endsWith 1, ", dude")
      org.scalatest.Assertions.assume(ci2.endsWith(1), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 endsWith 1, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.endsWith(1), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      org.scalatest.Assertions.assume(!s1.endsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s2.endsWith("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(endedWith(s2, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      org.scalatest.Assertions.assume(s3 contains "hi", ", dude")
      org.scalatest.Assertions.assume(s3.contains("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3 contains "hello", ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(s3, "hello") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3.contains("hello"), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(s3, "hello") + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      org.scalatest.Assertions.assume(ci2 contains 2, ", dude")
      org.scalatest.Assertions.assume(ci2.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ci1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ci1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      org.scalatest.Assertions.assume(!s3.contains("hello"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s3.contains("hi"), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(s3, "hi") + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      org.scalatest.Assertions.assume(l1 contains 2, ", dude")
      org.scalatest.Assertions.assume(l1.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      org.scalatest.Assertions.assume(!(l1 contains 5), ", dude")
      org.scalatest.Assertions.assume(!l1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1 contains 2), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(l1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.contains(2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(contained(l1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      org.scalatest.Assertions.assume(m1 contains 2, ", dude")
      org.scalatest.Assertions.assume(m1.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(m1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContainKey(m1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(m1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContainKey(m1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      org.scalatest.Assertions.assume(!(m1 contains 5), ", dude")
      org.scalatest.Assertions.assume(!m1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(m1 contains 2), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(containedKey(m1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!m1.contains(2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(containedKey(m1, 2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      org.scalatest.Assertions.assume(ct1 contains 8, ", dude")
      org.scalatest.Assertions.assume(ct1.contains(8), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ct1 contains 5, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(didNotContain(ct1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ct1.contains(5), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(didNotContain(ct1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      org.scalatest.Assertions.assume(!ct1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ct1.contains(8), ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(contained(ct1, 8) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      org.scalatest.Assertions.assume(ci1 eq ci3, ", dude")
      org.scalatest.Assertions.assume(ci1.eq(ci3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 eq ci2, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.eq(ci2), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      org.scalatest.Assertions.assume(!ci1.eq(ci2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ci1.eq(ci3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      org.scalatest.Assertions.assume(ci1 ne ci2, ", dude")
      org.scalatest.Assertions.assume(ci1.ne(ci2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1 ne ci3, ", dude")
      }
      org.scalatest.Assertions.assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e1.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.ne(ci3), ", dude")
      }
      org.scalatest.Assertions.assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      org.scalatest.Assertions.assert(e2.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      org.scalatest.Assertions.assume(!ci1.ne(ci3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!ci1.ne(ci2), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      org.scalatest.Assertions.assume(s4.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s3.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      org.scalatest.Assertions.assume(!s3.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s4.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      org.scalatest.Assertions.assume(l2.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      org.scalatest.Assertions.assume(!l1.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l2.isEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      org.scalatest.Assertions.assume(s3.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s4.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(s4) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      org.scalatest.Assertions.assume(!s4.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s3.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      org.scalatest.Assertions.assume(l1.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l2.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasEmpty(l2) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      org.scalatest.Assertions.assume(!l2.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.nonEmpty, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      org.scalatest.Assertions.assume(s1.isInstanceOf[String], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isInstanceOf[String], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assume(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      org.scalatest.Assertions.assume(date.isInstanceOf[Date], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.isInstanceOf[Date], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      org.scalatest.Assertions.assume(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!s1.isInstanceOf[String], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      org.scalatest.Assertions.assume(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      org.scalatest.Assertions.assume(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!date.isInstanceOf[Date], ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasInstanceOf(date, "java.util.Date") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      org.scalatest.Assertions.assume(s1.length == 12, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.length == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      org.scalatest.Assertions.assume(l1.length == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.length == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      org.scalatest.Assertions.assume(!(s1.length == 10), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(s1.length == 12), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(s1, 12) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      org.scalatest.Assertions.assume(!(l1.length == 2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1.length == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLength(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      org.scalatest.Assertions.assume(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(floatLengthSize.length == 1.0f, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      org.scalatest.Assertions.assume(s1.size == 12, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(s1.size == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      org.scalatest.Assertions.assume(l1.size == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.size == 10, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      org.scalatest.Assertions.assume(!(s1.size == 10), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(s1.size == 12), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(s1, 12) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      org.scalatest.Assertions.assume(!(l1.size == 2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!(l1.size == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSize(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      org.scalatest.Assertions.assume(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(floatLengthSize.size == 1.0f, ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      org.scalatest.Assertions.assume(l1.exists(_ == 3), ", dude")
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      org.scalatest.Assertions.assume(l1.exists(3 == _), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(_ == 5), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(5 == _), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      org.scalatest.Assertions.assume(!l1.exists(_ == 5), ", dude")
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      org.scalatest.Assertions.assume(!l1.exists(5 == _), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.exists(_ == 3), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(!l1.exists(3 == _), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(contained(l1, 3) + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$42: scala.Int) => _$42.>(3)))" else "l1.exists(((x$43: Int) => x$43.>(3)))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l1.exists(3 < _), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$43: scala.Int) => 3.<(_$43)))" else "l1.exists(((x$44: Int) => 3.<(x$44)))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$44: java.lang.String) => _$44.isEmpty()))" else "l3.exists(((x$45: String) => x$45.isEmpty()))") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(ci1.exists(321), ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(wasFalse("ci1.exists(321)") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      org.scalatest.Assertions.assume(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestCanceledException] {
        org.scalatest.Assertions.assume(woof { meow(y = 5) } == "meow", ", dude")
      }
      org.scalatest.Assertions.assert(e.message == Some(didNotEqual("woof", "meow") + ", dude"))
      org.scalatest.Assertions.assert(e.failedCodeFileName == (Some(fileName)))
      org.scalatest.Assertions.assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assume(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |_root_.org.scalatest.Assertions.assume(org === "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    _root_.org.scalatest.Assertions.assume(org === "test", ", dude")
          |  }
          |}
        """.stripMargin)
    }

    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          |class Test {
          |  def aCustomMethod: Boolean = true
          |}
          |val org = new Test
          |_root_.org.scalatest.Assertions.assume(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |_root_.org.scalatest.Assertions.assume(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |_root_.org.scalatest.Assertions.assume(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |_root_.org.scalatest.Assertions.assume(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |_root_.org.scalatest.Assertions.assume(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(assume(x + 1 == 2, "clue") eq Succeeded)
    }
  }

  describe("assertTypeError method ") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        org.scalatest.Assertions.assertTypeError("val a: String = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertTypeError("val a = 1")
        }
        org.scalatest.Assertions.assert(e.message == Some(Resources.expectedTypeErrorButGotNone("val a = 1")))
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertTypeError("println(\"test)")
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("println(\"test)") >= 0)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        org.scalatest.Assertions.assertTypeError(
          """
            |val a: String = 2
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertTypeError(
            """
              |val a = 1
              |""".stripMargin
          )
        }
        org.scalatest.Assertions.assert(e.message == Some(Resources.expectedTypeErrorButGotNone("" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed ") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertTypeError(
            """
              |println("test)
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("println(\"test)") >= 0, "error message was: " + e.message.get)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      org.scalatest.Assertions.assert(assertTypeError("val x: String = 1") eq Succeeded)
    }
  }

  describe("assertDoesNotCompile method ") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        org.scalatest.Assertions.assertDoesNotCompile("val a: String = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when parse and type check passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertDoesNotCompile("val a = 1")
        }
        org.scalatest.Assertions.assert(e.message == Some(Resources.expectedCompileErrorButGotNone("val a = 1")))
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should do nothing when parse failed") {
        org.scalatest.Assertions.assertDoesNotCompile("println(\"test)")
      }

      it("should result in type Assertion and, on success, return the Succeeded value") {
        org.scalatest.Assertions.assert(org.scalatest.Assertions.assertDoesNotCompile("val x: String = 1") eq Succeeded)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        org.scalatest.Assertions.assertDoesNotCompile("val i: Int = null")
      }

    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        org.scalatest.Assertions.assertDoesNotCompile(
          """
            |val a: String = 2
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when parse and type check passed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertDoesNotCompile(
            """
              |val a = 1
              |""".stripMargin
          )
        }
        org.scalatest.Assertions.assert(e.message == Some(Resources.expectedCompileErrorButGotNone("" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }

      it("should do nothing when parse failed ") {
        org.scalatest.Assertions.assertDoesNotCompile(
          """
            |println(\"test)
            |""".stripMargin
        )
      }

      it("should result in type Assertion and, on success, return the Succeeded value") {
        org.scalatest.Assertions.assert(org.scalatest.Assertions.assertDoesNotCompile(
          """
            |val x: String = 1
            |""".stripMargin) eq Succeeded)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        org.scalatest.Assertions.assertDoesNotCompile(
          """
            |val i: Int = null
            |""".stripMargin
        )
      }
    }
  }

  describe("assertCompiles method") {

    describe("when work with string literal") {

      it("should do nothing when type check passed") {
        org.scalatest.Assertions.assertCompiles("val a = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertCompiles("val a: String = 2")
        }
        val errMsg = Resources.expectedNoErrorButGotTypeError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("val a: String = 2") >= 0)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertCompiles("println(\"test)")
        }
        val errMsg = Resources.expectedNoErrorButGotParseError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("println(\"test)") >= 0)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check passed") {
        org.scalatest.Assertions.assertCompiles(
          """
            |val a = 1
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertCompiles(
            """
              |val a: String = 2
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedNoErrorButGotTypeError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("" + Prettifier.lineSeparator + "val a: String = 2" + Prettifier.lineSeparator + "") >= 0)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          org.scalatest.Assertions.assertCompiles(
            """
              |println("test)
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedNoErrorButGotParseError("", "")
        org.scalatest.Assertions.assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        org.scalatest.Assertions.assert(e.message.get.indexOf("println(\"test)") >= 0)
        org.scalatest.Assertions.assert(e.failedCodeFileName === (Some(fileName)))
        org.scalatest.Assertions.assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      org.scalatest.Assertions.assert(org.scalatest.Assertions.assertCompiles("val x: Int = 1") eq Succeeded)
    }
  }

  describe("The assertResult method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      org.scalatest.Assertions.assertResult(npe.getMessage) { null }
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1) { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1) { a3 }
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1) { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1) { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1) { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1) { a3 }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a3) { a1 }
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      org.scalatest.Assertions.assertResult(n1) { n2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(n1) { "hi" }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult("hi") { n1 }
      }
      val a1 = Array(1, 2, 3)
      val aNull: Array[Int] = null
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(aNull) { a1 }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1) { aNull }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a) { null }
      }
      org.scalatest.Assertions.assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, a, null)))
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(assertResult(2) { x + 1 } eq Succeeded)
    }
  }

  describe("The assertResult method that 'gets a clue'") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      org.scalatest.Assertions.assertResult(npe.getMessage, "a clue") { null }
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1, "a clue") { a3 }
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1, "a clue") { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      org.scalatest.Assertions.assert(a1 ne a2)
      org.scalatest.Assertions.assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1, "a clue") { a3 }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a3, "a clue") { a1 }
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assertResult(n1, "a clue") { n2 }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(n1, "a clue") { "hi" }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult("hi", "a clue") { n1 }
      }
      val a1 = Array(1, 2, 3)
      val aNull: Array[Int] = null
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(aNull, "a clue") { a1 }
      }
      intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a1, "a clue") { aNull }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a, "a clue") { null }
      }
      org.scalatest.Assertions.assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, a, null) + " a clue"))
    }
    it("should append clues in a satisfying manner") {
      val a = "hi"
      val b = "ho"
      val aDiff = "h[i]"
      val bDiff = "h[o]"
      val e1 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a, "the clue") { b }
      }
      org.scalatest.Assertions.assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + " the clue"))

      val e2 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a, ", the clue") { b }
      }
      org.scalatest.Assertions.assert(e2.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + ", the clue"))

      val e3 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a, ". the clue") { b }
      }
      org.scalatest.Assertions.assert(e3.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + ". the clue"))

      val e4 = intercept[TestFailedException] {
        org.scalatest.Assertions.assertResult(a, "; the clue") { b }
      }
      org.scalatest.Assertions.assert(e4.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + "; the clue"))
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      org.scalatest.Assertions.assert(assertResult(2, "clue") { x + 1 } eq Succeeded)
    }
  }
}
