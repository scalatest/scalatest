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

class AssertionsSpec extends AnyFunSpec {

  val fileName: String = "AssertionsSpec.scala"

  private val prettifier = Prettifier.default

  describe("The === method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      assert(npe.getMessage === null)
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
      intercept[TestFailedException] {
        assert(a3 === a1)
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assert(n1 === n2)
      intercept[TestFailedException] {
        assert(n1 === "hi")
      }
      intercept[TestFailedException] {
        assert("hi" === n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        assert(n1 === a1)
      }
      intercept[TestFailedException] {
        assert(a1 === n1)
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        assert(a === null)
      }
      assert(e1.message === Some(FailureMessages.didNotEqual(prettifier, a, null)))
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
      assert(result eq e)
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
        assert(caught.cause.value eq wrongException)
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
      assert(result eq Succeeded)
    }

    it("should throw TFE if no exception is thrown") {
      val caught =
        intercept[TestFailedException] {
          assertThrows[Exception] { "hi" }
        }
      assert(caught.isInstanceOf[TestFailedException])
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
        assert(caught.cause.value eq wrongException)
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

  def prettify(value: Any): String = Prettifier.default.apply(value)

  def didNotStartWith(left: Any, right: Any): String =
    prettify(left) + " did not start with " + prettify(right)

  def startedWith(left: Any, right: Any): String =
    prettify(left) + " started with " + prettify(right)

  def didNotEndWith(left: Any, right: Any): String =
    prettify(left) + " did not end with " + prettify(right)

  def endedWith(left: Any, right: Any): String =
    prettify(left) + " ended with " + prettify(right)

  def didNotContain(left: Any, right: Any): String =
    prettify(left) + " did not contain " + prettify(right)

  def contained(left: Any, right: Any): String =
    prettify(left) + " contained " + prettify(right)

  def didNotContainKey(left: Any, right: Any): String =
    prettify(left) + " did not contain key " + prettify(right)

  def containedKey(left: Any, right: Any): String =
    prettify(left) + " contained key " + prettify(right)

  def wasNotTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    prettify(left) + " was not the same instance as " + prettify(right)

  def wasTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    prettify(left) + " was the same instance as " + prettify(right)

  def wasNotEmpty(left: Any): String =
    prettify(left) + " was not empty"

  def wasEmpty(left: Any): String =
    prettify(left) + " was empty"

  def wasNotInstanceOf(left: Any, className: String) =
    prettify(left) + " was not instance of " + className

  def wasInstanceOf(left: Any, className: String) =
    prettify(left) + " was instance of " + className

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

  describe("The assert(boolean) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should do nothing when is used to check a == 3") {
      assert(a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 5)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      assert(5 == b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] {
        assert(3 == b)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      assert(a != 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] {
        assert(a != 3)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      assert(3 != b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] {
        assert(5 != b)
      }
      assert(e.message === Some(equaled(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      assert(3 == 3)
    }

    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestFailedException] {
        assert(3 == 5)
      }
      assert(e1.message === None)
      assert(e1.failedCodeFileName === (Some(fileName)))
      assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(3 == 5, "3 did not equal 5")
      }
      assert(e2.message === Some("3 did not equal 5"))
      assert(e2.failedCodeFileName === (Some(fileName)))
      assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] {
        assert(a == b)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }


    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val s = "test"
      val e = intercept[TestFailedException] {
        assert(s == null)
      }
      assert(e.message === Some(didNotEqual("test", null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val s = "test"
      val e = intercept[TestFailedException] {
        assert(null == s)
      }
      assert(e.message === Some(didNotEqual(null, "test")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] {
        assert(3 != a)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      assert(5 != a)
    }

    it("should do nothing when is used to check a > 2") {
      assert(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      assert(5 > a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] {
        assert(a > 3)
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] {
        assert(3 > a)
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      assert(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      assert(3 >= a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] {
        assert(a >= 4)
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] {
        assert(2 >= a)
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      assert(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      assert(3 < b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] {
        assert(b < 5)
      }
      assert(e.message === Some(wasNotLessThan(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] {
        assert(5 < b)
      }
      assert(e.message === Some(wasNotLessThan(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      assert(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      assert(5 <= b)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] {
        assert(b <= 4)
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] {
        assert(6 <= b)
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestFailedException] {
        assert(first < second)
      }
      assert(e.message === Some(wasNotLessThan(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used in another function to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps

      @scala.annotation.tailrec
      def assertInOrder2[A : Ordering](list: List[A]): Assertion =
        list match {
          case first :: (more @ second :: _) =>
            assert(first <= second)
            assertInOrder2[A](more)
          case _ => Succeeded
        }

      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestFailedException] {
        assertInOrder2(List(first, second))
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 12)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      assert(bob == "bob")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      assert(bob != "alice")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      assert(alice == "alice")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      assert(alice != "bob")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(bob == "alice")
      }
      assert(e.message === Some(didNotEqual(bob, "alice")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(bob != "bob")
      }
      assert(e.message === Some(equaled(bob, "bob")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(alice == "bob")
      }
      assert(e.message === Some(didNotEqual(alice, "bob")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(alice != "alice")
      }
      assert(e.message === Some(equaled(alice, "alice")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a === 3") {
      assert(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestFailedException] {
        assert(a === 5)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      assert(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestFailedException] {
        assert(5 === a)
      }
      assert(e.message === Some(didNotEqual(5, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      assert(a !== 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestFailedException] {
        assert(a !== 3)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      assert(5 !== a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestFailedException] {
        assert(3 !== a)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      assert(a == 3 && b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && b == 6)
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 5)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 6)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      assert(a == 3 & b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 & b == 6)
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 5)
      }
      assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      assert(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      assert(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      assert(a == 2 || b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 || b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      assert(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      assert(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      assert(a == 2 | b == 5)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 | b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      assert(a == 3 && (b == 5 && b > 3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && (b == 5 && b > 5))
      }
      assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5)))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      assert(!(a == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestFailedException] {
        assert(!(a == 3))
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && !(b == 5))
      }
      assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      assert((a == 3) == (b == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestFailedException] {
        assert((a == 3) == (b != 5))
      }
      assert(e.message === Some(didNotEqual(true, false)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 & s.changeState)
      }
      assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assert(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      assert(a == 3 | s.changeState)
      assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assert(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && { println("hi"); b == 3})
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else // for dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      assert({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        assert({ println("hi"); b == 5} && a == 5)
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      assert(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      assert(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      assert(neverRuns3(sys.error("Sad times 3"))(0))
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
      assert(s1 startsWith "hi")
      assert(s1.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s2 startsWith "hi")
      }
      assert(e1.message == Some(didNotStartWith(s2, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s2.startsWith("hi"))
      }
      assert(e2.message == Some(didNotStartWith(s2, "hi")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      assert(ci1 startsWith 1)
      assert(ci1.startsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci2 startsWith 1)
      }
      assert(e1.message == Some(didNotStartWith(ci2, 1)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci2.startsWith(1))
      }
      assert(e2.message == Some(didNotStartWith(ci2, 1)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      assert(!s2.startsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s1.startsWith("hi"))
      }
      assert(e1.message == Some(startedWith(s1, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      assert(s2 endsWith "hi")
      assert(s2.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s1 endsWith "hi")
      }
      assert(e1.message == Some(didNotEndWith(s1, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s1.endsWith("hi"))
      }
      assert(e2.message == Some(didNotEndWith(s1, "hi")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      assert(ci2 endsWith 1)
      assert(ci2.endsWith(1))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 endsWith 1)
      }
      assert(e1.message == Some(didNotEndWith(ci1, 1)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.endsWith(1))
      }
      assert(e2.message == Some(didNotEndWith(ci1, 1)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      assert(!s1.endsWith("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s2.endsWith("hi"))
      }
      assert(e1.message == Some(endedWith(s2, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      assert(s3 contains "hi")
      assert(s3.contains("hi"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestFailedException] {
        assert(s3 contains "hello")
      }
      assert(e1.message == Some(didNotContain(s3, "hello")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s3.contains("hello"))
      }
      assert(e2.message == Some(didNotContain(s3, "hello")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      assert(ci2 contains 2)
      assert(ci2.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 contains 5)
      }
      assert(e1.message == Some(didNotContain(ci1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.contains(5))
      }
      assert(e2.message == Some(didNotContain(ci1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      assert(!s3.contains("hello"))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s3.contains("hi"))
      }
      assert(e1.message == Some(contained(s3, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      assert(l1 contains 2)
      assert(l1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(l1 contains 5)
      }
      assert(e1.message == Some(didNotContain(l1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(l1.contains(5))
      }
      assert(e2.message == Some(didNotContain(l1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      assert(!(l1 contains 5))
      assert(!l1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(l1 contains 2))
      }
      assert(e1.message == Some(contained(l1, 2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(!l1.contains(2))
      }
      assert(e2.message == Some(contained(l1, 2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      assert(m1 contains 2)
      assert(m1.contains(2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(m1 contains 5)
      }
      assert(e1.message == Some(didNotContainKey(m1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(m1.contains(5))
      }
      assert(e2.message == Some(didNotContainKey(m1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      assert(!(m1 contains 5))
      assert(!m1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(m1 contains 2))
      }
      assert(e1.message == Some(containedKey(m1, 2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(!m1.contains(2))
      }
      assert(e2.message == Some(containedKey(m1, 2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      assert(ct1 contains 8)
      assert(ct1.contains(8))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ct1 contains 5)
      }
      assert(e1.message == Some(didNotContain(ct1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ct1.contains(5))
      }
      assert(e2.message == Some(didNotContain(ct1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      assert(!ct1.contains(5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestFailedException] {
        assert(!ct1.contains(8))
      }
      assert(e1.message == Some(contained(ct1, 8)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      assert(ci1 eq ci3)
      assert(ci1.eq(ci3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 eq ci2)
      }
      assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.eq(ci2))
      }
      assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      assert(!ci1.eq(ci2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.eq(ci3))
      }
      assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      assert(ci1 ne ci2)
      assert(ci1.ne(ci2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 ne ci3)
      }
      assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.ne(ci3))
      }
      assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      assert(!ci1.ne(ci3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.ne(ci2))
      }
      assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      assert(s4.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(s3.isEmpty)
      }
      assert(e.message == Some(wasNotEmpty(s3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      assert(!s3.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!s4.isEmpty)
      }
      assert(e.message == Some(wasEmpty(s4)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      assert(l2.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(l1.isEmpty)
      }
      assert(e.message == Some(wasNotEmpty(l1)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      assert(!l1.isEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!l2.isEmpty)
      }
      assert(e.message == Some(wasEmpty(l2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      assert(s3.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s4.nonEmpty ") {
      val e = intercept[TestFailedException] {
        assert(s4.nonEmpty)
      }
      assert(e.message == Some(wasEmpty(s4)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      assert(!s4.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(!s3.nonEmpty)
      }
      assert(e.message == Some(wasNotEmpty(s3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      assert(l1.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(l2.nonEmpty)
      }
      assert(e.message == Some(wasEmpty(l2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.isEmpty") {
      assert(!l2.nonEmpty)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(!l1.nonEmpty)
      }
      assert(e.message == Some(wasNotEmpty(l1)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      assert(s1.isInstanceOf[String])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[String])
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      assert(l1.isInstanceOf[List[Int]])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(s1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      assert(date.isInstanceOf[Date])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[Date])
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      assert(!l1.isInstanceOf[String])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(!s1.isInstanceOf[String])
      }
      assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      assert(!s1.isInstanceOf[List[Int]])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(!l1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      assert(!l1.isInstanceOf[Date])
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(!date.isInstanceOf[Date])
      }
      assert(e.message == Some(wasInstanceOf(date, "java.util.Date")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      assert(s1.length == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.length == 10)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      assert(l1.length == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.length == 10)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      assert(!(s1.length == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.length == 12))
      }
      assert(e.message == Some(hadLength(s1, 12)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      assert(!(l1.length == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.length == 3))
      }
      assert(e.message == Some(hadLength(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      assert(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestFailedException] {
        assert(floatLengthSize.length == 1.0f)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      assert(s1.size == 12)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.size == 10)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      assert(l1.size == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.size == 10)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      assert(!(s1.size == 10))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.size == 12))
      }
      assert(e.message == Some(hadSize(s1, 12)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      assert(!(l1.size == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.size == 3))
      }
      assert(e.message == Some(hadSize(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      assert(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestFailedException] {
        assert(floatLengthSize.size == 1.0f)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assert(l1.exists(_ == 3))
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      assert(l1.exists(3 == _))
    }


    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ == 5))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(5 == _))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assert(!l1.exists(_ == 5))
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      assert(!l1.exists(5 == _))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(_ == 3))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(3 == _))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$9: scala.Int) => _$9.>(3)))" else "l1.exists(((x$10: Int) => x$10.>(3)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(3 < _))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$10: scala.Int) => 3.<(_$10)))" else "l1.exists(((x$11: Int) => 3.<(x$11)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        assert(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$11: java.lang.String) => _$11.isEmpty()))" else "l3.exists(((x$12: String) => x$12.isEmpty()))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestFailedException] {
        assert(ci1.exists(321))
      }
      assert(e.message == Some(wasFalse("ci1.exists(321)")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      assert(woof { meow(y = 5) } == "woof")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestFailedException] {
        assert(woof { meow(y = 5) } == "meow")
      }
      assert(e.message == Some(didNotEqual("woof", "meow")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
        val org = "test"
        assert(org == "test")
        """)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          val org = "test"
          assert(org === "test")
        """)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            it("testing here") {
              val org = "test"
              assert(org === "test")
            }
          }
        """)
    }
    
    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          class Test {
            def aCustomMethod: Boolean = true
          }
          val org = new Test
          assert(org.aCustomMethod)
        """)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          val org = false
          assert(!org)
        """)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.isEmpty)
        """)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.isInstanceOf[String])
        """)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          val org = Array.empty[String]
          assert(org.size == 0)
        """)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.length == 0)
        """)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          val org = "abc"
          assert(org.exists(_ == 'b'))
        """)
    }

    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assert(x + 1 == 2) eq Succeeded)
    }

  }

  describe("The assert(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should throw NullArgumentException when null is passed in as clue") {
      val e = intercept[NullArgumentException] {
        assert(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      assert(a == 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 5, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      assert(5 == b, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestFailedException] {
        assert(3 == b, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      assert(a != 5, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestFailedException] {
        assert(a != 3, ". dude")
      }
      assert(e.message === Some(equaled(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      assert(3 != b, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestFailedException] {
        assert(5 != b, "; dude")
      }
      assert(e.message === Some(equaled(5, 5) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      assert(3 == 3, "dude")
    }

    it("should throw TestFailedException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestFailedException] {
        assert(3 == 5, "dude")
      }
      assert(e1.message === Some("dude"))
      assert(e1.failedCodeFileName === (Some(fileName)))
      assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(3 == 5, "3 did not equal 5")
      }
      assert(e2.message === Some("3 did not equal 5"))
      assert(e2.failedCodeFileName === (Some(fileName)))
      assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestFailedException] {
        assert(a == b, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val s = "test"
      val e = intercept[TestFailedException] {
        assert(s == null, ". dude")
      }
      assert(e.message === Some(didNotEqual("test", null) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val s = "test"
      val e = intercept[TestFailedException] {
        assert(null == s, "; dude")
      }
      assert(e.message === Some(didNotEqual(null, "test") + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestFailedException] {
        assert(3 != a, ", dude")
      }
      assert(e.message === Some(equaled(3, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      assert(5 != a, ". dude")
    }

    it("should do nothing when is used to check a > 2") {
      assert(a > 2, ". dude")
    }

    it("should do nothing when is used to check 5 > a") {
      assert(5 > a, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestFailedException] {
        assert(a > 3, ". dude")
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestFailedException] {
        assert(3 > a, "; dude")
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      assert(a >= 3, ", dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      assert(3 >= a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestFailedException] {
        assert(a >= 4, ", dude")
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestFailedException] {
        assert(2 >= a, ". dude")
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      assert(b < 6, "; dude")
    }

    it("should do nothing when is used to check 3 < b") {
      assert(3 < b, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestFailedException] {
        assert(b < 5, "; dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 5) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestFailedException] {
        assert(5 < b, ", dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 5) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      assert(b <= 5, ". dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      assert(5 <= b, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestFailedException] {
        assert(b <= 4, ". dude")
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestFailedException] {
        assert(6 <= b, "; dude")
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestFailedException] {
        assert(first < second, "; dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 4) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used in another function to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps

      @scala.annotation.tailrec
      def assertInOrder2[A : Ordering](list: List[A]): Assertion =
        list match {
          case first :: (more @ second :: _) =>
            assert(first <= second, "; dude")
            assertInOrder2[A](more)
          case _ => Succeeded
        }

      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestFailedException] {
        assertInOrder2(List(first, second))
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 12)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      assert(bob == "bob", "dude")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      assert(bob != "alice", "dude")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      assert(alice == "alice", "dude")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      assert(alice != "bob", "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(bob == "alice", "dude")
      }
      assert(e.message === Some(didNotEqual(bob, "alice") + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(bob != "bob", ", dude")
      }
      assert(e.message === Some(equaled(bob, "bob") + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestFailedException] {
        assert(alice == "bob", ". dude")
      }
      assert(e.message === Some(didNotEqual(alice, "bob") + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestFailedException] {
        assert(alice != "alice", "; dude")
      }
      assert(e.message === Some(equaled(alice, "alice") + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a === 3") {
      assert(a === 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestFailedException] {
        assert(a === 5, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      assert(3 === a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestFailedException] {
        assert(5 === a, ", dude")
      }
      assert(e.message === Some(didNotEqual(5, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      assert(a !== 5, ". dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestFailedException] {
        assert(a !== 3, ". dude")
      }
      assert(e.message === Some(equaled(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      assert(5 !== a, "; dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestFailedException] {
        assert(3 !== a, "; dude")
      }
      assert(e.message === Some(equaled(3, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      assert(a == 3 && b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && b == 6, ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 5, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 && b == 6, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      assert(a == 3 & b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 3 & b == 6, ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 5, ", dude")
      }
      assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      assert(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      assert(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      assert(a == 2 || b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 || b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      assert(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      assert(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      assert(a == 2 | b == 5, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 | b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      assert(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      assert(!(a == 5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestFailedException] {
        assert(!(a == 3), ", dude")
      }
      assert(e.message === Some(equaled(3, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && !(b == 5), ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      assert((a == 3) == (b == 5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestFailedException] {
        assert((a == 3) == (b != 5), ", dude")
      }
      assert(e.message === Some(didNotEqual(true, false) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assert(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      assert(a == 3 | s.changeState, ", dude")
      assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assert(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      assert({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        assert({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      assert(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      assert(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      assert(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
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
      assert(s1 startsWith "hi", ", dude")
      assert(s1.startsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s2 startsWith "hi", ", dude")
      }
      assert(e1.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s2.startsWith("hi"), ", dude")
      }
      assert(e2.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      assert(ci1 startsWith 1, ", dude")
      assert(ci1.startsWith(1), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci2 startsWith 1, ", dude")
      }
      assert(e1.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci2.startsWith(1), ", dude")
      }
      assert(e2.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      assert(!s2.startsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.message == Some(startedWith(s1, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      assert(s2 endsWith "hi", ", dude")
      assert(s2.endsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestFailedException] {
        assert(s1 endsWith "hi", ", dude")
      }
      assert(e1.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s1.endsWith("hi"), ", dude")
      }
      assert(e2.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      assert(ci2 endsWith 1, ", dude")
      assert(ci2.endsWith(1), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 endsWith 1, ", dude")
      }
      assert(e1.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.endsWith(1), ", dude")
      }
      assert(e2.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      assert(!s1.endsWith("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.message == Some(endedWith(s2, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      assert(s3 contains "hi", ", dude")
      assert(s3.contains("hi"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestFailedException] {
        assert(s3 contains "hello", ", dude")
      }
      assert(e1.message == Some(didNotContain(s3, "hello") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(s3.contains("hello"), ", dude")
      }
      assert(e2.message == Some(didNotContain(s3, "hello") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      assert(ci2 contains 2, ", dude")
      assert(ci2.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(ci1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(ci1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      assert(!s3.contains("hello"), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestFailedException] {
        assert(!s3.contains("hi"), ", dude")
      }
      assert(e1.message == Some(contained(s3, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      assert(l1 contains 2, ", dude")
      assert(l1.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(l1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(l1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      assert(!(l1 contains 5), ", dude")
      assert(!l1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(l1 contains 2), ", dude")
      }
      assert(e1.message == Some(contained(l1, 2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(!l1.contains(2), ", dude")
      }
      assert(e2.message == Some(contained(l1, 2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      assert(m1 contains 2, ", dude")
      assert(m1.contains(2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(m1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContainKey(m1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(m1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContainKey(m1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      assert(!(m1 contains 5), ", dude")
      assert(!m1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestFailedException] {
        assert(!(m1 contains 2), ", dude")
      }
      assert(e1.message == Some(containedKey(m1, 2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(!m1.contains(2), ", dude")
      }
      assert(e2.message == Some(containedKey(m1, 2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      assert(ct1 contains 8, ", dude")
      assert(ct1.contains(8), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestFailedException] {
        assert(ct1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(ct1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ct1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(ct1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      assert(!ct1.contains(5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestFailedException] {
        assert(!ct1.contains(8), ", dude")
      }
      assert(e1.message == Some(contained(ct1, 8) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      assert(ci1 eq ci3, ", dude")
      assert(ci1.eq(ci3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 eq ci2, ", dude")
      }
      assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.eq(ci2), ", dude")
      }
      assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      assert(!ci1.eq(ci2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.eq(ci3), ", dude")
      }
      assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      assert(ci1 ne ci2, ", dude")
      assert(ci1.ne(ci2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestFailedException] {
        assert(ci1 ne ci3, ", dude")
      }
      assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestFailedException] {
        assert(ci1.ne(ci3), ", dude")
      }
      assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      assert(!ci1.ne(ci3), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestFailedException] {
        assert(!ci1.ne(ci2), ", dude")
      }
      assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      assert(s4.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(s3.isEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      assert(!s3.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!s4.isEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(s4) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      assert(l2.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(l1.isEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      assert(!l1.isEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestFailedException] {
        assert(!l2.isEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(l2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      assert(s3.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(s4.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(s4) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      assert(!s4.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(!s3.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      assert(l1.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(l2.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(l2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      assert(!l2.nonEmpty, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestFailedException] {
        assert(!l1.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      assert(s1.isInstanceOf[String], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[String], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      assert(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      assert(date.isInstanceOf[Date], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      assert(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestFailedException] {
        assert(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      assert(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestFailedException] {
        assert(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      assert(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestFailedException] {
        assert(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(date, "java.util.Date") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      assert(s1.length == 12, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.length == 10, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      assert(l1.length == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.length == 10, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      assert(!(s1.length == 10), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.length == 12), ", dude")
      }
      assert(e.message == Some(hadLength(s1, 12) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      assert(!(l1.length == 2), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.length == 3), ", dude")
      }
      assert(e.message == Some(hadLength(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      assert(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestFailedException] {
        assert(floatLengthSize.length == 1.0f, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      assert(s1.size == 12, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(s1.size == 10, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      assert(l1.size == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestFailedException] {
        assert(l1.size == 10, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      assert(!(s1.size == 10), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(s1.size == 12), ", dude")
      }
      assert(e.message == Some(hadSize(s1, 12) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      assert(!(l1.size == 2), ", dude")
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      assert(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestFailedException] {
        assert(floatLengthSize.size == 1.0f, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.size == 3), ", dude")
      }
      assert(e.message == Some(hadSize(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assert(l1.exists(_ == 3), ", dude")
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      assert(l1.exists(3 == _), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ == 5), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(5 == _), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assert(!l1.exists(_ == 5), ", dude")
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      assert(!l1.exists(5 == _), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(_ == 3), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(3 == _), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$20: scala.Int) => _$20.>(3)))" else "l1.exists(((x$21: Int) => x$21.>(3)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(3 < _), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$21: scala.Int) => 3.<(_$21)))" else "l1.exists(((x$22: Int) => 3.<(x$22)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        assert(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$22: java.lang.String) => _$22.isEmpty()))" else "l3.exists(((x$23: String) => x$23.isEmpty()))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestFailedException] {
        assert(ci1.exists(321), ", dude")
      }
      assert(e.message == Some(wasFalse("ci1.exists(321)") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      assert(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestFailedException] {
        assert(woof { meow(y = 5) } == "meow", ", dude")
      }
      assert(e.message == Some(didNotEqual("woof", "meow") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          val org = "test"
          assert(org == "test", ", dude")
        """)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          val org = "test"
          assert(org === "test", ", dude")
        """)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
            it("testing here") {
              val org = "test"
              assert(org === "test", ", dude")
            }
          }
        """)
    }
    
    it("should compile when used with org.aCustomMethod that shadow org.scalactic") {
      assertCompiles(
        """
          class Test {
            def aCustomMethod: Boolean = true
          }
          val org = new Test
          assert(org.aCustomMethod, ", dude")
        """)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          val org = false
          assert(!org, ", dude")
        """)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.isEmpty, ", dude")
        """)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.isInstanceOf[String], ", dude")
        """)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          val org = Array.empty[String]
          assert(org.size == 0, ", dude")
        """)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          val org = ""
          assert(org.length == 0, ", dude")
        """)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          val org = "abc"
          assert(org.exists(_ == 'b'), ", dude")
        """)
    }

    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assert(x + 1 == 2, "clue") eq Succeeded)
    }
  }

  describe("The assume(boolean) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should do nothing when is used to check a == 3") {
      assume(a == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 5)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      assume(5 == b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestCanceledException] {
        assume(3 == b)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      assume(a != 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestCanceledException] {
        assume(a != 3)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      assume(3 != b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestCanceledException] {
        assume(5 != b)
      }
      assert(e.message === Some(equaled(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      assume(3 == 3)
    }

    it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestCanceledException] {
        assume(3 == 5)
      }
      assert(e1.message === None)
      assert(e1.failedCodeFileName === (Some(fileName)))
      assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(3 == 5, "3 did not equal 5")
      }
      assert(e2.message === Some("3 did not equal 5"))
      assert(e2.failedCodeFileName === (Some(fileName)))
      assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestCanceledException] {
        assume(a == b)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
      val s = "test"
      val e = intercept[TestCanceledException] {
        assume(s == null)
      }
      assert(e.message === Some(didNotEqual("test", null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val s = "test"
      val e = intercept[TestCanceledException] {
        assume(null == s)
      }
      assert(e.message === Some(didNotEqual(null, "test")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestCanceledException] {
        assume(3 != a)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      assume(5 != a)
    }

    it("should do nothing when is used to check a > 2") {
      assume(a > 2)
    }

    it("should do nothing when is used to check 5 > a") {
      assume(5 > a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestCanceledException] {
        assume(a > 3)
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestCanceledException] {
        assume(3 > a)
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      assume(a >= 3)
    }

    it("should do nothing when is used to check 3 >= a") {
      assume(3 >= a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestCanceledException] {
        assume(a >= 4)
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestCanceledException] {
        assume(2 >= a)
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      assume(b < 6)
    }

    it("should do nothing when is used to check 3 < b") {
      assume(3 < b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestCanceledException] {
        assume(b < 5)
      }
      assert(e.message === Some(wasNotLessThan(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestCanceledException] {
        assume(5 < b)
      }
      assert(e.message === Some(wasNotLessThan(5, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      assume(b <= 5)
    }

    it("should do nothing when is used to check 5 <= b") {
      assume(5 <= b)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestCanceledException] {
        assume(b <= 4)
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestCanceledException] {
        assume(6 <= b)
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should  throw TestCanceledException with correct message and stack depth when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestCanceledException] {
        assume(first < second)
      }
      assert(e.message === Some(wasNotLessThan(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used in another function to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps

      @scala.annotation.tailrec
      def assertInOrder2[A : Ordering](list: List[A]): Assertion =
        list match {
          case first :: (more @ second :: _) =>
            assume(first <= second)
            assertInOrder2[A](more)
          case _ => Succeeded
        }

      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestCanceledException] {
        assertInOrder2(List(first, second))
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 12)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      assume(bob == "bob")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      assume(bob != "alice")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      assume(alice == "alice")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      assume(alice != "bob")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestCanceledException] {
        assume(bob == "alice")
      }
      assert(e.message === Some(didNotEqual(bob, "alice")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestCanceledException] {
        assume(bob != "bob")
      }
      assert(e.message === Some(equaled(bob, "bob")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestCanceledException] {
        assume(alice == "bob")
      }
      assert(e.message === Some(didNotEqual(alice, "bob")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestCanceledException] {
        assume(alice != "alice")
      }
      assert(e.message === Some(equaled(alice, "alice")))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      assume(a === 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestCanceledException] {
        assume(a === 5)
      }
      assert(e.message === Some(didNotEqual(3, 5)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      assume(3 === a)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestCanceledException] {
        assume(5 === a)
      }
      assert(e.message === Some(didNotEqual(5, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      assume(a !== 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestCanceledException] {
        assume(a !== 3)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      assume(5 !== a)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestCanceledException] {
        assume(3 !== a)
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      assume(a == 3 && b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && b == 6)
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 && b == 5)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 && b == 6)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      assume(a == 3 & b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 & b == 6)
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 5)
      }
      assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      assume(a == 3 || b == 5)
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      assume(a == 3 || b == 6)
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      assume(a == 2 || b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 || b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      assume(a == 3 | b == 5)
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      assume(a == 3 | b == 6)
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      assume(a == 2 | b == 5)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 | b == 6)
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      assume(a == 3 && (b == 5 && b > 3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && (b == 5 && b > 5))
      }
      assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5)))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      assume(!(a == 5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!(a == 3))
      }
      assert(e.message === Some(equaled(3, 3)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && !(b == 5))
      }
      assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5))))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      assume((a == 3) == (b == 5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestCanceledException] {
        assume((a == 3) == (b != 5))
      }
      assert(e.message === Some(didNotEqual(true, false)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 && s.changeState)
      }
      assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 & s.changeState)
      }
      assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assume(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      assume(a == 3 | s.changeState)
      assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assume(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && { println("hi"); b == 3})
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      assume({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        assume({ println("hi"); b == 5} && a == 5)
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      assume(neverRuns1(sys.error("Sad times 1")))
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      assume(neverRuns2(sys.error("Sad times 2"))(0))
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      assume(neverRuns3(sys.error("Sad times 3"))(0))
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
      assume(s1 startsWith "hi")
      assume(s1.startsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s2 startsWith "hi")
      }
      assert(e1.message == Some(didNotStartWith(s2, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s2.startsWith("hi"))
      }
      assert(e2.message == Some(didNotStartWith(s2, "hi")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      assume(ci1 startsWith 1)
      assume(ci1.startsWith(1))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestCanceledException] {
        assume(ci2 startsWith 1)
      }
      assert(e1.message == Some(didNotStartWith(ci2, 1)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci2.startsWith(1))
      }
      assert(e2.message == Some(didNotStartWith(ci2, 1)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      assume(!s2.startsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s1.startsWith("hi"))
      }
      assert(e1.message == Some(startedWith(s1, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      assume(s2 endsWith "hi")
      assume(s2.endsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s1 endsWith "hi")
      }
      assert(e1.message == Some(didNotEndWith(s1, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s1.endsWith("hi"))
      }
      assert(e2.message == Some(didNotEndWith(s1, "hi")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      assume(ci2 endsWith 1)
      assume(ci2.endsWith(1))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 endsWith 1)
      }
      assert(e1.message == Some(didNotEndWith(ci1, 1)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.endsWith(1))
      }
      assert(e2.message == Some(didNotEndWith(ci1, 1)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      assume(!s1.endsWith("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s2.endsWith("hi"))
      }
      assert(e1.message == Some(endedWith(s2, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      assume(s3 contains "hi")
      assume(s3.contains("hi"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s3 contains "hello")
      }
      assert(e1.message == Some(didNotContain(s3, "hello")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s3.contains("hello"))
      }
      assert(e2.message == Some(didNotContain(s3, "hello")))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      assume(ci2 contains 2)
      assume(ci2.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 contains 5)
      }
      assert(e1.message == Some(didNotContain(ci1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.contains(5))
      }
      assert(e2.message == Some(didNotContain(ci1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      assume(!s3.contains("hello"))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s3.contains("hi"))
      }
      assert(e1.message == Some(contained(s3, "hi")))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      assume(l1 contains 2)
      assume(l1.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(l1 contains 5)
      }
      assert(e1.message == Some(didNotContain(l1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(l1.contains(5))
      }
      assert(e2.message == Some(didNotContain(l1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      assume(!(l1 contains 5))
      assume(!l1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        assume(!(l1 contains 2))
      }
      assert(e1.message == Some(contained(l1, 2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(!l1.contains(2))
      }
      assert(e2.message == Some(contained(l1, 2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      assume(m1 contains 2)
      assume(m1.contains(2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(m1 contains 5)
      }
      assert(e1.message == Some(didNotContainKey(m1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(m1.contains(5))
      }
      assert(e2.message == Some(didNotContainKey(m1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      assume(!(m1 contains 5))
      assume(!m1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        assume(!(m1 contains 2))
      }
      assert(e1.message == Some(containedKey(m1, 2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(!m1.contains(2))
      }
      assert(e2.message == Some(containedKey(m1, 2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      assume(ct1 contains 8)
      assume(ct1.contains(8))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(ct1 contains 5)
      }
      assert(e1.message == Some(didNotContain(ct1, 5)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ct1.contains(5))
      }
      assert(e2.message == Some(didNotContain(ct1, 5)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      assume(!ct1.contains(5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestCanceledException] {
        assume(!ct1.contains(8))
      }
      assert(e1.message == Some(contained(ct1, 8)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      assume(ci1 eq ci3)
      assume(ci1.eq(ci3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 eq ci2)
      }
      assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.eq(ci2))
      }
      assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      assume(!ci1.eq(ci2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestCanceledException] {
        assume(!ci1.eq(ci3))
      }
      assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      assume(ci1 ne ci2)
      assume(ci1.ne(ci2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 ne ci3)
      }
      assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.ne(ci3))
      }
      assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3)))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      assume(!ci1.ne(ci3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestCanceledException] {
        assume(!ci1.ne(ci2))
      }
      assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      assume(s4.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(s3.isEmpty)
      }
      assert(e.message == Some(wasNotEmpty(s3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      assume(!s3.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!s4.isEmpty)
      }
      assert(e.message == Some(wasEmpty(s4)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      assume(l2.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(l1.isEmpty)
      }
      assert(e.message == Some(wasNotEmpty(l1)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      assume(!l1.isEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!l2.isEmpty)
      }
      assert(e.message == Some(wasEmpty(l2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      assume(s3.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(s4.nonEmpty)
      }
      assert(e.message == Some(wasEmpty(s4)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      assume(!s4.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!s3.nonEmpty)
      }
      assert(e.message == Some(wasNotEmpty(s3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      assume(l1.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(l2.nonEmpty)
      }
      assert(e.message == Some(wasEmpty(l2)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      assume(!l2.nonEmpty)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!l1.nonEmpty)
      }
      assert(e.message == Some(wasNotEmpty(l1)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      assume(s1.isInstanceOf[String])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        assume(l1.isInstanceOf[String])
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      assume(l1.isInstanceOf[List[Int]])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        assume(s1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      assume(date.isInstanceOf[Date])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        assume(l1.isInstanceOf[Date])
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      assume(!l1.isInstanceOf[String])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        assume(!s1.isInstanceOf[String])
      }
      assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      assume(!s1.isInstanceOf[List[Int]])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        assume(!l1.isInstanceOf[List[Int]])
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      assume(!l1.isInstanceOf[Date])
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        assume(!date.isInstanceOf[Date])
      }
      assert(e.message == Some(wasInstanceOf(date, "java.util.Date")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      assume(s1.length == 12)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestCanceledException] {
        assume(s1.length == 10)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      assume(l1.length == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestCanceledException] {
        assume(l1.length == 10)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      assume(!(s1.length == 10))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(s1.length == 12))
      }
      assert(e.message == Some(hadLength(s1, 12)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      assume(!(l1.length == 2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(l1.length == 3))
      }
      assert(e.message == Some(hadLength(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      assume(floatLengthSize.length == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestCanceledException] {
        assume(floatLengthSize.length == 1.0f)
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      assume(s1.size == 12)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestCanceledException] {
        assume(s1.size == 10)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      assume(l1.size == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestCanceledException] {
        assume(l1.size == 10)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      assume(!(s1.size == 10))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(s1.size == 12))
      }
      assert(e.message == Some(hadSize(s1, 12)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      assume(!(l1.size == 2))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(l1.size == 3))
      }
      assert(e.message == Some(hadSize(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      assume(floatLengthSize.size == 2.0f)
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestCanceledException] {
        assume(floatLengthSize.size == 1.0f)
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assume(l1.exists(_ == 3))
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      assume(l1.exists(3 == _))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ == 5))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(5 == _))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assume(!l1.exists(_ == 5))
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      assume(!l1.exists(5 == _))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(_ == 3))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(3 == _))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$31: scala.Int) => _$31.>(3)))" else "l1.exists(((x$32: Int) => x$32.>(3)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(3 < _))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$32: scala.Int) => 3.<(_$32)))" else "l1.exists(((x$33: Int) => 3.<(x$33)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        assume(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$33: java.lang.String) => _$33.isEmpty()))" else "l3.exists(((x$34: String) => x$34.isEmpty()))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestCanceledException] {
        assume(ci1.exists(321))
      }
      assert(e.message == Some(wasFalse("ci1.exists(321)")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      assume(woof { meow(y = 5) } == "woof")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestCanceledException] {
        assume(woof { meow(y = 5) } == "meow")
      }
      assert(e.message == Some(didNotEqual("woof", "meow")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assume(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assume(org === "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    assume(org === "test")
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
          |assume(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |assume(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |assume(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |assume(org.exists(_ == 'b'))
        """.stripMargin)
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assume(x + 1 == 2) eq Succeeded)
    }
  }

  describe("The assume(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

    it("should throw NullArgumentException when null is passed in as clue") {
      val e = intercept[NullArgumentException] {
        assume(a == 3, null)
      }
      assert(e.getMessage == "clue was null")
    }

    it("should do nothing when is used to check a == 3") {
      assume(a == 3, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 5, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 == b") {
      assume(5 == b, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 == b") {
      val e = intercept[TestCanceledException] {
        assume(3 == b, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a != 5") {
      assume(a != 5, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a != 3") {
      val e = intercept[TestCanceledException] {
        assume(a != 3, ". dude")
      }
      assert(e.message === Some(equaled(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 != b") {
      assume(3 != b, "; dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 != b") {
      val e = intercept[TestCanceledException] {
        assume(5 != b, "; dude")
      }
      assert(e.message === Some(equaled(5, 5) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 == 3") {
      assume(3 == 3, "dude")
    }

    it("should throw TestCanceledException with message that contains the original code and correct stack depth when is used to check 3 == 5") {
      // This is because the compiler simply pass the false boolean literal
      // to the macro, can't find a way to get the 3 == 5 literal.
      val e1 = intercept[TestCanceledException] {
        assume(3 == 5, "dude")
      }
      assert(e1.message === Some("dude"))
      assert(e1.failedCodeFileName === (Some(fileName)))
      assert(e1.failedCodeLineNumber === (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(3 == 5, "3 did not equal 5")
      }
      assert(e2.message === Some("3 did not equal 5"))
      assert(e2.failedCodeFileName === (Some(fileName)))
      assert(e2.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == b") {
      val e = intercept[TestCanceledException] {
        assume(a == b, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
      val s = "test"
      val e = intercept[TestCanceledException] {
        assume(s == null, ", dude")
      }
      assert(e.message === Some(didNotEqual("test", null) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val s = "test"
      val e = intercept[TestCanceledException] {
        assume(null == s, ". dude")
      }
      assert(e.message === Some(didNotEqual(null, "test") + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 3 != a") {
      val e = intercept[TestCanceledException] {
        assume(3 != a, "; dude")
      }
      assert(e.message === Some(equaled(3, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 != a") {
      assume(5 != a, "dude")
    }

    it("should do nothing when is used to check a > 2") {
      assume(a > 2, "dude")
    }

    it("should do nothing when is used to check 5 > a") {
      assume(5 > a, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a > 3") {
      val e = intercept[TestCanceledException] {
        assume(a > 3, "dude")
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 > a") {
      val e = intercept[TestCanceledException] {
        assume(3 > a, ", dude")
      }
      assert(e.message === Some(wasNotGreaterThan(3, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a >= 3") {
      assume(a >= 3, ". dude")
    }

    it("should do nothing when is used to check 3 >= a") {
      assume(3 >= a, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a >= 4") {
      val e = intercept[TestCanceledException] {
        assume(a >= 4, ". dude")
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(3, 4) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 2 >= a") {
      val e = intercept[TestCanceledException] {
        assume(2 >= a, "; dude")
      }
      assert(e.message === Some(wasNotGreaterThanOrEqualTo(2, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b < 6") {
      assume(b < 6, "dude")
    }

    it("should do nothing when is used to check 3 < b") {
      assume(3 < b, "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b < 5") {
      val e = intercept[TestCanceledException] {
        assume(b < 5, "dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 5 < b") {
      val e = intercept[TestCanceledException] {
        assume(5 < b, ", dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 5) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check b <= 5") {
      assume(b <= 5, ". dude")
    }

    it("should do nothing when is used to check 5 <= b") {
      assume(5 <= b, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check b <= 4") {
      val e = intercept[TestCanceledException] {
        assume(b <= 4, ". dude")
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 6 <= b") {
      val e = intercept[TestCanceledException] {
        assume(6 <= b, "; dude")
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(6, 5) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should  throw TestCanceledException with correct message and stack depth when is used to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps
      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestCanceledException] {
        assume(first < second, "; dude")
      }
      assert(e.message === Some(wasNotLessThan(5, 4) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used in another function to check java.lang.Integer(5) < java.lang.Integer(4) with import scala.math.Ordering.Implicits.infixOrderingOps in scope") {
      import scala.math.Ordering.Implicits.infixOrderingOps

      @scala.annotation.tailrec
      def assertInOrder2[A : Ordering](list: List[A]): Assertion =
        list match {
          case first :: (more @ second :: _) =>
            assume(first <= second, "; dude")
            assertInOrder2[A](more)
          case _ => Succeeded
        }

      val first = new java.lang.Integer(5)
      val second = new java.lang.Integer(4)
      val e = intercept[TestCanceledException] {
        assertInOrder2(List(first, second))
      }
      assert(e.message === Some(wasNotLessThanOrEqualTo(5, 4) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 12)))
    }

    it("should do nothing when is used to check bob == \"bob\"") {
      assume(bob == "bob", "dude")
    }

    it("should do nothing when is used to check bob != \"alice\"") {
      assume(bob != "alice", "dude")
    }

    it("should do nothing when is used to check alice == \"alice\"") {
      assume(alice == "alice", "dude")
    }

    it("should do nothing when is used to check alice != \"bob\"") {
      assume(alice != "bob", "dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob == \"alice\"") {
      val e = intercept[TestCanceledException] {
        assume(bob == "alice", "dude")
      }
      assert(e.message === Some(didNotEqual(bob, "alice") + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check bob != \"bob\"") {
      val e = intercept[TestCanceledException] {
        assume(bob != "bob", ", dude")
      }
      assert(e.message === Some(equaled(bob, "bob") + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice == \"bob\"") {
      val e = intercept[TestCanceledException] {
        assume(alice == "bob", ". dude")
      }
      assert(e.message === Some(didNotEqual(alice, "bob") + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check alice != \"alice\"") {
      val e = intercept[TestCanceledException] {
        assume(alice != "alice", "; dude")
      }
      assert(e.message === Some(equaled(alice, "alice") + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalactic.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

    it("should do nothing when is used to check a === 3") {
      assume(a === 3, "dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a === 5") {
      val e = intercept[TestCanceledException] {
        assume(a === 5, "dude")
      }
      assert(e.message === Some(didNotEqual(3, 5) + " dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 3 === a") {
      assume(3 === a, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check 5 === a") {
      val e = intercept[TestCanceledException] {
        assume(5 === a, ", dude")
      }
      assert(e.message === Some(didNotEqual(5, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a !== 5") {
      assume(a !== 5, ". dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a !== 3") {
      val e = intercept[TestCanceledException] {
        assume(a !== 3, ". dude")
      }
      assert(e.message === Some(equaled(3, 3) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check 5 !== a") {
      assume(5 !== a, "; dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check 3 !== a") {
      val e = intercept[TestCanceledException] {
        assume(3 !== a, "; dude")
      }
      assert(e.message === Some(equaled(3, 3) + "; dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && b == 5") {
      assume(a == 3 && b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && b == 6, ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 && b == 5, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 && b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 && b == 6, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 & b == 5") {
      assume(a == 3 & b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 & b == 6, ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 5") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 5, ", dude")
      }
      assert(e.message === Some(commaBut(didNotEqual(3, 2), equaled(5, 5)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 || b == 5") {
      assume(a == 3 || b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 || b == 6") {
      assume(a == 3 || b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 || b == 5") {
      assume(a == 2 || b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 || b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 || b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 | b == 5") {
      assume(a == 3 | b == 5, ", dude")
    }

    it("should do nothing when is used to check a == 3 | b == 6") {
      assume(a == 3 | b == 6, ", dude")
    }

    it("should do nothing when is used to check a == 2 | b == 5") {
      assume(a == 2 | b == 5, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 | b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 | b == 6, ", dude")
      }
      assert(e.message === Some(commaAnd(didNotEqual(3, 2), didNotEqual(5, 6)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check a == 3 && (b == 5 && b > 3)") {
      assume(a == 3 && (b == 5 && b > 3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && (b == 5 && b > 5)") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && (b == 5 && b > 5), ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), commaBut(equaled(5, 5), wasNotGreaterThan(5, 5))) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(a == 5)") {
      assume(!(a == 5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(a == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!(a == 3), ", dude")
      }
      assert(e.message === Some(equaled(3, 3) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 3 && !(b == 5)") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && !(b == 5), ", dude")
      }
      assert(e.message === Some(commaBut(equaled(3, 3), equaled(5, 5)) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check (a == 3) == (b == 5)") {
      assume((a == 3) == (b == 5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check (a == 3) == (b != 5)") {
      val e = intercept[TestCanceledException] {
        assume((a == 3) == (b != 5), ", dude")
      }
      assert(e.message === Some(didNotEqual(true, false) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should short-circuit && when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 && s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should not short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == true)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assume(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should not short-circuit | when first condition was true") {
      val s = new Stateful
      assume(a == 3 | s.changeState, ", dude")
      assert(s.state == true)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assume(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      assume({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        assume({ println("hi"); b == 5} && a == 5, ", dude")
      }
      if (ScalaTestVersions.BuiltForScalaVersion == "2.12" || ScalaTestVersions.BuiltForScalaVersion == "2.13")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else if (ScalaTestVersions.BuiltForScalaVersion == "2.10" || ScalaTestVersions.BuiltForScalaVersion == "2.11")
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      else // dotty
        assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.Predef.println(\"hi\")" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 9)))
    }

    it("should preserve side effects when Apply with single argument is passed in") {
      assume(neverRuns1(sys.error("Sad times 1")), "should not fail!")
    }

    it("should preserve side effects when Apply with 2 argument list is passed in") {
      assume(neverRuns2(sys.error("Sad times 2"))(0), "should not fail!")
    }

    it("should preserve side effects when typed Apply with 2 argument list is passed in") {
      assume(neverRuns3(sys.error("Sad times 3"))(0), "should not fail!")
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
      assume(s1 startsWith "hi", ", dude")
      assume(s1.startsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s2 startsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s2 startsWith "hi", ", dude")
      }
      assert(e1.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s2.startsWith("hi"), ", dude")
      }
      assert(e2.message == Some(didNotStartWith(s2, "hi") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 startsWith 1") {
      assume(ci1 startsWith 1, ", dude")
      assume(ci1.startsWith(1), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci2 startsWith 1") {
      val e1 = intercept[TestCanceledException] {
        assume(ci2 startsWith 1, ", dude")
      }
      assert(e1.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci2.startsWith(1), ", dude")
      }
      assert(e2.message == Some(didNotStartWith(ci2, 1) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s2.startsWith(\"hi\")") {
      assume(!s2.startsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.startsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s1.startsWith("hi"), ", dude")
      }
      assert(e1.message == Some(startedWith(s1, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s2 endsWith \"hi\"") {
      assume(s2 endsWith "hi", ", dude")
      assume(s2.endsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1 endsWith \"hi\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s1 endsWith "hi", ", dude")
      }
      assert(e1.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s1.endsWith("hi"), ", dude")
      }
      assert(e2.message == Some(didNotEndWith(s1, "hi") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 endsWith 1") {
      assume(ci2 endsWith 1, ", dude")
      assume(ci2.endsWith(1), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 endsWith 1") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 endsWith 1, ", dude")
      }
      assert(e1.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.endsWith(1), ", dude")
      }
      assert(e2.message == Some(didNotEndWith(ci1, 1) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.endsWith(\"hi\")") {
      assume(!s1.endsWith("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s2.endsWith(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s2.endsWith("hi"), ", dude")
      }
      assert(e1.message == Some(endedWith(s2, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3 contains \"hi\"") {
      assume(s3 contains "hi", ", dude")
      assume(s3.contains("hi"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3 contains \"hello\"") {
      val e1 = intercept[TestCanceledException] {
        assume(s3 contains "hello", ", dude")
      }
      assert(e1.message == Some(didNotContain(s3, "hello") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(s3.contains("hello"), ", dude")
      }
      assert(e2.message == Some(didNotContain(s3, "hello") + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci2 contains 2") {
      assume(ci2 contains 2, ", dude")
      assume(ci2.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(ci1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(ci1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.contains(\"hello\")") {
      assume(!s3.contains("hello"), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.contains(\"hi\")") {
      val e1 = intercept[TestCanceledException] {
        assume(!s3.contains("hi"), ", dude")
      }
      assert(e1.message == Some(contained(s3, "hi") + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1 contains 2") {
      assume(l1 contains 2, ", dude")
      assume(l1.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(l1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(l1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1 contains 5)") {
      assume(!(l1 contains 5), ", dude")
      assume(!l1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        assume(!(l1 contains 2), ", dude")
      }
      assert(e1.message == Some(contained(l1, 2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(!l1.contains(2), ", dude")
      }
      assert(e2.message == Some(contained(l1, 2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check m1 contains 2") {
      assume(m1 contains 2, ", dude")
      assume(m1.contains(2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check m1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(m1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContainKey(m1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(m1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContainKey(m1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(m1 contains 5)") {
      assume(!(m1 contains 5), ", dude")
      assume(!m1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(m1 contains 2)") {
      val e1 = intercept[TestCanceledException] {
        assume(!(m1 contains 2), ", dude")
      }
      assert(e1.message == Some(containedKey(m1, 2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(!m1.contains(2), ", dude")
      }
      assert(e2.message == Some(containedKey(m1, 2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ct1 contains 8") {
      assume(ct1 contains 8, ", dude")
      assume(ct1.contains(8), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ct1 contains 5") {
      val e1 = intercept[TestCanceledException] {
        assume(ct1 contains 5, ", dude")
      }
      assert(e1.message == Some(didNotContain(ct1, 5) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ct1.contains(5), ", dude")
      }
      assert(e2.message == Some(didNotContain(ct1, 5) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ct1.contains(5)") {
      assume(!ct1.contains(5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ct1.contains(8)") {
      val e1 = intercept[TestCanceledException] {
        assume(!ct1.contains(8), ", dude")
      }
      assert(e1.message == Some(contained(ct1, 8) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 eq ci3") {
      assume(ci1 eq ci3, ", dude")
      assume(ci1.eq(ci3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 eq ci2") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 eq ci2, ", dude")
      }
      assert(e1.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.eq(ci2), ", dude")
      }
      assert(e2.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.eq(ci2)") {
      assume(!ci1.eq(ci2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.eq(ci3)") {
      val e = intercept[TestCanceledException] {
        assume(!ci1.eq(ci3), ", dude")
      }
      assert(e.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check ci1 ne ci2") {
      assume(ci1 ne ci2, ", dude")
      assume(ci1.ne(ci2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check ci1 ne ci3") {
      val e1 = intercept[TestCanceledException] {
        assume(ci1 ne ci3, ", dude")
      }
      assert(e1.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e1.failedCodeFileName == (Some(fileName)))
      assert(e1.failedCodeLineNumber == (Some(thisLineNumber - 4)))

      val e2 = intercept[TestCanceledException] {
        assume(ci1.ne(ci3), ", dude")
      }
      assert(e2.message == Some(wasTheSameInstanceAs(ci1, ci3) + ", dude"))
      assert(e2.failedCodeFileName == (Some(fileName)))
      assert(e2.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !ci1.ne(ci3)") {
      assume(!ci1.ne(ci3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !ci1.ne(ci2)") {
      val e = intercept[TestCanceledException] {
        assume(!ci1.ne(ci2), ", dude")
      }
      assert(e.message == Some(wasNotTheSameInstanceAs(ci1, ci2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s4.isEmpty") {
      assume(s4.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s3.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(s3.isEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s3.isEmpty") {
      assume(!s3.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s4.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!s4.isEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(s4) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l2.isEmpty") {
      assume(l2.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(l1.isEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isEmpty") {
      assume(!l1.isEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l2.isEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!l2.isEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(l2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s3.nonEmpty") {
      assume(s3.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s4.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(s4.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(s4) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s4.nonEmpty") {
      assume(!s4.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s3.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!s3.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(s3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.nonEmpty") {
      assume(l1.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l2.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(l2.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasEmpty(l2) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l2.nonEmpty") {
      assume(!l2.nonEmpty, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.nonEmpty") {
      val e = intercept[TestCanceledException] {
        assume(!l1.nonEmpty, ", dude")
      }
      assert(e.message == Some(wasNotEmpty(l1) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.isInstanceOf[String]") {
      assume(s1.isInstanceOf[String], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        assume(l1.isInstanceOf[String], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "scala.Predef.String") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.isInstanceOf[List[Int]]") {
      assume(l1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        assume(s1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(s1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check date.isInstanceOf[Date]") {
      assume(date.isInstanceOf[Date], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        assume(l1.isInstanceOf[Date], ", dude")
      }
      assert(e.message == Some(wasNotInstanceOf(l1, "java.util.Date") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.isInstanceOf[String]") {
      assume(!l1.isInstanceOf[String], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !s1.isInstanceOf[String]") {
      val e = intercept[TestCanceledException] {
        assume(!s1.isInstanceOf[String], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(s1, "scala.Predef.String") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !s1.isInstanceOf[List[Int]]") {
      assume(!s1.isInstanceOf[List[Int]], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.isInstanceOf[List[Int]]") {
      val e = intercept[TestCanceledException] {
        assume(!l1.isInstanceOf[List[Int]], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(l1, if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "scala.collection.immutable.List[scala.Int]" else "scala.List") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should do nothing when is used to check !l1.isInstanceOf[Date]") {
      assume(!l1.isInstanceOf[Date], ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !date.isInstanceOf[Date]") {
      val e = intercept[TestCanceledException] {
        assume(!date.isInstanceOf[Date], ", dude")
      }
      assert(e.message == Some(wasInstanceOf(date, "java.util.Date") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.length == 9") {
      assume(s1.length == 12, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.length == 10") {
      val e = intercept[TestCanceledException] {
        assume(s1.length == 10, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(s1, 12, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.length == 3") {
      assume(l1.length == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.length == 10") {
      val e = intercept[TestCanceledException] {
        assume(l1.length == 10, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(l1, 3, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.length == 10)") {
      assume(!(s1.length == 10), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.length == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(s1.length == 12), ", dude")
      }
      assert(e.message == Some(hadLength(s1, 12) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.length == 2)") {
      assume(!(l1.length == 2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(l1.length == 3), ", dude")
      }
      assert(e.message == Some(hadLength(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.length == 2.0f") {
      assume(floatLengthSize.length == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.length == 1.0f") {
      val e = intercept[TestCanceledException] {
        assume(floatLengthSize.length == 1.0f, ", dude")
      }
      assert(e.message == Some(hadLengthInsteadOfExpectedLength(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check s1.size == 9") {
      assume(s1.size == 12, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check s1.size == 10") {
      val e = intercept[TestCanceledException] {
        assume(s1.size == 10, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(s1, 12, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.size == 3") {
      assume(l1.size == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.size == 10") {
      val e = intercept[TestCanceledException] {
        assume(l1.size == 10, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(l1, 3, 10) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(s1.size == 10)") {
      assume(!(s1.size == 10), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(s1.size == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(s1.size == 12), ", dude")
      }
      assert(e.message == Some(hadSize(s1, 12) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !(l1.size == 2)") {
      assume(!(l1.size == 2), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !(l1.size == 9)") {
      val e = intercept[TestCanceledException] {
        assume(!(l1.size == 3), ", dude")
      }
      assert(e.message == Some(hadSize(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check floatLengthSize.size == 2.0f") {
      assume(floatLengthSize.size == 2.0f, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check floatLengthSize.size == 1.0f") {
      val e = intercept[TestCanceledException] {
        assume(floatLengthSize.size == 1.0f, ", dude")
      }
      assert(e.message == Some(hadSizeInsteadOfExpectedSize(floatLengthSize, 2.0f, 1.0f) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assume(l1.exists(_ == 3), ", dude")
    }

    it("should do nothing when is used to check l1.exists(3 == _)") {
      assume(l1.exists(3 == _), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ == 5), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(5 == _)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(5 == _), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assume(!l1.exists(_ == 5), ", dude")
    }

    it("should do nothing when is used to check !l1.exists(5 == _)") {
      assume(!l1.exists(5 == _), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(_ == 3), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(3 == _)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(3 == _), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$42: scala.Int) => _$42.>(3)))" else "l1.exists(((x$43: Int) => x$43.>(3)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(3 < _)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(3 < _), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l1.exists(((_$43: scala.Int) => 3.<(_$43)))" else "l1.exists(((x$44: Int) => 3.<(x$44)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        assume(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse(if (ScalaTestVersions.BuiltForScalaVersion.startsWith("3.")) "l3.exists(((_$44: java.lang.String) => _$44.isEmpty()))" else "l3.exists(((x$45: String) => x$45.isEmpty()))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(false)") {
      val e = intercept[TestCanceledException] {
        assume(ci1.exists(321), ", dude")
      }
      assert(e.message == Some(wasFalse("ci1.exists(321)") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    def woof(f: => Unit) = "woof"
    def meow(x: Int = 0, y: Int = 3) = "meow"

    it("should do nothing when used to check woof { meow(y = 5) } == \"woof\"") {
      assume(woof { meow(y = 5) } == "woof", ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check woof { meow(y = 5) } == \"meow\"") {
      val e = intercept[TestCanceledException] {
        assume(woof { meow(y = 5) } == "meow", ", dude")
      }
      assert(e.message == Some(didNotEqual("woof", "meow") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should compile when used with org == xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assume(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assume(org === "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends AnyFunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    assume(org === "test", ", dude")
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
          |assume(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |assume(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |assume(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assume(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |assume(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }

    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assume(x + 1 == 2, "clue") eq Succeeded)
    }
  }

  describe("assertTypeError method ") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        assertTypeError("val a: String = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          assertTypeError("val a = 1")
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertTypeError("println(\"test)")
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
      
      it("should do nothing when used with 'val i: Int = null'") {
        assertTypeError("val i: Int = null")
      }

      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertTypeError("arrayList.asScala")
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("arrayList.asScala")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        assertTypeError(
          """
            |val a: String = 2
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          assertTypeError(
            """
              |val a = 1
              |""".stripMargin
          )
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
      
      it("should throw TestFailedException with correct message and stack depth when parse failed ") {
        val e = intercept[TestFailedException] {
          assertTypeError(
            """
              |println("test)
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0, "error message was: " + e.message.get)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
      
      it("should do nothing when used with 'val i: Int = null'") {
        assertTypeError(
          """
            |val i: Int = null
            |""".stripMargin
        )
      }
      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertTypeError(
            """
              |arrayList.asScala
              |""".stripMargin
          )
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone(Prettifier.lineSeparator + "arrayList.asScala" + Prettifier.lineSeparator)))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      assert(assertTypeError("val x: String = 1") eq Succeeded)
    }
  }

  describe("assertOnTypeError method ") {

    describe("when work with string literal") {

      it("should do nothing when type check failed with a good error message") {
        assertOnTypeError("val a: String = 1"){ msg =>
          // SKIP-DOTTY-START
          assert(msg.contains("type mismatch") && msg.contains("String") && msg.contains("Int"))
          // SKIP-DOTTY-END
          //DOTTY-ONLY assert(msg.contains("Found:    (1 : Int)") && msg.contains("Required: String"))
        }

        assertOnTypeError("implicitly[Int =:= String]") { typeError =>
          assert(typeError.contains("Cannot prove that Int =:= String"))
        }
      }

      it("should throw TestFailedException when type check failed with a wrong error message") {
        val e = intercept[TestFailedException] {
          assertOnTypeError("val i: String = 5") { typeError =>
            assert(typeError.contains("foobar"))
          }
        }
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          assertOnTypeError("val a = 1")(_ => fail())
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertOnTypeError("println(\"test)")(_ => fail())
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
      
      it("should do nothing when used with 'val i: Int = null'") {
        assertOnTypeError("val i: Int = null") { msg =>
          // SKIP-DOTTY-START
          assert(msg.contains("an expression of type Null is ineligible for implicit conversion"))
          // SKIP-DOTTY-END
          //DOTTY-ONLY assert(msg.contains("Found:    Null") && msg.contains("Required: Int"))
        }
      }

      it("should provide a suitable error message to the assertion when used with 'val i: Int = null'") {
        val e = intercept[TestFailedException] {
          assertOnTypeError("val i: Int = null") { msg =>
            assert(msg.contains("foobar"))
          }
        }
        assert(e.failedCodeFileName === Some(fileName))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertOnTypeError("arrayList.asScala")(_ => fail())
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("arrayList.asScala")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        assertOnTypeError(
          """
            |val a: String = 2
            |""".stripMargin
        )(_ => succeed)
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          assertOnTypeError(
            """
              |val a = 1
              |""".stripMargin
          )(_ => succeed)
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone("" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
        assert(e.failedCodeFileName === (Some(fileName)))
        // SKIP-DOTTY-START
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 5)))
        // SKIP-DOTTY-END
        //DOTTY-ONLY assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
      
      it("should throw TestFailedException with correct message and stack depth when parse failed ") {
        val e = intercept[TestFailedException] {
          assertOnTypeError(
            """
              |println("test)
              |""".stripMargin
          )(_ => succeed)
        }
        val errMsg = Resources.expectedTypeErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0, "error message was: " + e.message.get)
        assert(e.failedCodeFileName === (Some(fileName)))
        // SKIP-DOTTY-START
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 7)))
        // SKIP-DOTTY-END
        //DOTTY-ONLY assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
      
      it("should do nothing when used with 'val i: Int = null'") {
        assertOnTypeError(
          """
            |val i: Int = null
            |""".stripMargin
        )(_ => succeed)
      }
      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertOnTypeError(
            """
              |arrayList.asScala
              |""".stripMargin
          )(_ => succeed)
        }
        assert(e.message == Some(Resources.expectedTypeErrorButGotNone(Prettifier.lineSeparator + "arrayList.asScala" + Prettifier.lineSeparator)))
        assert(e.failedCodeFileName === (Some(fileName)))
        // SKIP-DOTTY-START
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 5)))
        // SKIP-DOTTY-END
        //DOTTY-ONLY assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
    }
    it("should result in type Assertion and, on success, return the provided assertion") {
      val assertion = succeed
      assert(assertTypeError("val x: String = 1") eq Succeeded)
    }
  }

  describe("assertDoesNotCompile method ") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        assertDoesNotCompile("val a: String = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when parse and type check passed") {
        val e = intercept[TestFailedException] {
          assertDoesNotCompile("val a = 1")
        }
        assert(e.message == Some(Resources.expectedCompileErrorButGotNone("val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should do nothing when parse failed") {
        assertDoesNotCompile("println(\"test)")
      }

      it("should result in type Assertion and, on success, return the Succeeded value") {
        assert(assertDoesNotCompile("val x: String = 1") eq Succeeded)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        assertDoesNotCompile("val i: Int = null")
      }

      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertDoesNotCompile("arrayList.asScala".stripMargin)
        }
        assert(e.message == Some(Resources.expectedCompileErrorButGotNone("arrayList.asScala")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        assertDoesNotCompile(
          """
            |val a: String = 2
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when parse and type check passed") {
        val e = intercept[TestFailedException] {
          assertDoesNotCompile(
            """
              |val a = 1
              |""".stripMargin
          )
        }
        assert(e.message == Some(Resources.expectedCompileErrorButGotNone("" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
      
      it("should do nothing when parse failed ") {
        assertDoesNotCompile(
          """
            |println(\"test)
            |""".stripMargin
        )
      }

      it("should result in type Assertion and, on success, return the Succeeded value") {
        assert(assertDoesNotCompile(
          """
            |val x: String = 1
            |""".stripMargin) eq Succeeded)
      }

      it("should do nothing when used with 'val i: Int = null'") {
        assertDoesNotCompile(
          """
            |val i: Int = null
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        val e = intercept[TestFailedException] {
          assertDoesNotCompile(
            """
              |arrayList.asScala
              |""".stripMargin)
        }
        assert(e.message == Some(Resources.expectedCompileErrorButGotNone(Prettifier.lineSeparator + "arrayList.asScala" + Prettifier.lineSeparator)))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 7)))
      }
    }
  }

  describe("assertCompiles method") {

    describe("when work with string literal") {

      it("should do nothing when type check passed") {
        assertCompiles("val a = 1")
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          assertCompiles("val a: String = 2")
        }
        val errMsg = Resources.expectedNoErrorButGotTypeError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("val a: String = 2") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertCompiles("println(\"test)")
        }
        val errMsg = Resources.expectedNoErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should do nothing when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        assertCompiles("arrayList.asScala")
      }
    }

    describe("when used with triple quotes string literal with stripMargin") {

      it("should do nothing when type check passed") {
        assertCompiles(
          """
            |val a = 1
            |""".stripMargin
        )
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          assertCompiles(
            """
              |val a: String = 2
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedNoErrorButGotTypeError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("" + Prettifier.lineSeparator + "val a: String = 2" + Prettifier.lineSeparator + "") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
      
      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertCompiles(
            """
              |println("test)
              |""".stripMargin
          )
        }
        val errMsg = Resources.expectedNoErrorButGotParseError("", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
      
      it("should do nothing when the code compiles with implicit view in scope") {
        import scala.collection.JavaConverters._

        val arrayList: java.util.ArrayList[String] = new java.util.ArrayList[String]()

        arrayList.add("Foo")
        arrayList.add("Bar")

        assertCompiles(
          """
            |arrayList.asScala
            |""".stripMargin)
      }
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      assert(assertCompiles("val x: Int = 1") eq Succeeded)
    }
  }

  describe("The assertResult method") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      assertResult(npe.getMessage) { null }
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      assert(a1 ne a2)
      assertResult(a1) { a2 }
      intercept[TestFailedException] {
        assertResult(a1) { a3 }
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1) { a2 }
      intercept[TestFailedException] {
        assertResult(a1) { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1) { a2 }
      intercept[TestFailedException] {
        assertResult(a1) { a3 }
      }
      intercept[TestFailedException] {
        assertResult(a3) { a1 }
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assertResult(n1) { n2 }
      intercept[TestFailedException] {
        assertResult(n1) { "hi" }
      }
      intercept[TestFailedException] {
        assertResult("hi") { n1 }
      }
      val a1 = Array(1, 2, 3)
      val aNull: Array[Int] = null
      intercept[TestFailedException] {
        assertResult(aNull) { a1 }
      }
      intercept[TestFailedException] {
        assertResult(a1) { aNull }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        assertResult(a) { null }
      }
      assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, a, null)))
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assertResult(2) { x + 1 } eq Succeeded)
    }
  }

  describe("The assertResult method that 'gets a clue'") {
    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      assertResult(npe.getMessage, "a clue") { null }
    }
    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      assert(a1 ne a2)
      assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { a3 }
      }
    }
    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { a3 }
      }
      intercept[TestFailedException] {
        assertResult(a3, "a clue") { a1 }
      }
    }
    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      assertResult(n1, "a clue") { n2 }
      intercept[TestFailedException] {
        assertResult(n1, "a clue") { "hi" }
      }
      intercept[TestFailedException] {
        assertResult("hi", "a clue") { n1 }
      }
      val a1 = Array(1, 2, 3)
      val aNull: Array[Int] = null
      intercept[TestFailedException] {
        assertResult(aNull, "a clue") { a1 }
      }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { aNull }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        assertResult(a, "a clue") { null }
      }
      assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, a, null) + " a clue"))
    }
    it("should append clues in a satisfying manner") {
      val a = "hi"
      val b = "ho"
      val aDiff = "h[i]"
      val bDiff = "h[o]"
      val e1 = intercept[TestFailedException] {
        assertResult(a, "the clue") { b }
      }
      assert(e1.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + " the clue"))

      val e2 = intercept[TestFailedException] {
        assertResult(a, ", the clue") { b }
      }
      assert(e2.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + ", the clue"))

      val e3 = intercept[TestFailedException] {
        assertResult(a, ". the clue") { b }
      }
      assert(e3.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + ". the clue"))

      val e4 = intercept[TestFailedException] {
        assertResult(a, "; the clue") { b }
      }
      assert(e4.message === Some(FailureMessages.expectedButGot(prettifier, aDiff, bDiff) + "; the clue"))
    }
    it("should result in type Assertion and, on success, return the Succeeded value") {
      val x = 1
      assert(assertResult(2, "clue") { x + 1 } eq Succeeded)
    }
  }

  describe("The Assertions") {
    it("should not break stripMargin") {
      assert("foo".stripMargin('!') == "foo") 
    }    
  }
}
