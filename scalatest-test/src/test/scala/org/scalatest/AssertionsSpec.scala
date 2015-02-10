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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import scala.util.Failure
import scala.util.Success
import Assertions.NormalResult
import org.scalatest.exceptions.TestCanceledException
import OptionValues._
import java.util.Date
import org.scalactic.Prettifier

class AssertionsSpec extends FunSpec {
  
  val fileName: String = "AssertionsSpec.scala"

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
      val a1: Array[Any] = Array(1, Array("a", "b"), 3)
      val a2: Array[Any] = Array(1, Array("a", "b"), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assert(a1 === a2)
      intercept[TestFailedException] {
        assert(a1 === a3)
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1: Array[Any] = Array(1, Array("a", null), 3)
      val a2: Array[Any] = Array(1, Array("a", null), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
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
      assert(e1.message === Some(FailureMessages("didNotEqual", a, null)))
    }
  }
  describe("The intercept method") {
    it("should  catches subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      intercept[MyException] {
        throw new MyException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      intercept[MyException] {
        throw new MyExceptionSubClass
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod() {}
      }
      class AnotherException extends RuntimeException with MyTrait
      val caught = intercept[MyTrait] {
        throw new AnotherException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Make sure the result type is the type passed in, so I can 
      // not cast and still invoke any method on it I want
      caught.someRandomMethod()
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
    it("should catch subtypes of the given exception type") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      intercept[MyException] {
        throw new MyException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      intercept[MyException] {
        throw new MyExceptionSubClass
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod() {}
      }
      class AnotherException extends RuntimeException with MyTrait
      val caught = intercept[MyTrait] {
        throw new AnotherException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Make sure the result type is the type passed in, so I can 
      // not cast and still invoke any method on it I want
      caught.someRandomMethod()
    }
  }
  describe("The trap method") {
    it("should be a shorthand for catch and return any thrown exception that would cause a test to fail") {
      val a = 12
      val trappedEx = trap { assert(a == 13) }
      assert(trappedEx.isInstanceOf[TestFailedException])
      assert(trappedEx.getMessage == "12 did not equal 13")
      val trappedUnit = trap { assert(a == 12) }
      assert(trappedUnit == NormalResult(()))
      val trappedInt = trap { 12 }
      assert(trappedInt == NormalResult(12))
      val trappedString = trap { "12" }
      assert(trappedString == NormalResult("12"))
      assert(trappedString.toString == Resources("resultWas", "\"12\""))
      intercept[OutOfMemoryError] {
        trap { throw new OutOfMemoryError }
      }
    }
  }

  def didNotEqual(left: Any, right: Any): String = {
    val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right)
    FailureMessages("didNotEqual", leftee, rightee)
  }

  def equaled(left: Any, right: Any): String =
    FailureMessages("equaled", left, right)

  def expressionFailed(left: String): String =
    FailureMessages("expressionFailed", UnquotedString(left))

  def wasNotGreaterThan(left: Any, right: Any): String =
    FailureMessages("wasNotGreaterThan", left, right)

  def wasGreaterThan(left: Any, right: Any): String =
    FailureMessages("wasGreaterThan", left, right)

  def wasNotGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotGreaterThanOrEqualTo", left, right)

  def wasGreaterThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasGreaterThanOrEqualTo", left, right)

  def wasNotLessThan(left: Any, right: Any): String =
    FailureMessages("wasNotLessThan", left, right)

  def wasLessThan(left: Any, right: Any): String =
    FailureMessages("wasLessThan", left, right)

  def wasNotLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasNotLessThanOrEqualTo", left, right)

  def wasLessThanOrEqualTo(left: Any, right: Any): String =
    FailureMessages("wasLessThanOrEqualTo", left, right)

  def commaAnd(left: String, right: String): String =
    FailureMessages("commaAnd", UnquotedString(left), UnquotedString(right))

  def commaBut(left: String, right: String): String =
    FailureMessages("commaBut", UnquotedString(left), UnquotedString(right))

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

  def hadLengthInsteadOfExpectedLength(left: Any, actual: Long, expected: Long): String =
    FailureMessages("hadLengthInsteadOfExpectedLength", left, actual, expected)

  def hadLength(left: Any, actual: Long): String =
    FailureMessages("hadLength", left, actual)

  def hadSizeInsteadOfExpectedSize(left: Any, actual: Long, expected: Long): String =
    FailureMessages("hadSizeInsteadOfExpectedSize", left, actual, expected)

  def hadSize(left: Any, actual: Long): String =
    FailureMessages("hadSize", left, actual)

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
  
  describe("The assert(boolean) method") {
    val a = 3
    val b = 5
    val c = "8"
    
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
    
    it("should throw TestFailedException with correct message and stack depth when is used to check c == null") {
      val e = intercept[TestFailedException] { 
        assert(c == null)
      }
      assert(e.message === Some(didNotEqual(c, null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check null == c") {
      val e = intercept[TestFailedException] { 
        assert(null == c)
      }
      assert(e.message === Some(didNotEqual(null, c)))
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
    
    // UncheckedEquality tests
    // currently these tests are not calling UncheckedEquality's === and !== yet, import org.scalactic.UncheckedEquality does not seems to work
    // Should make Assertions to extend UncheckedEquality instead of LegacyTripleEquals instead.
    
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
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 6)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
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

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assert(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      assert(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assert(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && { println("hi"); b == 3})
      }
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      assert({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        assert({ println("hi"); b == 5} && a == 5)
      }
      assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
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
      assert(e.message == Some(wasNotInstanceOf(s1, "scala.List")))
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
      assert(e.message == Some(wasInstanceOf(l1, "scala.List")))
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

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assert(l1.exists(_ == 3))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ == 5))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assert(!l1.exists(_ == 5))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(_ == 3))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse("l1.exists(((x$6: Int) => x$6.>(3)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        assert(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse("l3.exists(((x$7: String) => x$7.isEmpty()))")))
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
        |val org = "test"
        |assert(org == "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assert(org === "test")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    assert(org === "test")
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
          |assert(org.aCustomMethod)
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |assert(!org)
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.isEmpty)
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.isInstanceOf[String])
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |assert(org.size == 0)
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.length == 0)
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |assert(org.exists(_ == 'b'))
        """.stripMargin)
    }
  }

  describe("The assert(boolean, clue) method") {
    val a = 3
    val b = 5
    val c = "8"

    val bob = "bob"
    val alice = "alice"

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
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

    it("should throw TestFailedException with correct message and stack depth when is used to check c == null") {
      val e = intercept[TestFailedException] {
        assert(c == null, ". dude")
      }
      assert(e.message === Some(didNotEqual(c, null) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == c") {
      val e = intercept[TestFailedException] {
        assert(null == c, "; dude")
      }
      assert(e.message === Some(didNotEqual(null, c) + "; dude"))
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

    // UncheckedEquality tests
    // currently these tests are not calling UncheckedEquality's === and !== yet, import org.scalactic.UncheckedEquality does not seems to work
    // Should make Assertions to extend UncheckedEquality instead of LegacyTripleEquals instead.

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
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestFailedException] {
        assert(a == 2 & b == 6, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
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

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestFailedException] {
        assert(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assert(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      assert(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assert(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestFailedException] {
        assert(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      assert({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestFailedException] {
        assert({ println("hi"); b == 5} && a == 5, ", dude")
      }
      assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
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
      assert(e.message == Some(wasNotInstanceOf(s1, "scala.List") + ", dude"))
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
      assert(e.message == Some(wasInstanceOf(l1, "scala.List") + ", dude"))
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
      assert(!(l1.length == 2))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !(l1.length == 9)") {
      val e = intercept[TestFailedException] {
        assert(!(l1.length == 3), ", dude")
      }
      assert(e.message == Some(hadLength(l1, 3) + ", dude"))
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

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ == 5), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assert(!l1.exists(_ == 5), ", dude")
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestFailedException] {
        assert(!l1.exists(_ == 3), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestFailedException] {
        assert(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse("l1.exists(((x$12: Int) => x$12.>(3)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestFailedException] {
        assert(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse("l3.exists(((x$13: String) => x$13.isEmpty()))") + ", dude"))
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
          |val org = "test"
          |assert(org == "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = "test"
          |assert(org === "test", ", dude")
        """.stripMargin)
    }

    it("should compile when used with org === xxx with TypeCheckedTripleEquals that shadow org.scalactic") {
      assertCompiles(
        """
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
          |  it("testing here") {
          |    val org = "test"
          |    assert(org === "test", ", dude")
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
          |assert(org.aCustomMethod, ", dude")
        """.stripMargin)
    }

    it("should compile when used with !org that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = false
          |assert(!org, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isEmpty that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.isEmpty, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.isInstanceOf that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.isInstanceOf[String], ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.size == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = Array.empty[String]
          |assert(org.size == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.length == 0 that shadow org.scalactic") {
      assertCompiles(
        """
          |val org = ""
          |assert(org.length == 0, ", dude")
        """.stripMargin)
    }

    it("should compile when used with org.exists(_ == 'b') that shadow org.scalactic ") {
      assertCompiles(
        """
          |val org = "abc"
          |assert(org.exists(_ == 'b'), ", dude")
        """.stripMargin)
    }

  }

  describe("The assume(boolean) method") {
    val a = 3
    val b = 5
    val c = "8"

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

    it("should throw TestCanceledException with correct message and stack depth when is used to check c == null") {
      val e = intercept[TestCanceledException] {
        assume(c == null)
      }
      assert(e.message === Some(didNotEqual(c, null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == c") {
      val e = intercept[TestCanceledException] {
        assume(null == c)
      }
      assert(e.message === Some(didNotEqual(null, c)))
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

    // UncheckedEquality tests
    // currently these tests are not calling UncheckedEquality's === and !== yet, import org.scalactic.UncheckedEquality does not seems to work
    // Should make Assertions to extend UncheckedEquality instead of LegacyTripleEquals instead.

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
      assert(e.message === Some(didNotEqual(3, 2)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 6)
      }
      assert(e.message === Some(didNotEqual(3, 2)))
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

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 & s.changeState)
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assume(a == 3 || s.changeState)
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      assume(a == 3 | s.changeState)
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assume(a == 3 && { println("hi"); b == 5})
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && { println("hi"); b == 3})
      }
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}"))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3") {
      assume({ println("hi"); b == 5} && a == 3)
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        assume({ println("hi"); b == 5} && a == 5)
      }
      assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5))))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
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
      assert(e.message == Some(wasNotInstanceOf(s1, "scala.List")))
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
      assert(e.message == Some(wasInstanceOf(l1, "scala.List")))
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

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assume(l1.exists(_ == 3))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ == 5))
      }
      assert(e.message == Some(didNotContain(l1, 5)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assume(!l1.exists(_ == 5))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(_ == 3))
      }
      assert(e.message == Some(contained(l1, 3)))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ > 3))
      }
      assert(e.message == Some(wasFalse("l1.exists(((x$18: Int) => x$18.>(3)))")))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        assume(l3.exists(_.isEmpty))
      }
      assert(e.message == Some(wasFalse("l3.exists(((x$19: String) => x$19.isEmpty()))")))
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
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
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
  }

  describe("The assume(boolean, clue) method") {
    val a = 3
    val b = 5
    val c = "8"

    val bob = "bob"
    val alice = "alice"

    it("should throw NullPointerException when null is passed in as clue") {
      val e = intercept[NullPointerException] {
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

    it("should throw TestCanceledException with correct message and stack depth when is used to check c == null") {
      val e = intercept[TestCanceledException] {
        assume(c == null, ", dude")
      }
      assert(e.message === Some(didNotEqual(c, null) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == c") {
      val e = intercept[TestCanceledException] {
        assume(null == c, ". dude")
      }
      assert(e.message === Some(didNotEqual(null, c) + ". dude"))
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

    // UncheckedEquality tests
    // currently these tests are not calling UncheckedEquality's === and !== yet, import org.scalactic.UncheckedEquality does not seems to work
    // Should make Assertions to extend UncheckedEquality instead of LegacyTripleEquals instead.

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
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == 2 & b == 6") {
      val e = intercept[TestCanceledException] {
        assume(a == 2 & b == 6, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, 2) + ", dude"))
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

    it("should short-circuit & when first condition was false") {
      val s = new Stateful
      intercept[TestCanceledException] {
        assume(a == 5 & s.changeState, ", dude")
      }
      assert(s.state == false)
    }

    it("should short-circuit || when first condition was true") {
      val s = new Stateful
      assume(a == 3 || s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should short-circuit | when first condition was true") {
      val s = new Stateful
      assume(a == 3 | s.changeState, ", dude")
      assert(s.state == false)
    }

    it("should do nothing when it is used to check a == 3 && { println(\"hi\"); b == 5}") {
      assume(a == 3 && { println("hi"); b == 5}, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to check a == 3 && { println(\"hi\"); b == 3}") {
      val e = intercept[TestCanceledException] {
        assume(a == 3 && { println("hi"); b == 3}, ", dude")
      }
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(3)" + Prettifier.lineSeparator + "}")) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when it is used to check { println(\"hi\"); b == 5} && a == 3 ") {
      assume({ println("hi"); b == 5} && a == 3, ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is usesd to { println(\"hi\"); b == 5} && a == 5") {
      val e = intercept[TestCanceledException] {
        assume({ println("hi"); b == 5} && a == 5, ", dude")
      }
      assert(e.message == Some(commaBut(wasTrue("{" + Prettifier.lineSeparator + "  scala.this.Predef.println(\"hi\");" + Prettifier.lineSeparator + "  b.==(5)" + Prettifier.lineSeparator + "}"), didNotEqual(3, 5)) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
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
      assert(e.message == Some(wasNotInstanceOf(s1, "scala.List") + ", dude"))
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
      assert(e.message == Some(wasInstanceOf(l1, "scala.List") + ", dude"))
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

    it("should do nothing when is used to check l1.exists(_ == 3)") {
      assume(l1.exists(_ == 3), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ == 5)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ == 5), ", dude")
      }
      assert(e.message == Some(didNotContain(l1, 5) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should do nothing when is used to check !l1.exists(_ == 5)") {
      assume(!l1.exists(_ == 5), ", dude")
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check !l1.exists(_ == 3)") {
      val e = intercept[TestCanceledException] {
        assume(!l1.exists(_ == 3), ", dude")
      }
      assert(e.message == Some(contained(l1, 3) + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l1.exists(_ > 3)") {
      val e = intercept[TestCanceledException] {
        assume(l1.exists(_ > 3), ", dude")
      }
      assert(e.message == Some(wasFalse("l1.exists(((x$24: Int) => x$24.>(3)))") + ", dude"))
      assert(e.failedCodeFileName == (Some(fileName)))
      assert(e.failedCodeLineNumber == (Some(thisLineNumber - 4)))
    }

    it("should throw TestCanceledException with correct message and stack depth when is used to check l3.exists(_.isEmpty)") {
      val e = intercept[TestCanceledException] {
        assume(l3.exists(_.isEmpty), ", dude")
      }
      assert(e.message == Some(wasFalse("l3.exists(((x$25: String) => x$25.isEmpty()))") + ", dude"))
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
          |class TestSpec extends FunSpec with org.scalactic.TypeCheckedTripleEquals {
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
        assert(e.message == Some(Resources("expectedTypeErrorButGotNone", "val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertTypeError("println(\"test)")
        }
        val errMsg = Resources("expectedTypeErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
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
        assert(e.message == Some(Resources("expectedTypeErrorButGotNone", "" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
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
        val errMsg = Resources("expectedTypeErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0, "error message was: " + e.message.get)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
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
        assert(e.message == Some(Resources("expectedCompileErrorButGotNone", "val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should do nothing when parse failed") {
        assertDoesNotCompile("println(\"test)")
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
        assert(e.message == Some(Resources("expectedCompileErrorButGotNone", "" + Prettifier.lineSeparator + "val a = 1" + Prettifier.lineSeparator + "")))
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
        val errMsg = Resources("expectedNoErrorButGotTypeError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("val a: String = 2") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          assertCompiles("println(\"test)")
        }
        val errMsg = Resources("expectedNoErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
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
        val errMsg = Resources("expectedNoErrorButGotTypeError", "", "")
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
        val errMsg = Resources("expectedNoErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 10)))
      }
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
      val a1: Array[Any] = Array(1, Array("a", "b"), 3)
      val a2: Array[Any] = Array(1, Array("a", "b"), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1) { a2 }
      intercept[TestFailedException] {
        assertResult(a1) { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1: Array[Any] = Array(1, Array("a", null), 3)
      val a2: Array[Any] = Array(1, Array("a", null), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
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
      intercept[TestFailedException] {
        assertResult(n1) { a1 }
      }
      intercept[TestFailedException] {
        assertResult(a1) { n1 }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        assertResult(a) { null }
      }
      assert(e1.message === Some(FailureMessages("expectedButGot", a, null)))
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
      val a1: Array[Any] = Array(1, Array("a", "b"), 3)
      val a2: Array[Any] = Array(1, Array("a", "b"), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
      assert(a1 ne a2)
      assertResult(a1, "a clue") { a2 }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { a3 }
      }
    }
    it("should compare arrays containing nulls fine") {
      val a1: Array[Any] = Array(1, Array("a", null), 3)
      val a2: Array[Any] = Array(1, Array("a", null), 3)
      val a3: Array[Any] = Array(1, Array("c", "d"), 3)
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
      intercept[TestFailedException] {
        assertResult(n1, "a clue") { a1 }
      }
      intercept[TestFailedException] {
        assertResult(a1, "a clue") { n1 }
      }
      val a = "hi"
      val e1 = intercept[TestFailedException] {
        assertResult(a, "a clue") { null }
      }
      assert(e1.message === Some(FailureMessages("expectedButGot", a, null) + " a clue"))
    }
    it("should append clues in a satisfying manner") {
      val a = "hi"
      val b = "ho"
      val aDiff = "h[i]"
      val bDiff = "h[o]"
      val e1 = intercept[TestFailedException] {
        assertResult(a, "the clue") { b }
      }
      assert(e1.message === Some(FailureMessages("expectedButGot", aDiff, bDiff) + " the clue"))

      val e2 = intercept[TestFailedException] {
        assertResult(a, ", the clue") { b }
      }
      assert(e2.message === Some(FailureMessages("expectedButGot", aDiff, bDiff) + ", the clue"))

      val e3 = intercept[TestFailedException] {
        assertResult(a, ". the clue") { b }
      }
      assert(e3.message === Some(FailureMessages("expectedButGot", aDiff, bDiff) + ". the clue"))

      val e4 = intercept[TestFailedException] {
        assertResult(a, "; the clue") { b }
      }
      assert(e4.message === Some(FailureMessages("expectedButGot", aDiff, bDiff) + "; the clue"))
    }
  }
}
