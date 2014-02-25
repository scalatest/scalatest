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

/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

import SharedHelpers.thisLineNumber
import scala.util.Failure
import scala.util.Success
import Assertions.NormalResult
import org.scalatest.exceptions.TestCanceledException
import OptionValues._

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
      assert(e1.message === Some(FailureMessages("didNotEqual", a, null)))
    }
  }
  describe("The intercept method") {
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

    it("should return the caught exception") {
      val e = new RuntimeException
      val result = intercept[RuntimeException] {
        throw e
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      assert(result eq e)
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

  def wasNotTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    quoteString(left) + " was not the same instance as " + quoteString(right)

  def wasTheSameInstanceAs(left: AnyRef, right: AnyRef): String =
    quoteString(left) + " was the same instance as " + quoteString(right)

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

    override def toString: String = value.toString
  }

  private def neverRuns1(f: => Unit): Boolean = true
  private def neverRuns2(f: => Unit)(a: Int): Boolean = true
  private def neverRuns3[T](f: => Unit)(a: T): Boolean = true
  
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
      val e = intercept[TestFailedException] { 
        assert(a == null) 
      }
      assert(e.message === Some(didNotEqual(3, null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }
    
    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] { 
        assert(null == a) 
      }
      assert(e.message === Some(didNotEqual(null, 3)))
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
    
    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalautils.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.
    
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
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}"))))
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
      assert(e.message == Some(commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5))))
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

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)

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

  }

  describe("The assert(boolean, clue) method") {
    val a = 3
    val b = 5

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

    it("should throw TestFailedException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestFailedException] {
        assert(a == null, ". dude")
      }
      assert(e.message === Some(didNotEqual(3, null) + ". dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestFailedException] {
        assert(null == a, "; dude")
      }
      assert(e.message === Some(didNotEqual(null, 3) + "; dude"))
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

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalautils.TripleEquals does not seems to work
    // Should make Assertions to extend TripleEquals instead of LegacyTripleEquals instead.

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
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")) + ", dude"))
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
      assert(e.message == Some(commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)) + ", dude"))
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

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)

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
      val e = intercept[TestCanceledException] {
        assume(a == null)
      }
      assert(e.message === Some(didNotEqual(3, null)))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestCanceledException] {
        assume(null == a)
      }
      assert(e.message === Some(didNotEqual(null, 3)))
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

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalautils.TripleEquals does not seems to work
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
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}"))))
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
      assert(e.message == Some(commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5))))
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

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)

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
  }

  describe("The assume(boolean, clue) method") {
    val a = 3
    val b = 5

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

    it("should throw TestCanceledException with correct message and stack depth when is used to check a == null") {
      val e = intercept[TestCanceledException] {
        assume(a == null, ", dude")
      }
      assert(e.message === Some(didNotEqual(3, null) + ", dude"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when is used to check null == a") {
      val e = intercept[TestCanceledException] {
        assume(null == a, ". dude")
      }
      assert(e.message === Some(didNotEqual(null, 3) + ". dude"))
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

    // TripleEquals tests
    // currently these tests are not calling TripleEquals's === and !== yet, import org.scalautils.TripleEquals does not seems to work
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
      assert(e.message == Some(commaBut(equaled(3, 3), wasFalse("{\n  scala.this.Predef.println(\"hi\");\n  b.==(3)\n}")) + ", dude"))
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
      assert(e.message == Some(commaBut(wasTrue("{\n  scala.this.Predef.println(\"hi\");\n  b.==(5)\n}"), didNotEqual(3, 5)) + ", dude"))
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

    val ci1 = new CustomInt(123)
    val ci2 = new CustomInt(321)
    val ci3 = ci1

    val l1 = List(1, 2, 3)

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

  }

  describe("assertTypeError method") {

    it("should do nothing when type check failed") {
      assertTypeError("val a: String = 1")
    }

    it("should throw TestFailedException with correct message and stack depth when type check passed") {
      val e = intercept[TestFailedException] {
        assertTypeError("val a = 1")
      }
      assert(e.message == Some("Expected a type error, but got none for: val a = 1"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when parse failed") {
      val e = intercept[TestFailedException] {
        assertTypeError("println(\"test)")
      }
      assert(e.message.get.startsWith("Expected type error, but get parse error: "))
      assert(e.message.get.endsWith("\nfor: println(\"test)"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 5)))
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
