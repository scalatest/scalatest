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

class AssertionsSpec extends FunSpec with OptionValues {
  
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
    
    /*it("should do nothing when is used to check a > 2") {
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
    }*/
    
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
  }

  describe("The assert(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

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

    /*it("should do nothing when is used to check a > 2") {
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
    }*/

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

    /*it("should do nothing when is used to check a > 2") {
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
    }*/

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
  }

  describe("The assume(boolean, clue) method") {
    val a = 3
    val b = 5

    val bob = "bob"
    val alice = "alice"

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

    /*it("should do nothing when is used to check a > 2") {
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
    }*/

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
}
