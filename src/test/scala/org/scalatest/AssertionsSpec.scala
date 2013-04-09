/*
 * Copyright 2001-2008 Artima, Inc.
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

class AssertionsSpec extends FunSpec with OptionValues {

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
}
