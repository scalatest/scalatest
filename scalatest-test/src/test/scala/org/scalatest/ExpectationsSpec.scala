/*
 * Copyright 2001-2015 Artima, Inc.
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

class ExpectationsSpec extends FunSpec with Expectations {

  describe("The expectThrows method") {
    it("should catch subtypes") {
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      assert(
        (
          expectThrows[MyException] {
            throw new MyException
          } &&
          expectThrows[MyException] {
            throw new MyExceptionSubClass
          } && {
            // Try with a trait
            trait MyTrait {
              def someRandomMethod() {}
            }
            class AnotherException extends RuntimeException with MyTrait
            expectThrows[MyTrait] {
              throw new AnotherException
            }
          }
        ).isTrue
      )
    }
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the TFE's cause") { pending // Dang, need to an a cause: Option[Throwabe] to Expectation
/*
        val wrongException = new RuntimeException("oops!")
        val caught =
          expectThrows[TestFailedException] {
            expectThrows[IllegalArgumentException] {
              throw wrongException
            }
          }
        assert(caught.cause.value eq wrongException)
*/
      }
    }
    it("should catch subtypes of the given exception type") { pending
/*
      class MyException extends RuntimeException
      class MyExceptionSubClass extends MyException
      expectThrows[MyException] {
        throw new MyException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      expectThrows[MyException] {
        throw new MyExceptionSubClass
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Try with a trait
      trait MyTrait {
        def someRandomMethod() {}
      }
      class AnotherException extends RuntimeException with MyTrait
      val caught = expectThrows[MyTrait] {
        throw new AnotherException
        new AnyRef // This is needed because right now Nothing doesn't overload as an Any
      }
      // Make sure the result type is the type passed in, so I can 
      // not cast and still invoke any method on it I want
      caught.someRandomMethod()
*/
    }
  }
}
