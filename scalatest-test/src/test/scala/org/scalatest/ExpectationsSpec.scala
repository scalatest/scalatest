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

import exceptions.TestFailedException
import OptionValues._

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
    it("should include the expected exception as the cause so that it will be there if negated") {
        val expectedException = new IllegalArgumentException("I meant to do that!")
        val fact =
          expectThrows[IllegalArgumentException] {
            throw expectedException
          }
        assert(fact.cause.value eq expectedException)
        assert((!fact).asInstanceOf[Fact.Unary_!].underlying.cause.value eq expectedException)
    }
    describe("when the bit of code throws the wrong exception") {
      it("should include that wrong exception as the Fact's cause") {
        val wrongException = new RuntimeException("oops!")
        val fact =
          expectThrows[IllegalArgumentException] {
            throw wrongException
          }
        assert(fact.cause.value eq wrongException)
      }
    }
  }
}
