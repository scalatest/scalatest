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
package org.scalatest.prop

import org.scalatest._
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class WheneverSpec extends AnyFunSpec with Matchers with Whenever {

  describe("The whenever construct") {
    describe("when the result type of the block is Expectation") {
      describe("when the Boolean condition is true") {
        // TODO: Re-enable this when we bring in expectations.Expectations.
        /*it("should pass the result of the block through") {

          import expectations.Expectations._
          val x = 1
          val yes = expect(x == 1)
          val no = expect(x == 2)
          val res1 = whenever (true) { yes }
          assert(res1 == yes)
          val res2 = whenever (true) { no }
          assert(res2 == no)

          assertThrows[java.lang.Exception] {
            whenever (true) { throw new java.lang.Exception; expect(x == 1) }
          }
          // SKIP-SCALATESTJS,NATIVE-START
          assertThrows[StringIndexOutOfBoundsException] {
            whenever (true) { "hi".charAt(-1); expect(x == 1) }
          }
          // SKIP-SCALATESTJS,NATIVE-END
        }*/
      }
      describe("when the Boolean condition is false") {
        it("should throw DiscardedEvaluationException") {
          import expectations.Expectations._
          val x = 1
          val yes = expect(x == 1)
          val no = expect(x == 2)
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { yes }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { no }
          }
        }
      }
    }
    describe("when the result type of the block is Assertion") {
      describe("when the Boolean condition is true") {
        it("should return Succeeded or throw an exception") {
          val res1 = whenever (true) { assert(1 == 1) }
          assert(res1 == Succeeded)
          assertThrows[TestFailedException] {
            whenever (true) { assert(1 == 2) }
          }
          assertThrows[java.lang.Exception] {
            whenever (true) { throw new java.lang.Exception; succeed }
          }
          // SKIP-SCALATESTJS,NATIVE-START
          assertThrows[StringIndexOutOfBoundsException] {
            whenever (true) { "hi".charAt(-1); succeed }
          }
          // SKIP-SCALATESTJS,NATIVE-END
          assertThrows[TestFailedException] {
            whenever (true) { fail() }
          }
          assertThrows[TestCanceledException] {
            whenever (true) { cancel() }
          }
        }
      }
      describe("when the Boolean condition is false") {
        it("should throw DiscardedEvaluationException") {
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { assert(1 == 1) }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { assert(1 == 2) }
          }
        }
      }
    }
    describe("when the result type of the block is anything other than Expectation or Assertion") {
      describe("when the Boolean condition is true") {
        it("should return the Unit value or throw an exception") {
          val res1 = whenever (true) { Predef.assert(1 == 1) }
          assert(res1 == ())
          assertThrows[TestFailedException] {
            whenever (true) { assert(1 == 2); () }
          }
          assertThrows[AssertionError] {
            whenever (true) { Predef.assert(1 == 2) }
          }
          assertThrows[java.lang.Exception] {
            whenever (true) { throw new java.lang.Exception; 33 }
          }
          val res2 = whenever (true) { 1 }
          assert(res2 == ())
          val res3 = whenever (true) { () }
          assert(res3 == ())
          val res4 = whenever (true) { 1 == 1 }
          assert(res2 == ())
          // SKIP-SCALATESTJS,NATIVE-START
          assertThrows[StringIndexOutOfBoundsException] {
            whenever (true) { "hi".charAt(-1) }
          }
          // SKIP-SCALATESTJS,NATIVE-END
          assertThrows[TestFailedException] {
            whenever (true) { fail() }
          }
          assertThrows[TestCanceledException] {
            whenever (true) { cancel() }
          }
        }
      }
      describe("when the Boolean condition is false") {
        it("should throw DiscardedEvaluationException") {
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { Predef.assert(1 == 1) }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { Predef.assert(1 == 2) }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { 1 }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { () }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { 1 == 1 }
          }
        }
      }
    }
  }
}
