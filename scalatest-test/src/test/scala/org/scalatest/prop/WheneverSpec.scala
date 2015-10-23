/*
3* Copyright 2001-2015 Artima, Inc.
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
import org.scalacheck._
import org.scalacheck.util.Pretty
import Arbitrary._
import Prop._
import org.scalatest.Matchers._
import org.scalatest.SharedHelpers.thisLineNumber
import org.scalatest.exceptions.TestFailedException
import org.scalatest.exceptions.DiscardedEvaluationException
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException

class WheneverSpec extends FunSpec with Matchers with Whenever {

  describe("The whenever construct") {
    describe("when the result type of the block is Expectation") {
      describe("when the Boolean condition is true") {
        ignore("should, under Compatibility, either return Succeeded or throw TFE") { // Unignore after we uncomment the expectation implicits in RegistrationPolicy
          import Expectations._
          val res1 = whenever (true) { expect(1 == 1); succeed }
          assert(res1 == AssertionValue)
          assertThrows[TestFailedException] {
            whenever (true) { expect(1 == 2); succeed }
          }
        }
      }
      describe("when the Boolean condition is false") {
        it("should throw DiscardedEvaluationException") {
          import Expectations._
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { expect(1 == 1); succeed }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { expect(1 == 2); succeed }
          }
        }
      }
    }
    describe("when the result type of the block is anything other than Expectation") {
      describe("when the Boolean condition is true") {
        it("should return Succeeded or throw an exception") {
          val res1 = whenever (true) { assert(1 == 1) }
          assert(res1 == AssertionValue)
          assertThrows[TestFailedException] {
            whenever (true) { assert(1 == 2) }
          }
          val res2 = whenever (true) { 1; succeed }
          assert(res2 == AssertionValue)
          val res3 = whenever (true) { (); succeed }
          assert(res3 == AssertionValue)
          // SKIP-SCALATESTJS-START
          assertThrows[StringIndexOutOfBoundsException] {
            whenever (true) { "hi".charAt(-1); succeed }
          }
          // SKIP-SCALATESTJS-END
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
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { 1; succeed }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { (); succeed }
          }
        }
      }
    }
/*
    it("should infer the result type from the block, and pass the value through") {

      val x = 1
      val s = "hi"

      whenever(true) { x } shouldBe 1
      (whenever(true) { x }: Int) shouldBe an [Integer]

      whenever(true) { s } shouldBe "hi"
      (whenever(true) { s }: String) shouldBe a [String]

      whenever(true) { assert(x == 1) } shouldBe Succeeded
      (whenever(true) { assert(x == 1) }: Assertion) shouldBe an [Assertion]
    }
*/
  }
}
