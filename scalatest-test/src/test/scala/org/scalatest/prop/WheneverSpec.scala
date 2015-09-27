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
        it("should pass the result of the block through") {
          import Expectations._
          val x = 1
          val yes = expect(x == 1)
          val no = expect(x == 2)
          val res1 = whenever (true) { yes }
          assert(res1 eq yes)
          val res2 = whenever (true) { no }
          assert(res2 eq no)
        }
      }
      describe("when the Boolean condition is false") {
        it("should return a vacuous Yes") {
          import Expectations._
          val x = 1
          val yes = expect(x == 1)
          val no = expect(x == 2)
          val res1 = whenever (false) { yes }
          assert(res1.isYes)
          assert(res1.isVacuousYes)
          val res2 = whenever (false) { no }
          assert(res2.isYes)
          assert(res2.isVacuousYes)
        }
      }
    }
    describe("when the result type of the block is anything other than Expectation") {
      describe("when the Boolean condition is true") {
        it("should return Succeeded or throw an exception") {
          val res1 = whenever (true) { assert(1 == 1) }
          assert(res1 eq Succeeded)
          assertThrows[TestFailedException] {
            whenever (true) { assert(1 == 2) }
          }
          val res2 = whenever (true) { 1 }
          assert(res2 eq Succeeded)
          val res3 = whenever (true) { () }
          assert(res3 eq Succeeded)
          assertThrows[StringIndexOutOfBoundsException] {
            whenever (true) { "hi".charAt(-1) }
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
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { 1 }
          }
          assertThrows[DiscardedEvaluationException] {
            whenever (false) { () }
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
