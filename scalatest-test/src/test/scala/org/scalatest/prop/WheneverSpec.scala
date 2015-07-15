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
import org.scalatest.exceptions.GeneratorDrivenPropertyCheckFailedException

class WheneverSpec extends FunSpec with Matchers with Whenever {

  describe("The whenever construct") {
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
  }
}
