/*
 * Copyright 2001-2014 Artima, Inc.
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

import Matchers._
import SharedHelpers.thisLineNumber

class ShouldCompileSpec extends FunSpec {

  val fileName = "ShouldCompileSpec.scala"

  describe("Compile matcher") {

    it("should do nothing when type check passed") {
      "val a = 1" should compile
    }

    it("should throw TestFailedException with correct message and stack depth when type check failed") {
      val e = intercept[TestFailedException] {
        "val a: String = 2" should compile
      }
      assert(e.message.get.startsWith("val a: String = 2 encountered a type error:"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

    it("should throw TestFailedException with correct message and stack depth when parse failed") {
      val e = intercept[TestFailedException] {
        "println(\"test)" should compile
      }
      assert(e.message.get.startsWith("println(\"test) encountered a parse error:"))
      assert(e.failedCodeFileName === (Some(fileName)))
      assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
    }

  }

}