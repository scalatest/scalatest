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

import Matchers._
import SharedHelpers.thisLineNumber

class ShouldNotCompileSpec extends FunSpec {

  val fileName = "ShouldNotCompileSpec.scala"

  describe("Compile matcher") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        "val a: String = 2" shouldNot compile
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          "val a = 1" shouldNot compile
        }
        assert(e.message == Some("Expected a type error, but got none for: val a = 1"))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          "println(\"test)" shouldNot compile
        }
        assert(e.message.get.startsWith("Expected type error, but get parse error: "))
        assert(e.message.get.endsWith("\nfor: println(\"test)"))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 5)))
      }
    }

    describe("when work with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        """
          |val a: String = 2
          |""".stripMargin shouldNot compile
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          """
            |val a = 1
            |""".stripMargin shouldNot compile
        }
        assert(e.message == Some(
          """Expected a type error, but got none for: 
            |val a = 1
            |""".stripMargin))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 7)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed ") {
        val e = intercept[TestFailedException] {
          """
            |println(\"test)
            |""".stripMargin shouldNot compile
        }
        assert(e.message.get.startsWith("Expected type error, but get parse error: "))
        assert(e.message.get.endsWith(
          """for: 
            |println(\"test)
            |""".stripMargin))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 8)))
      }
    }


  }

}
