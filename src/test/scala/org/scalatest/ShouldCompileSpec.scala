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

    describe("when work with string literal") {

      it("should do nothing when type check passed") {
        "val a = 1" should compile
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          "val a: String = 2" should compile
        }
        val errMsg = Resources("expectedNoErrorButGotTypeError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("val a: String = 2") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          "println(\"test)" should compile
        }
        val errMsg = Resources("expectedNoErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }

    describe("when work with triple quotes string literal with stripMargin") {

      it("should do nothing when type check passed") {
        """
          |val a = 1
          |""".stripMargin should compile
      }

      it("should throw TestFailedException with correct message and stack depth when type check failed") {
        val e = intercept[TestFailedException] {
          """
            |val a: String = 2
            |""".stripMargin should compile
        }
        val errMsg = Resources("expectedNoErrorButGotTypeError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("val a: String = 2") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          """
            |println("test)
            |""".stripMargin should compile
        }
        val errMsg = Resources("expectedNoErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }
  }
}
