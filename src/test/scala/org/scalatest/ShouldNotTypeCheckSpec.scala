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
import org.scalactic.Prettifier.lineSeparator

class ShouldNotTypeCheckSpec extends FunSpec {

  val fileName = "ShouldNotTypeCheckSpec.scala"

  describe("TypeCheck matcher") {

    describe("when work with string literal") {

      it("should do nothing when type check failed") {
        "val a: String = 2" shouldNot typeCheck
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          "val a = 1" shouldNot typeCheck
        }
        assert(e.message == Some(Resources("expectedTypeErrorButGotNone", "val a = 1")))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed") {
        val e = intercept[TestFailedException] {
          "println(\"test)" shouldNot typeCheck
        }
        val errMsg = Resources("expectedTypeErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }

    describe("when work with triple quotes string literal with stripMargin") {

      it("should do nothing when type check failed") {
        """
          |val a: String = 2
          |""".stripMargin shouldNot typeCheck
      }

      it("should throw TestFailedException with correct message and stack depth when type check passed") {
        val e = intercept[TestFailedException] {
          """
            |val a = 1
            |""".stripMargin shouldNot typeCheck
        }
        assert(e.message == Some(Resources("expectedTypeErrorButGotNone", lineSeparator + "val a = 1" + lineSeparator)))
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 4)))
      }

      it("should throw TestFailedException with correct message and stack depth when parse failed ") {
        val e = intercept[TestFailedException] {
          """
            |println("test)
            |""".stripMargin shouldNot typeCheck
        }
        val errMsg = Resources("expectedTypeErrorButGotParseError", "", "")
        assert(e.message.get.startsWith(errMsg.substring(0, errMsg.indexOf(':'))))
        assert(e.message.get.indexOf("println(\"test)") >= 0)
        assert(e.failedCodeFileName === (Some(fileName)))
        assert(e.failedCodeLineNumber === (Some(thisLineNumber - 6)))
      }
    }
  }
}
