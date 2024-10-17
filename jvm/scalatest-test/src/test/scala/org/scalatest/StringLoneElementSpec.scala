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
package org.scalatest

import SharedHelpers._
import FailureMessages.decorateToStringValue
import LoneElement._
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class StringLoneElementSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  def didNotEqual(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " did not equal " + decorateToStringValue(prettifier, right)

  def wasNotEqualTo(left: Any, right: Any): String =
    decorateToStringValue(prettifier, left) + " was not equal to " + decorateToStringValue(prettifier, right)

  def notLoneElement(left: Any, size: Int): String =
    "Expected " + decorateToStringValue(prettifier, left) + " to contain exactly 1 element, but it has size " + size

  describe("The loneElement syntax") {

    describe("when used with List") {
      it("should work with xs.loneElement and passed when should syntax is used and xs only contains one element and the one element passed the check") {
        "9".loneElement should be ('9')
      }

      it("should work with xs.loneElement and passed when assert syntax is used and xs only contains one element and the one element passed the check") {
        assert("9".loneElement == '9')
      }

      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs.loneElement contains one element but it failed the check") {
        val e = intercept[exceptions.TestFailedException] {
          "8".loneElement should be ('9')
        }
        e.failedCodeFileName should be (Some("StringLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(wasNotEqualTo('8', '9')))
      }

      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs.loneElement contains one element but it failed the check") {
        val e = intercept[exceptions.TestFailedException] {
          assert("8".loneElement == '9')
        }
        assert(e.failedCodeFileName === Some("StringLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
        assert(e.message === Some(didNotEqual('8', '9')))
      }

      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains 0 element and xs.loneElement is called") {
        val e = intercept[exceptions.TestFailedException] {
          "".loneElement should be ('9')
        }
        e.failedCodeFileName should be (Some("StringLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(notLoneElement("", 0)))
      }

      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains 0 element and xs.loneElement is called") {
        val e = intercept[exceptions.TestFailedException] {
          assert("".loneElement == '9')
        }
        assert(e.failedCodeFileName == Some("StringLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber == Some(thisLineNumber - 3))
        assert(e.message === Some(notLoneElement("", 0)))
      }

      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains > 1 elements and xs.loneElement is called") {
        val e = intercept[exceptions.TestFailedException] {
          "28".loneElement should be ('9')
        }
        e.failedCodeFileName should be (Some("StringLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(notLoneElement("28", 2)))
      }

      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains > 1 elements and xs.loneElement is called") {
        val xs = List(10, 12)
        val e = intercept[exceptions.TestFailedException] {
          assert("28".loneElement == '9')
        }
        assert(e.failedCodeFileName === Some("StringLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
        assert(e.message === Some(notLoneElement("28", 2)))
      }
    }
  }
}
