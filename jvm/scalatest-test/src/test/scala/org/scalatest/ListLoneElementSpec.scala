/*
 * Copyright 2001-2025 Artima, Inc.
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

class ListLoneElementSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default
  
  def wasNotGreaterThan(left: Any, right: Any): String = 
    decorateToStringValue(prettifier, left) + " was not greater than " + (right)
    
  def notLoneElement(left: Any, size: Int): String = 
    "Expected " + decorateToStringValue(prettifier, left) + " to contain exactly 1 element, but it has size " + size

  describe("The loneElement syntax") {
    
    describe("when used with List") {
      it("should work with xs.loneElement and passed when should syntax is used and xs only contains one element and the one element passed the check") {
        List(10).loneElement should be > 9
      }
      
      it("should work with xs.loneElement and passed when assert syntax is used and xs only contains one element and the one element passed the check") {
        assert(List(10).loneElement > 9)
      }
      
      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs.loneElement contains one element but it failed the check") {
        val e = intercept[exceptions.TestFailedException] {
          List(8).loneElement should be > 9
        }
        e.failedCodeFileName should be (Some("ListLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(wasNotGreaterThan(8, 9)))
      }
      
      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs.loneElement contains one element but it failed the check") {
        val e = intercept[exceptions.TestFailedException] {
          assert(List(8).loneElement > 9)
        }
        assert(e.failedCodeFileName == Some("ListLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber == Some(thisLineNumber - 3))
        assert(e.message == Some(wasNotGreaterThan(8, 9)))
      }
      
      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains 0 element and xs.loneElement is called") {
        val xs = List.empty[Int]
        val e = intercept[exceptions.TestFailedException] {
          xs.loneElement should be > 9
        }
        e.failedCodeFileName should be (Some("ListLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(notLoneElement(xs, 0)))
      }
      
      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains 0 element and xs.loneElement is called") {
        val xs = List.empty[Int]
        val e = intercept[exceptions.TestFailedException] {
          assert(xs.loneElement > 9)
        }
        assert(e.failedCodeFileName == Some("ListLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber == Some(thisLineNumber - 3))
        assert(e.message === Some(notLoneElement(xs, 0)))
      }
      
      it("should throw TestFailedException with correct stack depth and message when should syntax is used and xs contains > 1 elements and xs.loneElement is called") {
        val xs = List(10, 12)
        val e = intercept[exceptions.TestFailedException] {
          xs.loneElement should be > 9
        }
        e.failedCodeFileName should be (Some("ListLoneElementSpec.scala"))
        e.failedCodeLineNumber should be (Some(thisLineNumber - 3))
        e.message should be (Some(notLoneElement(xs, 2)))
      }
      
      it("should throw TestFailedException with correct stack depth and message when assert syntax is used and xs contains > 1 elements and xs.loneElement is called") {
        val xs = List(10, 12)
        val e = intercept[exceptions.TestFailedException] {
          assert(xs.loneElement > 9)
        }
        assert(e.failedCodeFileName === Some("ListLoneElementSpec.scala"))
        assert(e.failedCodeLineNumber === Some(thisLineNumber - 3))
        assert(e.message === Some(notLoneElement(xs, 2)))
      }
      it("should allow parentheses to be immediately applied on the result") {
        val xs = List(List(1, 2, 3))
        assert(xs.loneElement(1) === 2)
      }
    }
  }
}
