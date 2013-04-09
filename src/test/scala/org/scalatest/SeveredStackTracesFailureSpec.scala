/*
 * Copyright 2001-2011 Artima, Inc.
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

import org.scalatest.matchers.ShouldMatchers

class SeveredStackTracesFailureSpec extends FunSpec with ShouldMatchers with SeveredStackTraces {

  override def withFixture(test: NoArgTest): Outcome = {
    super.withFixture(test) match {
      case Failed(e: TestFailedException) => 
        e.failedCodeStackDepth should equal (0)
        e.failedCodeFileNameAndLineNumberString match {
          case Some(s) =>
            checkFileNameAndLineNumber(e, s)
          case None => fail("TestFailedException didn't contain file name and line number string", e)
        }
        Succeeded
      case other => other
    }
  }

  describe("A severed TestFailedException") {

    it("should be properly severed on fail()") {
      fail()
    }

    it("should be properly severed on fail(\"message\")") {
      fail("some message")
    }

    it("should be properly severed on fail(throwable)") {
      fail(new RuntimeException)
    }

    it("should be properly severed on fail(\"some message\", throwable)") {
      fail("some message", new RuntimeException)
    }

    it("should be properly severed on assert(false)") {
      assert(false)
    }

    it("should be properly severed on assert(false, \"some message\")") {
      assert(false, "some message")
    }

    it("should be properly severed on assert(1 === 2)") {
      assert(1 === 2)
    }

    it("should be properly severed on assert(1 === 2, \"some message\")") {
      assert(1 === 2, "some message")
    }

    it("should be properly severed on assertResult(1) { 2 }") {
      assertResult(1) { 2 }
    }

    it("should be properly severed on assertResult(1, \"some message\") { 2 }") {
      assertResult(1, "some message") { 2 }
    }

    it("should be properly severed on intercept[IllegalArgumentException] {}") {
      intercept[IllegalArgumentException] {}
    }

    it("should be properly severed on intercept[IllegalArgumentException] { throw new RuntimeException }") {
      intercept[IllegalArgumentException] { if (false) 1 else throw new RuntimeException }
    }

    it("should be properly severed on 1 should be === 2") {
      1 should be === 2
    }

    it("should be properly severed on evaluating {} should produce [IllegalArgumentException] {}") {
      evaluating {} should produce [IllegalArgumentException]
    }

    it("should be properly severed on evaluating { throw new RuntimeException } should produce [IllegalArgumentException]") {
      evaluating { if (false) 1 else throw new RuntimeException } should produce [IllegalArgumentException]
    }
  }
  private def checkFileNameAndLineNumber(e: TestFailedException, failedCodeFileNameAndLineNumberString: String) {
    val stackTraceElement = e.getStackTrace()(e.failedCodeStackDepth)
    val fileName = stackTraceElement.getFileName
    val lineNumber = stackTraceElement.getLineNumber
    failedCodeFileNameAndLineNumberString should equal (fileName + ":" + lineNumber)
  }
}
