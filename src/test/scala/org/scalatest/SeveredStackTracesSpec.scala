/*
 * Copyright 2001-2008 Artima, Inc.
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
/* Uncomment after remove type aliases in org.scalatest package object
import org.scalatest.exceptions.TestFailedException
*/

class SeveredStackTracesSpec extends FunSpec with ShouldMatchers with SeveredStackTraces {

  describe("A severed TestFailedException") {

    it("should give the proper line on fail()") {
      try {
        fail()
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("fail() didn't produce a file name and line number string: " + e.failedCodeFileNameAndLineNumberString, e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("fail() didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on fail(\"message\")") {
      try {
        fail("some message")
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("fail(\"some message\") didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("fail(\"some message\") didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on fail(throwable)") {
      try {
        fail(new RuntimeException)
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("fail(throwable) didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("fail(throwable) didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on fail(\"some message\", throwable)") {
      try {
        fail("some message", new RuntimeException)
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("fail(\"some message\", throwable) didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("fail(\"some message\", throwable) didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assert(false)") {
      try {
        assert(false)
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assert(false) didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assert(false) didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assert(false, \"some message\")") {
      try {
        assert(false, "some message")
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assert(false, \"some message\") didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assert(false, \"some message\") didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assert(1 === 2)") {
      try {
        assert(1 === 2)
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assert(1 === 2) didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assert(1 === 2) didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assert(1 === 2, \"some message\")") {
      try {
        assert(1 === 2, "some message")
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assert(1 === 2, \"some message\") didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assert(1 === 2, \"some message\") didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assertResult(1) { 2 }") {
      try {
        assertResult(1) { 2 }
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assertResult(1) { 2 } didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assertResult(1) { 2 } didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on assertResult(1, \"some message\") { 2 }") {
      try {
        assertResult(1, "some message") { 2 }
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("assertResult(1, \"some message\") { 2 } didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("assertResult(1, \"some message\") { 2 } didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on intercept[IllegalArgumentException] {}") {
      try {
        intercept[IllegalArgumentException] {}
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("intercept[IllegalArgumentException] {} didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("intercept[IllegalArgumentException] {} didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on intercept[IllegalArgumentException] { throw new RuntimeException }") {
      try {
        intercept[IllegalArgumentException] { if (false) 1 else throw new RuntimeException }
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on 1 should be === 2") {
      try {
        1 should be === 2
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              if (s != ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))) {
                fail("s was: " + s, e)
              }
              checkFileNameAndLineNumber(e, s)
            case None => fail("1 should be === 2 didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (4)
        case e: Throwable =>
          fail("1 should be === 2 didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on evaluating {} should produce [IllegalArgumentException] {}") {
      try {
        evaluating {} should produce [IllegalArgumentException]
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => // s should equal ("SeveredStackTracesSpec.scala:" + (baseLineNumber + 204))
              if (s != ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))) {
                fail("s was: " + s, e)
              }
              checkFileNameAndLineNumber(e, s)
            case None => fail("evaluating {} should produce [IllegalArgumentException] didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (3)
        case e: Throwable =>
          fail("evaluating {} should produce [IllegalArgumentException] didn't produce a TestFailedException", e)
      }
    }

    it("should give the proper line on evaluating { throw new RuntimeException } should produce [IllegalArgumentException]") {
      try {
        evaluating { if (false) 1 else throw new RuntimeException } should produce [IllegalArgumentException]
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              s should equal ("SeveredStackTracesSpec.scala:" + (thisLineNumber - 6))
              checkFileNameAndLineNumber(e, s)
            case None => fail("evaluating { throw new RuntimeException } should produce [IllegalArgumentException] didn't produce a file name and line number string", e)
          }
          e.failedCodeStackDepth should equal (3)
        case e: Throwable =>
          fail("evaluating { throw new RuntimeException } should produce [IllegalArgumentException] didn't produce a TestFailedException", e)
      }
    }

    it("should return the cause in both cause and getCause") {
      val theCause = new IllegalArgumentException("howdy")
      val tfe = new TestFailedException(Some("doody"), Some(theCause), 3)
      assert(tfe.cause.isDefined)
      assert(tfe.cause.get === theCause)
      assert(tfe.getCause == theCause)
    }

    it("should return None in cause and null in getCause if no cause") {
      val tfe = new TestFailedException(Some("doody"), None, 3)
      assert(tfe.cause.isEmpty)
      assert(tfe.getCause == null)
    }
  }
  private def checkFileNameAndLineNumber(e: TestFailedException, failedCodeFileNameAndLineNumberString: String) {
    val stackTraceElement = e.getStackTrace()(e.failedCodeStackDepth)
    val fileName = stackTraceElement.getFileName
    val lineNumber = stackTraceElement.getLineNumber
    failedCodeFileNameAndLineNumberString should equal (fileName + ":" + lineNumber)
  }
  
  //
  // Returns the line number from which this method was called.
  //
  // Found that on some machines it was in the third element in the stack
  // trace, and on others it was the fourth, so here we check the method
  // name of the third element to decide which of the two to return.
  //
  private def thisLineNumber = {
    val st = Thread.currentThread.getStackTrace

    if (!st(2).getMethodName.contains("thisLineNum"))
      st(2).getLineNumber
    else
      st(3).getLineNumber
  }
}
