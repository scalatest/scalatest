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
package exceptions

import org.scalatest.matchers.ShouldMatchers

class TestFailedExceptionSpec extends FunSpec with ShouldMatchers {

  val baseLineNumber = 23

  describe("The TestFailedException") {

    it("should give the proper line on fail()") {
      try {
        fail()
      }
      catch {
        case e: TestFailedException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 6))
            case None => fail("fail() didn't produce a file name and line number string: " + e.failedCodeFileNameAndLineNumberString, e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 21))
            case None => fail("fail(\"some message\") didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 36))
            case None => fail("fail(throwable) didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 51))
            case None => fail("fail(\"some message\", throwable) didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 66))
            case None => fail("assert(false) didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 81))
            case None => fail("assert(false, \"some message\") didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 96))
            case None => fail("assert(1 === 2) didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 111))
            case None => fail("assert(1 === 2, \"some message\") didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 126))
            case None => fail("assertResult(1) { 2 } didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 141))
            case None => fail("assertResult(1, \"some message\") { 2 } didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 156))
            case None => fail("intercept[IllegalArgumentException] {} didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 171))
            case None => fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a file name and line number string", e)
          }
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
              if (s != ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 186))) {
                fail("s was: " + s, e)
              }
            case None => fail("1 should be === 2 didn't produce a file name and line number string", e)
          }
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
            case Some(s) => // s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 204))
            if (s != ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 204))) {
                fail("s was: " + s, e)
              }
            case None => fail("evaluating {} should produce [IllegalArgumentException] didn't produce a file name and line number string", e)
          }
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
            case Some(s) => s should equal ("TestFailedExceptionSpec.scala:" + (baseLineNumber + 222))
            case None => fail("evaluating { throw new RuntimeException } should produce [IllegalArgumentException] didn't produce a file name and line number string", e)
          }
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

    it("should be equal to itself") {
      val tfe = new TestFailedException(Some("doody"), None, 3)
      assert(tfe equals tfe)
    }
  }
}
 
