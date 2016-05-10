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
package org.scalatest.exceptions

import org.scalatest.FunSpec
import org.scalatest.Matchers
import org.scalactic.source
import org.scalatest.SharedHelpers.thisLineNumber

class TestCanceledExceptionSpec extends FunSpec with Matchers {

  val baseLineNumber = thisLineNumber

  describe("The TestCanceledException") {

    it("should give the proper line on cancel()") {
      try {
        cancel()
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 6))
            case None => fail("cancel() didn't produce a file name and line number string: " + e.failedCodeFileNameAndLineNumberString, e)
          }
        case e: Throwable =>
          cancel("cancel() didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on cancel(\"message\")") {
      try {
        cancel("some message")
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 21))
            case None => fail("cancel(\"some message\") didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("cancel(\"some message\") didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on cancel(throwable)") {
      try {
        cancel(new RuntimeException)
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 36))
            case None => fail("cancel(throwable) didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("cancel(throwable) didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on cancel(\"some message\", throwable)") {
      try {
        cancel("some message", new RuntimeException)
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 51))
            case None => fail("cancel(\"some message\", throwable) didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("cancel(\"some message\", throwable) didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assume(false)") {
      try {
        assume(false)
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 66))
            case None => fail("assume(false) didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assume(false) didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assume(false, \"some message\")") {
      try {
        assume(false, "some message")
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 81))
            case None => fail("assume(false, \"some message\") didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assume(false, \"some message\") didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assume(1 === 2)") {
      try {
        assume(1 === 2)
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 96))
            case None => fail("assume(1 === 2) didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assume(1 === 2) didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assume(1 === 2, \"some message\")") {
      try {
        assume(1 === 2, "some message")
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 111))
            case None => fail("assume(1 === 2, \"some message\") didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assume(1 === 2, \"some message\") didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assertResult(1) { 2 }") {
      try {
        assertResult(1) {
          2
        }
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 126))
            case None => fail("assertResult(1) { 2 } didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assertResult(1) { 2 } didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on assertResult(1, \"some message\") { 2 }") {
      try {
        assertResult(1, "some message") {
          2
        }
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 141))
            case None => fail("assertResult(1, \"some message\") { 2 } didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("assertResult(1, \"some message\") { 2 } didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on intercept[IllegalArgumentException] {}") {
      try {
        intercept[IllegalArgumentException] {}
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 156))
            case None => fail("intercept[IllegalArgumentException] {} didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("intercept[IllegalArgumentException] {} didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on intercept[IllegalArgumentException] { throw new RuntimeException }") {
      try {
        intercept[IllegalArgumentException] {
          if (false) 1 else throw new RuntimeException
        }
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 171))
            case None => fail("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("intercept[IllegalArgumentException] { throw new RuntimeException } didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on 1 should === (2)") {
      try {
        1 should === (2)
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) =>
              if (s != ("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 186))) {
                cancel("s was: " + s, e)
              }
            case None => fail("1 should === 2 didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("1 should === 2 didn't produce a TestCanceledException", e)
      }
    }

    it("should give the proper line on an [IllegalArgumentException] should be thrownBy {}") {
      try {
        an [IllegalArgumentException] should be thrownBy {}
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => // s should equal ("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 204))
              if (s != ("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 204))) {
                cancel("s was: " + s, e)
              }
            case None => fail("an [IllegalArgumentException] should be thrownBy {} didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("an [IllegalArgumentException] should be thrownBy {} didn't produce a TestCanceledException", e)
      }
    }

    // TODO: What is this testing? there should be no TCE thrown by an [IAE] should be thrownBy ....
    it("should give the proper line on an [IllegalArgumentException] should be thrownBy { throw new RuntimeException }") {
      try {
        an [IllegalArgumentException] should be thrownBy {
          if (false) () else throw new RuntimeException
        }
      }
      catch {
        case e: TestCanceledException =>
          e.failedCodeFileNameAndLineNumberString match {
            case Some(s) => s should equal("TestCanceledExceptionSpec.scala:" + (baseLineNumber + 222))
            case None => fail("an [IllegalArgumentException] should be thrownBy { throw new RuntimeException } didn't produce a file name and line number string", e)
          }
        case e: Throwable =>
          cancel("an [IllegalArgumentException] should be thrownBy { throw new RuntimeException } didn't produce a TestCanceledException", e)
      }
    }

    it("should return the cause in both cause and getCause") {
      val theCause = new IllegalArgumentException("howdy")
      val tfe = new TestCanceledException(Some("doody"), Some(theCause), Some(source.Position.here), 3)
      assume(tfe.cause.isDefined)
      assume(tfe.cause.get === theCause)
      assume(tfe.getCause == theCause)
    }

    it("should return None in cause and null in getCause if no cause") {
      val tfe = new TestCanceledException(Some("doody"), None, Some(source.Position.here), 3)
      assume(tfe.cause.isEmpty)
      assume(tfe.getCause == null)
    }

    it("should be equal to itself") {
      val tfe = new TestCanceledException(Some("doody"), None, Some(source.Position.here), 3)
      assume(tfe == tfe)
    }
  }
}

