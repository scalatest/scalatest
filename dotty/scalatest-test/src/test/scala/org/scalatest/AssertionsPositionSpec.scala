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

import org.scalactic.source
import org.scalatest.exceptions.{TestCanceledException, TestFailedException}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class AssertionsPositionSpec extends AnyFunSpec with Matchers {

  def doFail()(using pos: source.Position): Unit = {
    fail()
  }

  def doFailWithMessage(message: String)(using pos: source.Position): Unit = {
    fail(message)
  }

  def doCancel(message: String)(using pos: source.Position): Unit = {
    cancel(message)
  }

  def doIntercept(f: => Any)(using pos: source.Position): Unit = {
    intercept[RuntimeException](f)
  }

  def doAssertResult(actual: String)(using pos: source.Position): Unit = {
    assertResult("expected")(actual)
  }

  def doFailWithMessageAndCause(message: String, cause: Throwable)(using pos: source.Position): Unit = {
    fail(message, cause)
  }

  def doFailWithCause(cause: Throwable)(using pos: source.Position): Unit = {
    fail(cause)
  }

  def doCancelNoArgs()(using pos: source.Position): Unit = {
    cancel()
  }

  def doCancelWithMessageAndCause(message: String, cause: Throwable)(using pos: source.Position): Unit = {
    cancel(message, cause)
  }

  def doCancelWithCause(cause: Throwable)(using pos: source.Position): Unit = {
    cancel(cause)
  }

  def doAssertThrows(f: => Any)(using pos: source.Position): Unit = {
    assertThrows[RuntimeException](f)
  }

  def doAssertResultWithoutClue(actual: String)(using pos: source.Position): Unit = {
    assertResult("expected")(actual)
  }

  def doAssertResultWithClue(actual: String)(using pos: source.Position): Unit = {
    assertResult("expected", "the clue")(actual)
  }

  def doPendingUntilFixed()(using pos: source.Position): Unit = {
    pendingUntilFixed(())
  }

  describe("assertions") {
    it("should use the Position available in the enclosing scope when reporting location for fail") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 42)
      val e =
        try {
          doFail()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(42))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for fail(message)") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 43)
      val e =
        try {
          doFailWithMessage("oops")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(43))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for cancel") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 44)
      val e =
        try {
          doCancel("canceled")(using callerPos)
          fail("Expected TestCanceledException to be thrown")
        }
        catch {
          case e: TestCanceledException => e
        }

      e.position.map(_.lineNumber) should be(Some(44))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for intercept") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 45)
      val e =
        try {
          doIntercept("no exception thrown")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(45))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertResult") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 46)
      val e =
        try {
          doAssertResult("actual")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(46))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for fail(message, cause)") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 46)
      val e =
        try {
          doFailWithMessageAndCause("oops", new RuntimeException("cause"))(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(46))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for fail(cause)") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 50)
      val e =
        try {
          doFailWithCause(new RuntimeException("cause"))(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(50))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for cancel()") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 54)
      val e =
        try {
          doCancelNoArgs()(using callerPos)
          fail("Expected TestCanceledException to be thrown")
        }
        catch {
          case e: TestCanceledException => e
        }

      e.position.map(_.lineNumber) should be(Some(54))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for cancel(message, cause)") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 58)
      val e =
        try {
          doCancelWithMessageAndCause("canceled", new RuntimeException("cause"))(using callerPos)
          fail("Expected TestCanceledException to be thrown")
        }
        catch {
          case e: TestCanceledException => e
        }

      e.position.map(_.lineNumber) should be(Some(58))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for cancel(cause)") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 62)
      val e =
        try {
          doCancelWithCause(new RuntimeException("cause"))(using callerPos)
          fail("Expected TestCanceledException to be thrown")
        }
        catch {
          case e: TestCanceledException => e
        }

      e.position.map(_.lineNumber) should be(Some(62))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertThrows") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 66)
      val e =
        try {
          doAssertThrows("no exception thrown")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(66))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertResult without clue") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e =
        try {
          doAssertResultWithoutClue("actual")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertResult with clue") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 74)
      val e =
        try {
          doAssertResultWithClue("actual")(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(74))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for pendingUntilFixed that succeeded") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 78)
      val e =
        try {
          doPendingUntilFixed()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(78))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
