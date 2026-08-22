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
  }
}
