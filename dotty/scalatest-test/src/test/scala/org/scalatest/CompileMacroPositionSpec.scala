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
import org.scalatest.exceptions.TestFailedException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class CompileMacroPositionSpec extends AnyFunSpec with Matchers {

  def doAssertTypeError()(using pos: source.Position): Unit = {
    assertTypeError("val a: Int = 1")
  }

  def doAssertOnTypeErrorWithParseError()(using pos: source.Position): Unit = {
    assertOnTypeError("""val a: String = "unterminated""") { _ => Succeeded }
  }

  def doAssertOnTypeErrorWhenCompiles()(using pos: source.Position): Unit = {
    assertOnTypeError("val a: Int = 1") { _ => Succeeded }
  }

  def doAssertDoesNotCompile()(using pos: source.Position): Unit = {
    assertDoesNotCompile("val a: Int = 1")
  }

  def doAssertCompilesWithTypeError()(using pos: source.Position): Unit = {
    assertCompiles("val a: String = 1")
  }

  def doAssertCompilesWithParseError()(using pos: source.Position): Unit = {
    assertCompiles("""val a: String = "unterminated""")
  }

  describe("compile assertions") {
    it("should use the Position available in the enclosing scope when reporting location for assertTypeError") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 55)
      val e =
        try {
          doAssertTypeError()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(55))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertOnTypeError with parse error") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e =
        try {
          doAssertOnTypeErrorWithParseError()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertOnTypeError when snippet compiles") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 85)
      val e =
        try {
          doAssertOnTypeErrorWhenCompiles()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(85))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertDoesNotCompile") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 100)
      val e =
        try {
          doAssertDoesNotCompile()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(100))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertCompiles with type error") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 115)
      val e =
        try {
          doAssertCompilesWithTypeError()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(115))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for assertCompiles with parse error") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 130)
      val e =
        try {
          doAssertCompilesWithParseError()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(130))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
