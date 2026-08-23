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

class ThrownByPositionSpec extends AnyFunSpec with Matchers {

  def doAnShouldBeThrownBy()(using pos: source.Position): Unit = {
    an [RuntimeException] shouldBe thrownBy { "no exception thrown" }
  }

  def doAShouldBeThrownBy()(using pos: source.Position): Unit = {
    a [RuntimeException] shouldBe thrownBy { "no exception thrown" }
  }

  def doAnShouldBeThrownByBlock()(using pos: source.Position): Unit = {
    an [RuntimeException] should (be thrownBy { "no exception thrown" })
  }

  def doAShouldBeThrownByBlock()(using pos: source.Position): Unit = {
    a [RuntimeException] should (be thrownBy { "no exception thrown" })
  }

  def doAnMustBeThrownBy()(using pos: source.Position): Unit = {
    an [RuntimeException] mustBe thrownBy { "no exception thrown" }
  }

  def doAMustBeThrownBy()(using pos: source.Position): Unit = {
    a [RuntimeException] mustBe thrownBy { "no exception thrown" }
  }

  def doAnMustBeThrownByBlock()(using pos: source.Position): Unit = {
    an [RuntimeException] must (be thrownBy { "no exception thrown" })
  }

  def doAMustBeThrownByBlock()(using pos: source.Position): Unit = {
    a [RuntimeException] must (be thrownBy { "no exception thrown" })
  }

  describe("thrownBy syntax") {
    it("should use the Position available in the enclosing scope when reporting location for an [T] shouldBe thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 55)
      val e =
        try {
          doAnShouldBeThrownBy()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(55))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for a [T] shouldBe thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e =
        try {
          doAShouldBeThrownBy()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for an [T] should be thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 85)
      val e =
        try {
          doAnShouldBeThrownByBlock()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(85))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for a [T] should be thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 100)
      val e =
        try {
          doAShouldBeThrownByBlock()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(100))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for an [T] mustBe thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 115)
      val e =
        try {
          doAnMustBeThrownBy()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(115))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for a [T] mustBe thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 130)
      val e =
        try {
          doAMustBeThrownBy()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(130))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for an [T] must be thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 145)
      val e =
        try {
          doAnMustBeThrownByBlock()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(145))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting location for a [T] must be thrownBy") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 160)
      val e =
        try {
          doAMustBeThrownByBlock()(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(160))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
