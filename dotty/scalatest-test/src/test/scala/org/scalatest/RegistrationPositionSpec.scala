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
import org.scalatest.exceptions.DuplicateTestNameException
import org.scalatest.exceptions.NotAllowedException
import org.scalatest.exceptions.StackDepthException
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.featurespec.AnyFeatureSpec
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.funsuite.{AnyFunSuite, AsyncFunSuite}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class RegistrationPositionSpec extends AnyFunSpec with Matchers {

  // Inner classes, so they will not be discovered and run by the test framework.

  private class FunSuiteProbe extends AnyFunSuite {
    def registerDupTests()(using pos: source.Position): Unit = {
      test("dup") { succeed }
      test("dup") { succeed }
    }
  }

  private class AsyncFunSuiteProbe extends AsyncFunSuite {
    def registerDupTests()(using pos: source.Position): Unit = {
      test("dup") { Future { succeed } }
      test("dup") { Future { succeed } }
    }
  }

  private class FunSpecProbe extends AnyFunSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      describe("ctx") {
        it("dup") { succeed }
        it("dup") { succeed }
      }
    }
  }

  private class WordSpecProbe extends AnyWordSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      "a subject" should {
        "dup" in { succeed }
        "dup" in { succeed }
      }
    }
  }

  private class FlatSpecProbe extends AnyFlatSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      "a subject" should "dup" in { succeed }
      "a subject" should "dup" in { succeed }
    }
  }

  private class FreeSpecProbe extends AnyFreeSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      "dup" in { succeed }
      "dup" in { succeed }
    }
  }

  private class FeatureSpecProbe extends AnyFeatureSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      feature("F1") {
        scenario("dup") { succeed }
        scenario("dup") { succeed }
      }
    }
  }

  private class PropSpecProbe extends AnyPropSpec {
    def registerDupTests()(using pos: source.Position): Unit = {
      property("dup") { succeed }
      property("dup") { succeed }
    }
  }

  private def expectRegistrationException(registration: => Unit): StackDepthException =
    try {
      registration
      fail("Expected an exception to be thrown during registration")
    }
    catch {
      case e: DuplicateTestNameException => e
      case e: NotAllowedException        => e
    }

  describe("registration methods") {
    it("should use the Position available in the enclosing scope when registering tests in a funsuite") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 55)
      val e = expectRegistrationException(new FunSuiteProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(55))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in an async funsuite") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e = expectRegistrationException(new AsyncFunSuiteProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a funspec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 85)
      val e = expectRegistrationException(new FunSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(85))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a wordspec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 100)
      val e = expectRegistrationException(new WordSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(100))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a flatspec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 115)
      val e = expectRegistrationException(new FlatSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(115))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a freespec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 130)
      val e = expectRegistrationException(new FreeSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(130))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a featurespec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 145)
      val e = expectRegistrationException(new FeatureSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(145))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when registering tests in a propspec") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 160)
      val e = expectRegistrationException(new PropSpecProbe().registerDupTests()(using callerPos))

      e.position.map(_.lineNumber) should be(Some(160))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
