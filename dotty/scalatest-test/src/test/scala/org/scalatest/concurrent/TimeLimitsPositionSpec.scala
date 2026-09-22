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
package org.scalatest.concurrent

import org.scalactic.source
import org.scalatest.exceptions.{TestCanceledException, TestFailedDueToTimeoutException}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.time.{Millis, Span}

class TimeLimitsPositionSpec extends AnyFunSpec with Matchers with TimeLimits {

  def doFailAfter()(using pos: source.Position): Unit = {
    failAfter(Span(30, Millis)) {
      Thread.sleep(500)
      42
    }
  }

  def doCancelAfter()(using pos: source.Position): Unit = {
    cancelAfter(Span(30, Millis)) {
      Thread.sleep(500)
      42
    }
  }

  describe("failAfter") {
    it("should use the Position available in the enclosing scope when reporting location") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 55)
      val e =
        try {
          doFailAfter()(using callerPos)
          fail("Expected TestFailedDueToTimeoutException to be thrown")
        }
        catch {
          case e: TestFailedDueToTimeoutException => e
        }

      e.position.map(_.lineNumber) should be(Some(55))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }

  describe("cancelAfter") {
    it("should use the Position available in the enclosing scope when reporting location") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 70)
      val e =
        try {
          doCancelAfter()(using callerPos)
          fail("Expected TestCanceledException to be thrown")
        }
        catch {
          case e: TestCanceledException => e
        }

      e.position.map(_.lineNumber) should be(Some(70))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
