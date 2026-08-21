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
import org.scalatest.exceptions.TestFailedDueToTimeoutException
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class EventuallyPositionSpec extends AnyFunSpec with Matchers with Eventually {

  def eventuallyShouldBe(expected: Int, actual: Int)(using pos: source.Position): Unit = {
    eventually {
      expected shouldBe actual
    }
  }

  describe("eventually") {
    it("should use the Position available in the enclosing scope when reporting failure location") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 42)
      val e =
        try {
          eventuallyShouldBe(42, 43)(using callerPos)
          fail("Expected TestFailedDueToTimeoutException to be thrown")
        }
        catch {
          case e: TestFailedDueToTimeoutException => e
        }

      e.position.map(_.lineNumber) should be(Some(42))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
