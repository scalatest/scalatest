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

class InspectorsPositionSpec extends AnyFunSpec with Matchers with Inspectors {

  def forAllShouldBeNegative(xs: List[Int])(using pos: source.Position): Unit = {
    forAll(xs) { n =>
      n should be < 0
    }
  }

  def forEveryShouldBeNegative(xs: List[Int])(using pos: source.Position): Unit = {
    forEvery(xs) { n =>
      n should be < 0
    }
  }

  describe("inspectors") {
    it("should use the Position available in the enclosing scope when reporting failure location for forAll") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 42)
      val e =
        try {
          forAllShouldBeNegative(List(1, -2, 3))(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(42))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }

    it("should use the Position available in the enclosing scope when reporting failure location for forEvery") {
      val callerPos = source.Position("SomeCallerFile.scala", "/some/path/SomeCallerFile.scala", 43)
      val e =
        try {
          forEveryShouldBeNegative(List(-1, 2, -3))(using callerPos)
          fail("Expected TestFailedException to be thrown")
        }
        catch {
          case e: TestFailedException => e
        }

      e.position.map(_.lineNumber) should be(Some(43))
      e.position.map(_.fileName) should be(Some("SomeCallerFile.scala"))
    }
  }
}
