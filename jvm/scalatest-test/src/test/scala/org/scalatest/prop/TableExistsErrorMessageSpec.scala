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
package org.scalatest.prop

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

/**
 * Test that verifies the error message for exists() uses the correct resource key.
 */
class TableExistsErrorMessageSpec extends AnyFunSpec with Matchers with TableDrivenPropertyChecks {

  describe("exists error message") {
    it("should use the correct error message for failed exists checks") {
      val table = Table(
        ("a", "b"),
        (1, 2),
        (3, 4),
        (5, 6)
      )

      val exception = intercept[exceptions.TestFailedException] {
        exists(table) { (a: Int, b: Int) =>
          // This will fail for all rows, triggering the error message
          a should be > 100
        }
      }

      // The error message should use the exists-specific message
      exception.message.get should startWith("Table-driven exists check failed: no rows satisfied the property:")
      exception.message.get should not startWith("Table-driven property check failed for one or more rows:")
    }
  }
}
