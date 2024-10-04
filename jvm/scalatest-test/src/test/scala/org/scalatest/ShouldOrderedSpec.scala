/*
 * Copyright 2001-2024 Artima, Inc.
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

import org.scalatest.prop.PropertyChecks
import Integer.{MAX_VALUE, MIN_VALUE}
import org.scalatest.exceptions.TestFailedException
import org.scalactic.anyvals.PosInt
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldOrderedSpec extends AnyFunSpec with PropertyChecks with ReturnsNormallyThrowsAssertion {

  // TODO: Fix these tests. They are wasting a bunch of time with discarded values
  implicit override val generatorDrivenConfig = PropertyCheckConfiguration(maxDiscardedFactor = 500.0)

  // Checking for a specific size
  describe("The 'be >/</>=/<= (x)' syntax") {

    describe("on Int") {

      it("should do nothing if the comparison holds true") {
        forAll((left: Int, right: Int) => if (left < right) left should be < (right) else succeed)
        forAll((left: Int, right: Int) => if (left <= right) left should be <= (right) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should be > (right) else succeed)
        forAll((left: Int, right: Int) => if (left >= right) left should be >= (right) else succeed)
      }

      it("should do nothing if the comparison fails and used with not") {

        forAll((left: Int, right: Int) => if (left >= right) left should not be < (right) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should not be <= (right) else succeed)
        forAll((left: Int, right: Int) => if (left <= right) left should not be > (right) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should not be >= (right) else succeed)

        forAll((left: Int, right: Int) => if (left >= right) left should not (be < (right)) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should not (be <= (right)) else succeed)
        forAll((left: Int, right: Int) => if (left <= right) left should not (be > (right)) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should not (be >= (right)) else succeed)
      }

      it("should do nothing when comparison succeeds and used in a logical-and expression") {

        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should ((be < (right)) and (be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should (be < (right) and (be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should (be < (right) and be < (right + 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should ((be <= (right)) and (be <= (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should (be <= (right) and (be <= (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should (be <= (right) and be <= (right + 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should ((be > (right)) and (be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should (be > (right) and (be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should (be > (right) and be > (right - 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should ((be >= (right)) and (be >= (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should (be >= (right) and (be >= (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should (be >= (right) and be >= (right - 1)) else succeed)
      }

      it("should do nothing when array size matches and used in a logical-or expression") {

        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should ((be < (right - 1)) or (be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should (be < (right - 1) or (be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left < right) && (right < MAX_VALUE)) left should (be < (right - 1) or be < (right + 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should ((be <= (right - 1)) or (be <= (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should (be <= (right - 1) or (be <= (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left <= right) && (right < MAX_VALUE)) left should (be <= (right - 1) or be <= (right + 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should ((be > (right + 1)) or (be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should (be > (right + 1) or (be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left > right) && (right > MIN_VALUE)) left should (be > (right + 1) or be > (right - 1)) else succeed)

        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should ((be >= (right + 1)) or (be >= (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should (be >= (right + 1) or (be >= (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if ((left >= right) && (right > MIN_VALUE)) left should (be >= (right + 1) or be >= (right - 1)) else succeed)

        forAll((left: Int, right: Int) => left should (be >= (right) or be < (right)))
        forAll((left: Int, right: Int) => left should (be > (right) or be <= (right)))
      }

      it("should do nothing when comparison fails and used in a logical-and expression with not") {

        forAll((left: Int, right: Int) => if (left > right) left should (not (be < (right)) and not (be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should ((not be < (right)) and (not be < (right + 1))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should (not be < (right) and not be < (right + 1)) else succeed)

        forAll((left: Int, right: Int) => if (left > right) left should (not (be <= (right)) and not (be <= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should ((not be <= (right)) and (not be <= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should (not be <= (right) and not be <= (right)) else succeed)

        forAll((left: Int, right: Int) => if (left < right) left should (not (be > (right)) and not (be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should ((not be > (right)) and (not be > (right - 1))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should (not be > (right) and not be > (right - 1)) else succeed)

        forAll((left: Int, right: Int) => if (left < right) left should (not (be >= (right)) and not (be >= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should ((not be >= (right)) and (not be >= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should (not be >= (right) and not be >= (right)) else succeed)
      }

      it("should do nothing when comparison fails and used in a logical-or expression with not") {

        forAll((left: Int, right: Int) => if (left > right) left should (not (be >= (right)) or not (be < (right))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should ((not be >= (right)) or (not be < (right))) else succeed)
        forAll((left: Int, right: Int) => if (left > right) left should (not be >= (right) or not be < (right)) else succeed)

        forAll((left: Int, right: Int) => if (left >= right) left should (not (be > (right)) or not (be <= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left >= right) left should ((not be > (right)) or (not be <= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left >= right) left should (not be > (right) or not be <= (right)) else succeed)

        forAll((left: Int, right: Int) => if (left < right) left should (not (be <= (right)) or not (be > (right))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should ((not be <= (right)) or (not be > (right))) else succeed)
        forAll((left: Int, right: Int) => if (left < right) left should (not be <= (right) or not be > (right)) else succeed)

        forAll((left: Int, right: Int) => if (left <= right) left should (not (be < (right)) or not (be >= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left <= right) left should ((not be < (right)) or (not be >= (right))) else succeed)
        forAll((left: Int, right: Int) => if (left <= right) left should (not be < (right) or not be >= (right)) else succeed)
      }

      it("should throw TestFailedException if comparison does not succeed") {

        val caught1 = intercept[TestFailedException] {
          1 should be < (1)
        }
        assert(caught1.getMessage === "1 was not less than 1")
        forAll((left: Int, right: Int) => if (left >= right) assertThrows[TestFailedException](left should be < (right)) else succeed)

        val caught2 = intercept[TestFailedException] {
          2 should be <= (1)
        }
        assert(caught2.getMessage === "2 was not less than or equal to 1")
        forAll((left: Int, right: Int) => if (left > right) assertThrows[TestFailedException](left should be <= (right)) else succeed)

        val caught3 = intercept[TestFailedException] {
          1 should be > (1)
        }
        assert(caught3.getMessage === "1 was not greater than 1")
        forAll((left: Int, right: Int) => if (left <= right) assertThrows[TestFailedException](left should be > (right)) else succeed)

        val caught4 = intercept[TestFailedException] {
          1 should be >= (2)
        }
        assert(caught4.getMessage === "1 was not greater than or equal to 2")
        forAll((left: Int, right: Int) => if (left < right) assertThrows[TestFailedException](left should be >= (right)) else succeed)
      }

      it("should throw TestFailedException if comparison succeeds but used with not") {

        val caught1 = intercept[TestFailedException] {
          1 should not be < (2)
        }
        assert(caught1.getMessage === "1 was less than 2")
        forAll((left: Int, right: Int) => if (left < right) assertThrows[TestFailedException](left should not be < (right)) else succeed)

        val caught2 = intercept[TestFailedException] {
          1 should not be <= (1)
        }
        assert(caught2.getMessage === "1 was less than or equal to 1")
        forAll((left: Int, right: Int) => if (left <= right) assertThrows[TestFailedException](left should not be <= (right)) else succeed)

        val caught3 = intercept[TestFailedException] {
          2 should not be > (1)
        }
        assert(caught3.getMessage === "2 was greater than 1")
        forAll((left: Int, right: Int) => if (left > right) assertThrows[TestFailedException](left should not be > (right)) else succeed)

        val caught4 = intercept[TestFailedException] {
          1 should not be >= (1)
        }
        assert(caught4.getMessage === "1 was greater than or equal to 1")
        forAll((left: Int, right: Int) => if (left >= right) assertThrows[TestFailedException](left should not be >= (right)) else succeed)
      }

      // Comparison with and
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          2 should { be < (5) and (be < (2)) }
        }
        assert(caught1.getMessage === "2 was less than 5, but 2 was not less than 2")

        val caught2 = intercept[TestFailedException] {
          2 should ((be < (5)) and (be < (2)))
        }
        assert(caught2.getMessage === "2 was less than 5, but 2 was not less than 2")

        val caught3 = intercept[TestFailedException] {
          2 should (be < (5) and be < (2))
        }
        assert(caught3.getMessage === "2 was less than 5, but 2 was not less than 2")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          7 should { be > (5) and (be > (12)) }
        }
        assert(caught1.getMessage === "7 was greater than 5, but 7 was not greater than 12")

        val caught2 = intercept[TestFailedException] {
          7 should ((be > (5)) and (be > (12)))
        }
        assert(caught2.getMessage === "7 was greater than 5, but 7 was not greater than 12")

        val caught3 = intercept[TestFailedException] {
          7 should (be > (5) and be > (12))
        }
        assert(caught3.getMessage === "7 was greater than 5, but 7 was not greater than 12")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          2 should { be <= (2) and (be <= (1)) }
        }
        assert(caught1.getMessage === "2 was less than or equal to 2, but 2 was not less than or equal to 1")

        val caught2 = intercept[TestFailedException] {
          2 should ((be <= (2)) and (be <= (1)))
        }
        assert(caught2.getMessage === "2 was less than or equal to 2, but 2 was not less than or equal to 1")

        val caught3 = intercept[TestFailedException] {
          2 should (be <= (2) and be <= (1))
        }
        assert(caught3.getMessage === "2 was less than or equal to 2, but 2 was not less than or equal to 1")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          7 should { be >= (7) and (be >= (8)) }
        }
        assert(caught1.getMessage === "7 was greater than or equal to 7, but 7 was not greater than or equal to 8")

        val caught2 = intercept[TestFailedException] {
          7 should ((be >= (7)) and (be >= (8)))
        }
        assert(caught2.getMessage === "7 was greater than or equal to 7, but 7 was not greater than or equal to 8")

        val caught3 = intercept[TestFailedException] {
          7 should (be >= (7) and be >= (8))
        }
        assert(caught3.getMessage === "7 was greater than or equal to 7, but 7 was not greater than or equal to 8")
      }

      // Comparison with or
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          2 should { be < (2) or (be < (1)) }
        }
        assert(caught1.getMessage === "2 was not less than 2, and 2 was not less than 1")

        val caught2 = intercept[TestFailedException] {
          2 should ((be < (2)) or (be < (1)))
        }
        assert(caught2.getMessage === "2 was not less than 2, and 2 was not less than 1")

        val caught3 = intercept[TestFailedException] {
          2 should (be < (2) or be < (1))
        }
        assert(caught3.getMessage === "2 was not less than 2, and 2 was not less than 1")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          1 should { be > (5) or (be > (12)) }
        }
        assert(caught1.getMessage === "1 was not greater than 5, and 1 was not greater than 12")

        val caught2 = intercept[TestFailedException] {
          1 should ((be > (5)) or (be > (12)))
        }
        assert(caught2.getMessage === "1 was not greater than 5, and 1 was not greater than 12")

        val caught3 = intercept[TestFailedException] {
          1 should (be > (5) or be > (12))
        }
        assert(caught3.getMessage === "1 was not greater than 5, and 1 was not greater than 12")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          3 should { be <= (2) or (be <= (1)) }
        }
        assert(caught1.getMessage === "3 was not less than or equal to 2, and 3 was not less than or equal to 1")

        val caught2 = intercept[TestFailedException] {
          3 should ((be <= (2)) or (be <= (1)))
        }
        assert(caught2.getMessage === "3 was not less than or equal to 2, and 3 was not less than or equal to 1")

        val caught3 = intercept[TestFailedException] {
          3 should (be <= (2) or be <= (1))
        }
        assert(caught3.getMessage === "3 was not less than or equal to 2, and 3 was not less than or equal to 1")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          6 should { be >= (7) or (be >= (8)) }
        }
        assert(caught1.getMessage === "6 was not greater than or equal to 7, and 6 was not greater than or equal to 8")

        val caught2 = intercept[TestFailedException] {
          6 should ((be >= (7)) or (be >= (8)))
        }
        assert(caught2.getMessage === "6 was not greater than or equal to 7, and 6 was not greater than or equal to 8")

        val caught3 = intercept[TestFailedException] {
          6 should (be >= (7) or be >= (8))
        }
        assert(caught3.getMessage === "6 was not greater than or equal to 7, and 6 was not greater than or equal to 8")
      }

      // Comparison with and not
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be < (2) } and not { be < (6) }}
        }
        assert(caught1.getMessage === "5 was not less than 2, but 5 was less than 6")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be < (2)) and (not be < (6)))
        }
        assert(caught2.getMessage === "5 was not less than 2, but 5 was less than 6")

        val caught3 = intercept[TestFailedException] {
          5 should (not be < (2) and not be < (6))
        }
        assert(caught3.getMessage === "5 was not less than 2, but 5 was less than 6")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          7 should { not { be > (8) } and not (be > (6)) }
        }
        assert(caught1.getMessage === "7 was not greater than 8, but 7 was greater than 6")

        val caught2 = intercept[TestFailedException] {
          7 should ((not be > (8)) and (not be > (6)))
        }
        assert(caught2.getMessage === "7 was not greater than 8, but 7 was greater than 6")

        val caught3 = intercept[TestFailedException] {
          7 should (not be > (8) and not be > (6))
        }
        assert(caught3.getMessage === "7 was not greater than 8, but 7 was greater than 6")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          2 should { not { be <= (1) } and (not be <= (2)) }
        }
        assert(caught1.getMessage === "2 was not less than or equal to 1, but 2 was less than or equal to 2")

        val caught2 = intercept[TestFailedException] {
          2 should ((not be <= (1)) and (not be <= (2)))
        }
        assert(caught2.getMessage === "2 was not less than or equal to 1, but 2 was less than or equal to 2")

        val caught3 = intercept[TestFailedException] {
          2 should (not be <= (1) and not be <= (2))
        }
        assert(caught3.getMessage === "2 was not less than or equal to 1, but 2 was less than or equal to 2")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          7 should { not { be >= (8) } and not (be >= (6)) }
        }
        assert(caught1.getMessage === "7 was not greater than or equal to 8, but 7 was greater than or equal to 6")

        val caught2 = intercept[TestFailedException] {
          7 should ((not be >= (8)) and (not be >= (6)))
        }
        assert(caught2.getMessage === "7 was not greater than or equal to 8, but 7 was greater than or equal to 6")

        val caught3 = intercept[TestFailedException] {
          7 should (not be >= (8) and not be >= (6))
        }
        assert(caught3.getMessage === "7 was not greater than or equal to 8, but 7 was greater than or equal to 6")
      }

      // Comparison with or not
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be < (7) } or not { be < (8) }}
        }
        assert(caught1.getMessage === "5 was less than 7, and 5 was less than 8")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be < (7)) or (not be < (8)))
        }
        assert(caught2.getMessage === "5 was less than 7, and 5 was less than 8")

        val caught3 = intercept[TestFailedException] {
          5 should (not be < (7) or not be < (8))
        }
        assert(caught3.getMessage === "5 was less than 7, and 5 was less than 8")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          7 should { not { be > (5) } or not (be > (6)) }
        }
        assert(caught1.getMessage === "7 was greater than 5, and 7 was greater than 6")

        val caught2 = intercept[TestFailedException] {
          7 should ((not be > (5)) or (not be > (6)))
        }
        assert(caught2.getMessage === "7 was greater than 5, and 7 was greater than 6")

        val caught3 = intercept[TestFailedException] {
          7 should (not be > (5) or not be > (6))
        }
        assert(caught3.getMessage === "7 was greater than 5, and 7 was greater than 6")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          2 should { not { be <= (3) } or (not be <= (2)) }
        }
        assert(caught1.getMessage === "2 was less than or equal to 3, and 2 was less than or equal to 2")

        val caught2 = intercept[TestFailedException] {
          2 should ((not be <= (3)) or (not be <= (2)))
        }
        assert(caught2.getMessage === "2 was less than or equal to 3, and 2 was less than or equal to 2")

        val caught3 = intercept[TestFailedException] {
          2 should (not be <= (3) or not be <= (2))
        }
        assert(caught3.getMessage === "2 was less than or equal to 3, and 2 was less than or equal to 2")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          8 should { not { be >= (7) } or not (be >= (6)) }
        }
        assert(caught1.getMessage === "8 was greater than or equal to 7, and 8 was greater than or equal to 6")

        val caught2 = intercept[TestFailedException] {
          8 should ((not be >= (7)) or (not be >= (6)))
        }
        assert(caught2.getMessage === "8 was greater than or equal to 7, and 8 was greater than or equal to 6")

        val caught3 = intercept[TestFailedException] {
          8 should (not be >= (7) or not be >= (6))
        }
        assert(caught3.getMessage === "8 was greater than or equal to 7, and 8 was greater than or equal to 6")
      }
    }

    describe("on String") {

      it("should do nothing if the comparison holds true") {
        forAll((left: String, right: String) => if (left < right) left should be < (right) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should be <= (right) else succeed)
        forAll((left: String, right: String) => if (left > right) left should be > (right) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should be >= (right) else succeed)
      }

      it("should do nothing if the comparison fails and used with not") {

        forAll((left: String, right: String) => if (left >= right) left should not be < (right) else succeed)
        forAll((left: String, right: String) => if (left > right) left should not be <= (right) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should not be > (right) else succeed)
        forAll((left: String, right: String) => if (left < right) left should not be >= (right) else succeed)

        forAll((left: String, right: String) => if (left >= right) left should not (be < (right)) else succeed)
        forAll((left: String, right: String) => if (left > right) left should not (be <= (right)) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should not (be > (right)) else succeed)
        forAll((left: String, right: String) => if (left < right) left should not (be >= (right)) else succeed)
      }

      it("should do nothing when comparison succeeds and used in a logical-and expression") {

        forAll((left: String, right: String) => if (left < right) left should ((be < (right)) and (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (be < (right) and (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (be < (right) and be < (right)) else succeed)

        forAll((left: String, right: String) => if (left <= right) left should ((be <= (right)) and (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (be <= (right) and (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (be <= (right) and be <= (right)) else succeed)

        forAll((left: String, right: String) => if (left > right) left should ((be > (right)) and (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (be > (right) and (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (be > (right) and be > (right)) else succeed)

        forAll((left: String, right: String) => if (left >= right) left should ((be >= (right)) and (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (be >= (right) and (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (be >= (right) and be >= (right)) else succeed)
      }

      it("should do nothing when array size matches and used in a logical-or expression") {

        forAll((left: String, right: String) => if (left < right) left should ((be < (right)) or (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (be < (right) or (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (be < (right) or be < (right)) else succeed)

        forAll((left: String, right: String) => if (left <= right) left should ((be <= (right)) or (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (be <= (right) or (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (be <= (right) or be <= (right)) else succeed)

        forAll((left: String, right: String) => if (left > right) left should ((be > (right)) or (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (be > (right) or (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (be > (right) or be > (right)) else succeed)

        forAll((left: String, right: String) => if (left >= right) left should ((be >= (right)) or (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (be >= (right) or (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (be >= (right) or be >= (right)) else succeed)

        forAll((left: String, right: String) => left should (be >= (right) or be < (right)))
        forAll((left: String, right: String) => left should (be > (right) or be <= (right)))
      }

      it("should do nothing when comparison fails and used in a logical-and expression with not") {

        forAll((left: String, right: String) => if (left >= right) left should (not (be < (right)) and not (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should ((not be < (right)) and (not be < (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (not be < (right) and not be < (right)) else succeed)

        forAll((left: String, right: String) => if (left > right) left should (not (be <= (right)) and not (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should ((not be <= (right)) and (not be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (not be <= (right) and not be <= (right)) else succeed)

        forAll((left: String, right: String) => if (left <= right) left should (not (be > (right)) and not (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should ((not be > (right)) and (not be > (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (not be > (right) and not be > (right)) else succeed)

        forAll((left: String, right: String) => if (left < right) left should (not (be >= (right)) and not (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should ((not be >= (right)) and (not be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (not be >= (right) and not be >= (right)) else succeed)
      }

      it("should do nothing when comparison fails and used in a logical-or expression with not") {

        forAll((left: String, right: String) => if (left > right) left should (not (be >= (right)) or not (be < (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should ((not be >= (right)) or (not be < (right))) else succeed)
        forAll((left: String, right: String) => if (left > right) left should (not be >= (right) or not be < (right)) else succeed)

        forAll((left: String, right: String) => if (left >= right) left should (not (be > (right)) or not (be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should ((not be > (right)) or (not be <= (right))) else succeed)
        forAll((left: String, right: String) => if (left >= right) left should (not be > (right) or not be <= (right)) else succeed)

        forAll((left: String, right: String) => if (left < right) left should (not (be <= (right)) or not (be > (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should ((not be <= (right)) or (not be > (right))) else succeed)
        forAll((left: String, right: String) => if (left < right) left should (not be <= (right) or not be > (right)) else succeed)

        forAll((left: String, right: String) => if (left <= right) left should (not (be < (right)) or not (be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should ((not be < (right)) or (not be >= (right))) else succeed)
        forAll((left: String, right: String) => if (left <= right) left should (not be < (right) or not be >= (right)) else succeed)
      }

      it("should throw TestFailedException if comparison does not succeed") {

        val caught1 = intercept[TestFailedException] {
          "aaa" should be < ("aaa")
        }
        assert(caught1.getMessage === "\"aaa\" was not less than \"aaa\"")
        forAll((left: String, right: String) => if (left >= right) assertThrows[TestFailedException](left should be < (right)) else succeed)

        val caught2 = intercept[TestFailedException] {
          "bbb" should be <= ("aaa")
        }
        assert(caught2.getMessage === "\"bbb\" was not less than or equal to \"aaa\"")
        forAll((left: String, right: String) => if (left > right) assertThrows[TestFailedException](left should be <= (right)) else succeed)

        val caught3 = intercept[TestFailedException] {
          "aaa" should be > ("aaa")
        }
        assert(caught3.getMessage === "\"aaa\" was not greater than \"aaa\"")
        forAll((left: String, right: String) => if (left <= right) assertThrows[TestFailedException](left should be > (right)) else succeed)

        val caught4 = intercept[TestFailedException] {
          "aaa" should be >= ("bbb")
        }
        assert(caught4.getMessage === "\"aaa\" was not greater than or equal to \"bbb\"")
        forAll((left: String, right: String) => if (left < right) assertThrows[TestFailedException](left should be >= (right)) else succeed)
      }

      it("should throw TestFailedException if comparison succeeds but used with not") {

        val caught1 = intercept[TestFailedException] {
          "aaa" should not be < ("bbb")
        }
        assert(caught1.getMessage === "\"aaa\" was less than \"bbb\"")
        forAll((left: String, right: String) => if (left < right) assertThrows[TestFailedException](left should not be < (right)) else succeed)

        val caught2 = intercept[TestFailedException] {
          "aaa" should not be <= ("aaa")
        }
        assert(caught2.getMessage === "\"aaa\" was less than or equal to \"aaa\"")
        forAll((left: String, right: String) => if (left <= right) assertThrows[TestFailedException](left should not be <= (right)) else succeed)

        val caught3 = intercept[TestFailedException] {
          "bbb" should not be > ("aaa")
        }
        assert(caught3.getMessage === "\"bbb\" was greater than \"aaa\"")
        forAll((left: String, right: String) => if (left > right) assertThrows[TestFailedException](left should not be > (right)) else succeed)

        val caught4 = intercept[TestFailedException] {
          "aaa" should not be >= ("aaa")
        }
        assert(caught4.getMessage === "\"aaa\" was greater than or equal to \"aaa\"")
        forAll((left: String, right: String) => if (left >= right) assertThrows[TestFailedException](left should not be >= (right)) else succeed)
      }

      // Comparison with and
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          "2" should { be < ("5") and (be < ("2")) }
        }
        assert(caught1.getMessage === "\"2\" was less than \"5\", but \"2\" was not less than \"2\"")

        val caught2 = intercept[TestFailedException] {
          "2" should ((be < ("5")) and (be < ("2")))
        }
        assert(caught2.getMessage === "\"2\" was less than \"5\", but \"2\" was not less than \"2\"")

        val caught3 = intercept[TestFailedException] {
          "2" should (be < ("5") and be < ("2"))
        }
        assert(caught3.getMessage === "\"2\" was less than \"5\", but \"2\" was not less than \"2\"")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          "7" should { be > ("5") and (be > ("9")) }
        }
        assert(caught1.getMessage === "\"7\" was greater than \"5\", but \"7\" was not greater than \"9\"")

        val caught2 = intercept[TestFailedException] {
          "7" should ((be > ("5")) and (be > ("9")))
        }
        assert(caught2.getMessage === "\"7\" was greater than \"5\", but \"7\" was not greater than \"9\"")

        val caught3 = intercept[TestFailedException] {
          "7" should (be > ("5") and be > ("9"))
        }
        assert(caught3.getMessage === "\"7\" was greater than \"5\", but \"7\" was not greater than \"9\"")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          "2" should { be <= ("2") and (be <= ("1")) }
        }
        assert(caught1.getMessage === "\"2\" was less than or equal to \"2\", but \"2\" was not less than or equal to \"1\"")

        val caught2 = intercept[TestFailedException] {
          "2" should ((be <= ("2")) and (be <= ("1")))
        }
        assert(caught2.getMessage === "\"2\" was less than or equal to \"2\", but \"2\" was not less than or equal to \"1\"")

        val caught3 = intercept[TestFailedException] {
          "2" should (be <= ("2") and be <= ("1"))
        }
        assert(caught3.getMessage === "\"2\" was less than or equal to \"2\", but \"2\" was not less than or equal to \"1\"")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression") {

        val caught1 = intercept[TestFailedException] {
          "7" should { be >= ("7") and (be >= ("8")) }
        }
        assert(caught1.getMessage === "\"7\" was greater than or equal to \"7\", but \"7\" was not greater than or equal to \"8\"")

        val caught2 = intercept[TestFailedException] {
          "7" should ((be >= ("7")) and (be >= ("8")))
        }
        assert(caught2.getMessage === "\"7\" was greater than or equal to \"7\", but \"7\" was not greater than or equal to \"8\"")

        val caught3 = intercept[TestFailedException] {
          "7" should (be >= ("7") and be >= ("8"))
        }
        assert(caught3.getMessage === "\"7\" was greater than or equal to \"7\", but \"7\" was not greater than or equal to \"8\"")
      }

      // Comparison with or
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          "2" should { be < ("2") or (be < ("1")) }
        }
        assert(caught1.getMessage === "\"2\" was not less than \"2\", and \"2\" was not less than \"1\"")

        val caught2 = intercept[TestFailedException] {
          "2" should ((be < ("2")) or (be < ("1")))
        }
        assert(caught2.getMessage === "\"2\" was not less than \"2\", and \"2\" was not less than \"1\"")

        val caught3 = intercept[TestFailedException] {
          "2" should (be < ("2") or be < ("1"))
        }
        assert(caught3.getMessage === "\"2\" was not less than \"2\", and \"2\" was not less than \"1\"")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          "1" should { be > ("5") or (be > ("9")) }
        }
        assert(caught1.getMessage === "\"1\" was not greater than \"5\", and \"1\" was not greater than \"9\"")

        val caught2 = intercept[TestFailedException] {
          "1" should ((be > ("5")) or (be > ("9")))
        }
        assert(caught2.getMessage === "\"1\" was not greater than \"5\", and \"1\" was not greater than \"9\"")

        val caught3 = intercept[TestFailedException] {
          "1" should (be > ("5") or be > ("9"))
        }
        assert(caught3.getMessage === "\"1\" was not greater than \"5\", and \"1\" was not greater than \"9\"")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          "3" should { be <= ("2") or (be <= ("1")) }
        }
        assert(caught1.getMessage === "\"3\" was not less than or equal to \"2\", and \"3\" was not less than or equal to \"1\"")

        val caught2 = intercept[TestFailedException] {
          "3" should ((be <= ("2")) or (be <= ("1")))
        }
        assert(caught2.getMessage === "\"3\" was not less than or equal to \"2\", and \"3\" was not less than or equal to \"1\"")

        val caught3 = intercept[TestFailedException] {
          "3" should (be <= ("2") or be <= ("1"))
        }
        assert(caught3.getMessage === "\"3\" was not less than or equal to \"2\", and \"3\" was not less than or equal to \"1\"")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression") {

        val caught1 = intercept[TestFailedException] {
          "6" should { be >= ("7") or (be >= ("8")) }
        }
        assert(caught1.getMessage === "\"6\" was not greater than or equal to \"7\", and \"6\" was not greater than or equal to \"8\"")

        val caught2 = intercept[TestFailedException] {
          "6" should ((be >= ("7")) or (be >= ("8")))
        }
        assert(caught2.getMessage === "\"6\" was not greater than or equal to \"7\", and \"6\" was not greater than or equal to \"8\"")

        val caught3 = intercept[TestFailedException] {
          "6" should (be >= ("7") or be >= ("8"))
        }
        assert(caught3.getMessage === "\"6\" was not greater than or equal to \"7\", and \"6\" was not greater than or equal to \"8\"")
      }

      // Comparison with and not
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "5" should { not { be < ("2") } and not { be < ("6") }}
        }
        assert(caught1.getMessage === "\"5\" was not less than \"2\", but \"5\" was less than \"6\"")

        val caught2 = intercept[TestFailedException] {
          "5" should ((not be < ("2")) and (not be < ("6")))
        }
        assert(caught2.getMessage === "\"5\" was not less than \"2\", but \"5\" was less than \"6\"")

        val caught3 = intercept[TestFailedException] {
          "5" should (not be < ("2") and not be < ("6"))
        }
        assert(caught3.getMessage === "\"5\" was not less than \"2\", but \"5\" was less than \"6\"")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "7" should { not { be > ("8") } and not (be > ("6")) }
        }
        assert(caught1.getMessage === "\"7\" was not greater than \"8\", but \"7\" was greater than \"6\"")

        val caught2 = intercept[TestFailedException] {
          "7" should ((not be > ("8")) and (not be > ("6")))
        }
        assert(caught2.getMessage === "\"7\" was not greater than \"8\", but \"7\" was greater than \"6\"")

        val caught3 = intercept[TestFailedException] {
          "7" should (not be > ("8") and not be > ("6"))
        }
        assert(caught3.getMessage === "\"7\" was not greater than \"8\", but \"7\" was greater than \"6\"")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "2" should { not { be <= ("1") } and (not be <= ("2")) }
        }
        assert(caught1.getMessage === "\"2\" was not less than or equal to \"1\", but \"2\" was less than or equal to \"2\"")

        val caught2 = intercept[TestFailedException] {
          "2" should ((not be <= ("1")) and (not be <= ("2")))
        }
        assert(caught2.getMessage === "\"2\" was not less than or equal to \"1\", but \"2\" was less than or equal to \"2\"")

        val caught3 = intercept[TestFailedException] {
          "2" should (not be <= ("1") and not be <= ("2"))
        }
        assert(caught3.getMessage === "\"2\" was not less than or equal to \"1\", but \"2\" was less than or equal to \"2\"")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "7" should { not { be >= ("8") } and not (be >= ("6")) }
        }
        assert(caught1.getMessage === "\"7\" was not greater than or equal to \"8\", but \"7\" was greater than or equal to \"6\"")

        val caught2 = intercept[TestFailedException] {
          "7" should ((not be >= ("8")) and (not be >= ("6")))
        }
        assert(caught2.getMessage === "\"7\" was not greater than or equal to \"8\", but \"7\" was greater than or equal to \"6\"")

        val caught3 = intercept[TestFailedException] {
          "7" should (not be >= ("8") and not be >= ("6"))
        }
        assert(caught3.getMessage === "\"7\" was not greater than or equal to \"8\", but \"7\" was greater than or equal to \"6\"")
      }

      // Comparison with or not
      it("should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "5" should { not { be < ("7") } or not { be < ("8") }}
        }
        assert(caught1.getMessage === "\"5\" was less than \"7\", and \"5\" was less than \"8\"")

        val caught2 = intercept[TestFailedException] {
          "5" should ((not be < ("7")) or (not be < ("8")))
        }
        assert(caught2.getMessage === "\"5\" was less than \"7\", and \"5\" was less than \"8\"")

        val caught3 = intercept[TestFailedException] {
          "5" should (not be < ("7") or not be < ("8"))
        }
        assert(caught3.getMessage === "\"5\" was less than \"7\", and \"5\" was less than \"8\"")
      }

      it("should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "7" should { not { be > ("5") } or not (be > ("6")) }
        }
        assert(caught1.getMessage === "\"7\" was greater than \"5\", and \"7\" was greater than \"6\"")

        val caught2 = intercept[TestFailedException] {
          "7" should ((not be > ("5")) or (not be > ("6")))
        }
        assert(caught2.getMessage === "\"7\" was greater than \"5\", and \"7\" was greater than \"6\"")

        val caught3 = intercept[TestFailedException] {
          "7" should (not be > ("5") or not be > ("6"))
        }
        assert(caught3.getMessage === "\"7\" was greater than \"5\", and \"7\" was greater than \"6\"")
      }

      it("should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "2" should { not { be <= ("3") } or (not be <= ("2")) }
        }
        assert(caught1.getMessage === "\"2\" was less than or equal to \"3\", and \"2\" was less than or equal to \"2\"")

        val caught2 = intercept[TestFailedException] {
          "2" should ((not be <= ("3")) or (not be <= ("2")))
        }
        assert(caught2.getMessage === "\"2\" was less than or equal to \"3\", and \"2\" was less than or equal to \"2\"")

        val caught3 = intercept[TestFailedException] {
          "2" should (not be <= ("3") or not be <= ("2"))
        }
        assert(caught3.getMessage === "\"2\" was less than or equal to \"3\", and \"2\" was less than or equal to \"2\"")
      }

      it("should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "8" should { not { be >= ("7") } or not (be >= ("6")) }
        }
        assert(caught1.getMessage === "\"8\" was greater than or equal to \"7\", and \"8\" was greater than or equal to \"6\"")

        val caught2 = intercept[TestFailedException] {
          "8" should ((not be >= ("7")) or (not be >= ("6")))
        }
        assert(caught2.getMessage === "\"8\" was greater than or equal to \"7\", and \"8\" was greater than or equal to \"6\"")

        val caught3 = intercept[TestFailedException] {
          "8" should (not be >= ("7") or not be >= ("6"))
        }
        assert(caught3.getMessage === "\"8\" was greater than or equal to \"7\", and \"8\" was greater than or equal to \"6\"")
      }
    }
  }
}
