/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.matchers

import org.scalatest._
import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import Integer.{MAX_VALUE, MIN_VALUE}
import org.scalatest.exceptions.TestFailedException

class ShouldOrderedSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific size
  object `The 'be >/</>=/<= (x)' syntax` {

    object `on Int` {

      def `should do nothing if the comparison holds true` {
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should be < (right)))
        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should be <= (right)))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should be > (right)))
        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should be >= (right)))
      }

      def `should do nothing if the comparison fails and used with not` {

        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should not be < (right)))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should not be <= (right)))
        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should not be > (right)))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should not be >= (right)))

        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should not (be < (right))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should not (be <= (right))))
        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should not (be > (right))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should not (be >= (right))))
      }

      def `should do nothing when comparison succeeds and used in a logical-and expression` {

        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should ((be < (right)) and (be < (right + 1)))))
        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be < (right) and (be < (right + 1)))))
        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be < (right) and be < (right + 1))))

        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should ((be <= (right)) and (be <= (right + 1)))))
        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be <= (right) and (be <= (right + 1)))))
        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be <= (right) and be <= (right + 1))))

        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should ((be > (right)) and (be > (right - 1)))))
        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be > (right) and (be > (right - 1)))))
        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be > (right) and be > (right - 1))))

        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should ((be >= (right)) and (be >= (right - 1)))))
        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be >= (right) and (be >= (right - 1)))))
        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be >= (right) and be >= (right - 1))))
      }

      def `should do nothing when array size matches and used in a logical-or expression` {

        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should ((be < (right - 1)) or (be < (right + 1)))))
        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be < (right - 1) or (be < (right + 1)))))
        check((left: Int, right: Int) => ((left < right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be < (right - 1) or be < (right + 1))))

        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should ((be <= (right - 1)) or (be <= (right + 1)))))
        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be <= (right - 1) or (be <= (right + 1)))))
        check((left: Int, right: Int) => ((left <= right) && (right < MAX_VALUE)) ==> returnsNormally(left should (be <= (right - 1) or be <= (right + 1))))

        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should ((be > (right + 1)) or (be > (right - 1)))))
        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be > (right + 1) or (be > (right - 1)))))
        check((left: Int, right: Int) => ((left > right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be > (right + 1) or be > (right - 1))))

        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should ((be >= (right + 1)) or (be >= (right - 1)))))
        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be >= (right + 1) or (be >= (right - 1)))))
        check((left: Int, right: Int) => ((left >= right) && (right > MIN_VALUE)) ==> returnsNormally(left should (be >= (right + 1) or be >= (right - 1))))

        check((left: Int, right: Int) => returnsNormally(left should (be >= (right) or be < (right))))
        check((left: Int, right: Int) => returnsNormally(left should (be > (right) or be <= (right))))
      }

      def `should do nothing when comparison fails and used in a logical-and expression with not` {

        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not (be < (right)) and not (be < (right + 1)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should ((not be < (right)) and (not be < (right + 1)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not be < (right) and not be < (right + 1))))

        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not (be <= (right)) and not (be <= (right)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should ((not be <= (right)) and (not be <= (right)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not be <= (right) and not be <= (right))))

        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not (be > (right)) and not (be > (right - 1)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should ((not be > (right)) and (not be > (right - 1)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not be > (right) and not be > (right - 1))))

        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not (be >= (right)) and not (be >= (right)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should ((not be >= (right)) and (not be >= (right)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not be >= (right) and not be >= (right))))
      }

      def `should do nothing when comparison fails and used in a logical-or expression with not` {

        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not (be >= (right)) or not (be < (right)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should ((not be >= (right)) or (not be < (right)))))
        check((left: Int, right: Int) => left > right ==> returnsNormally(left should (not be >= (right) or not be < (right))))

        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should (not (be > (right)) or not (be <= (right)))))
        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should ((not be > (right)) or (not be <= (right)))))
        check((left: Int, right: Int) => left >= right ==> returnsNormally(left should (not be > (right) or not be <= (right))))

        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not (be <= (right)) or not (be > (right)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should ((not be <= (right)) or (not be > (right)))))
        check((left: Int, right: Int) => left < right ==> returnsNormally(left should (not be <= (right) or not be > (right))))

        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should (not (be < (right)) or not (be >= (right)))))
        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should ((not be < (right)) or (not be >= (right)))))
        check((left: Int, right: Int) => left <= right ==> returnsNormally(left should (not be < (right) or not be >= (right))))
      }

      def `should throw TestFailedException if comparison does not succeed` {

        val caught1 = intercept[TestFailedException] {
          1 should be < (1)
        }
        assert(caught1.getMessage === "1 was not less than 1")
        check((left: Int, right: Int) => left >= right ==> throwsTestFailedException(left should be < (right)))

        val caught2 = intercept[TestFailedException] {
          2 should be <= (1)
        }
        assert(caught2.getMessage === "2 was not less than or equal to 1")
        check((left: Int, right: Int) => left > right ==> throwsTestFailedException(left should be <= (right)))

        val caught3 = intercept[TestFailedException] {
          1 should be > (1)
        }
        assert(caught3.getMessage === "1 was not greater than 1")
        check((left: Int, right: Int) => left <= right ==> throwsTestFailedException(left should be > (right)))

        val caught4 = intercept[TestFailedException] {
          1 should be >= (2)
        }
        assert(caught4.getMessage === "1 was not greater than or equal to 2")
        check((left: Int, right: Int) => left < right ==> throwsTestFailedException(left should be >= (right)))
      }

      def `should throw TestFailedException if comparison succeeds but used with not` {

        val caught1 = intercept[TestFailedException] {
          1 should not be < (2)
        }
        assert(caught1.getMessage === "1 was less than 2")
        check((left: Int, right: Int) => left < right ==> throwsTestFailedException(left should not be < (right)))

        val caught2 = intercept[TestFailedException] {
          1 should not be <= (1)
        }
        assert(caught2.getMessage === "1 was less than or equal to 1")
        check((left: Int, right: Int) => left <= right ==> throwsTestFailedException(left should not be <= (right)))

        val caught3 = intercept[TestFailedException] {
          2 should not be > (1)
        }
        assert(caught3.getMessage === "2 was greater than 1")
        check((left: Int, right: Int) => left > right ==> throwsTestFailedException(left should not be > (right)))

        val caught4 = intercept[TestFailedException] {
          1 should not be >= (1)
        }
        assert(caught4.getMessage === "1 was greater than or equal to 1")
        check((left: Int, right: Int) => left >= right ==> throwsTestFailedException(left should not be >= (right)))
      }

      // Comparison with and
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression used with not` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression used with not` {

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

    object `on String` {

      def `should do nothing if the comparison holds true` {
        check((left: String, right: String) => left < right ==> returnsNormally(left should be < (right)))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should be <= (right)))
        check((left: String, right: String) => left > right ==> returnsNormally(left should be > (right)))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should be >= (right)))
      }

      def `should do nothing if the comparison fails and used with not` {

        check((left: String, right: String) => left >= right ==> returnsNormally(left should not be < (right)))
        check((left: String, right: String) => left > right ==> returnsNormally(left should not be <= (right)))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should not be > (right)))
        check((left: String, right: String) => left < right ==> returnsNormally(left should not be >= (right)))

        check((left: String, right: String) => left >= right ==> returnsNormally(left should not (be < (right))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should not (be <= (right))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should not (be > (right))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should not (be >= (right))))
      }

      def `should do nothing when comparison succeeds and used in a logical-and expression` {

        check((left: String, right: String) => left < right ==> returnsNormally(left should ((be < (right)) and (be < (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (be < (right) and (be < (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (be < (right) and be < (right))))

        check((left: String, right: String) => left <= right ==> returnsNormally(left should ((be <= (right)) and (be <= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (be <= (right) and (be <= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (be <= (right) and be <= (right))))

        check((left: String, right: String) => left > right ==> returnsNormally(left should ((be > (right)) and (be > (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (be > (right) and (be > (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (be > (right) and be > (right))))

        check((left: String, right: String) => left >= right ==> returnsNormally(left should ((be >= (right)) and (be >= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (be >= (right) and (be >= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (be >= (right) and be >= (right))))
      }

      def `should do nothing when array size matches and used in a logical-or expression` {

        check((left: String, right: String) => left < right ==> returnsNormally(left should ((be < (right)) or (be < (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (be < (right) or (be < (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (be < (right) or be < (right))))

        check((left: String, right: String) => left <= right ==> returnsNormally(left should ((be <= (right)) or (be <= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (be <= (right) or (be <= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (be <= (right) or be <= (right))))

        check((left: String, right: String) => left > right ==> returnsNormally(left should ((be > (right)) or (be > (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (be > (right) or (be > (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (be > (right) or be > (right))))

        check((left: String, right: String) => left >= right ==> returnsNormally(left should ((be >= (right)) or (be >= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (be >= (right) or (be >= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (be >= (right) or be >= (right))))

        check((left: String, right: String) => returnsNormally(left should (be >= (right) or be < (right))))
        check((left: String, right: String) => returnsNormally(left should (be > (right) or be <= (right))))
      }

      def `should do nothing when comparison fails and used in a logical-and expression with not` {

        check((left: String, right: String) => left >= right ==> returnsNormally(left should (not (be < (right)) and not (be < (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should ((not be < (right)) and (not be < (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (not be < (right) and not be < (right))))

        check((left: String, right: String) => left > right ==> returnsNormally(left should (not (be <= (right)) and not (be <= (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should ((not be <= (right)) and (not be <= (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (not be <= (right) and not be <= (right))))

        check((left: String, right: String) => left <= right ==> returnsNormally(left should (not (be > (right)) and not (be > (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should ((not be > (right)) and (not be > (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (not be > (right) and not be > (right))))

        check((left: String, right: String) => left < right ==> returnsNormally(left should (not (be >= (right)) and not (be >= (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should ((not be >= (right)) and (not be >= (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (not be >= (right) and not be >= (right))))
      }

      def `should do nothing when comparison fails and used in a logical-or expression with not` {

        check((left: String, right: String) => left > right ==> returnsNormally(left should (not (be >= (right)) or not (be < (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should ((not be >= (right)) or (not be < (right)))))
        check((left: String, right: String) => left > right ==> returnsNormally(left should (not be >= (right) or not be < (right))))

        check((left: String, right: String) => left >= right ==> returnsNormally(left should (not (be > (right)) or not (be <= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should ((not be > (right)) or (not be <= (right)))))
        check((left: String, right: String) => left >= right ==> returnsNormally(left should (not be > (right) or not be <= (right))))

        check((left: String, right: String) => left < right ==> returnsNormally(left should (not (be <= (right)) or not (be > (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should ((not be <= (right)) or (not be > (right)))))
        check((left: String, right: String) => left < right ==> returnsNormally(left should (not be <= (right) or not be > (right))))

        check((left: String, right: String) => left <= right ==> returnsNormally(left should (not (be < (right)) or not (be >= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should ((not be < (right)) or (not be >= (right)))))
        check((left: String, right: String) => left <= right ==> returnsNormally(left should (not be < (right) or not be >= (right))))
      }

      def `should throw TestFailedException if comparison does not succeed` {

        val caught1 = intercept[TestFailedException] {
          "aaa" should be < ("aaa")
        }
        assert(caught1.getMessage === "\"aaa\" was not less than \"aaa\"")
        check((left: String, right: String) => left >= right ==> throwsTestFailedException(left should be < (right)))

        val caught2 = intercept[TestFailedException] {
          "bbb" should be <= ("aaa")
        }
        assert(caught2.getMessage === "\"bbb\" was not less than or equal to \"aaa\"")
        check((left: String, right: String) => left > right ==> throwsTestFailedException(left should be <= (right)))

        val caught3 = intercept[TestFailedException] {
          "aaa" should be > ("aaa")
        }
        assert(caught3.getMessage === "\"aaa\" was not greater than \"aaa\"")
        check((left: String, right: String) => left <= right ==> throwsTestFailedException(left should be > (right)))

        val caught4 = intercept[TestFailedException] {
          "aaa" should be >= ("bbb")
        }
        assert(caught4.getMessage === "\"aaa\" was not greater than or equal to \"bbb\"")
        check((left: String, right: String) => left < right ==> throwsTestFailedException(left should be >= (right)))
      }

      def `should throw TestFailedException if comparison succeeds but used with not` {

        val caught1 = intercept[TestFailedException] {
          "aaa" should not be < ("bbb")
        }
        assert(caught1.getMessage === "\"aaa\" was less than \"bbb\"")
        check((left: String, right: String) => left < right ==> throwsTestFailedException(left should not be < (right)))

        val caught2 = intercept[TestFailedException] {
          "aaa" should not be <= ("aaa")
        }
        assert(caught2.getMessage === "\"aaa\" was less than or equal to \"aaa\"")
        check((left: String, right: String) => left <= right ==> throwsTestFailedException(left should not be <= (right)))

        val caught3 = intercept[TestFailedException] {
          "bbb" should not be > ("aaa")
        }
        assert(caught3.getMessage === "\"bbb\" was greater than \"aaa\"")
        check((left: String, right: String) => left > right ==> throwsTestFailedException(left should not be > (right)))

        val caught4 = intercept[TestFailedException] {
          "aaa" should not be >= ("aaa")
        }
        assert(caught4.getMessage === "\"aaa\" was greater than or equal to \"aaa\"")
        check((left: String, right: String) => left >= right ==> throwsTestFailedException(left should not be >= (right)))
      }

      // Comparison with and
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-and expression used with not` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-and expression used with not` {

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
      def `should throw an assertion error when less than comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when greater than comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when less than or equal to comparison doesn't succeed and used in a logical-or expression used with not` {

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

      def `should throw an assertion error when greater than or equal to comparison doesn't succeed and used in a logical-or expression used with not` {

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
