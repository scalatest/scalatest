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
import org.scalatest.exceptions.TestFailedException

class ShouldBeTripleEqualsSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific size
  object `The 'be === (x)' syntax` {

    object `when used with Arrays` {
      def `should compare arrays structurally` {
        Array(1, 2) should be === Array(1, 2)
        val a1 = Array(1, 2, 3)
        val a2 = Array(1, 2, 3)
        val a3 = Array(4, 5, 6)
        a1 should not be theSameInstanceAs (a2)
        a1 should be === a2
        intercept[TestFailedException] {
          a1 should be === a3
        }
      }
      def `should compare arrays deeply` {
        val a1 = Array(1, Array("a", "b"), 3)
        val a2 = Array(1, Array("a", "b"), 3)
        val a3 = Array(1, Array("c", "d"), 3)
        a1 should not be theSameInstanceAs (a2)
        a1 should be === a2
        intercept[TestFailedException] {
          a1 should be === a3
        }
      }
    }

    object `when used with nulls` {
      def `should not throw NullPointerException` {
        val s: String = null
        intercept[TestFailedException] {
          s should be === Array(1, 2)
        }
        s should be === null
        intercept[TestFailedException] {
          "hi" should be === null
        }
      }
    }

    object `on Int` {

      def `should do nothing if the comparison holds true` {
        check((i: Int) => returnsNormally(i should be === (i)))
        check((i: Int) => returnsNormally(i should be === i))
      }

      def `should do nothing if the comparison fails and used with not` {

        check((left: Int, right: Int) => left != right ==> returnsNormally(left should not be === (right)))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should not (be === (right))))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should not (be === right)))
      }

      def `should do nothing when comparison succeeds and used in a logical-and expression` {

        check((i: Int) => returnsNormally(i should ((be === (i)) and (be === (i)))))
        check((i: Int) => returnsNormally(i should (be === (i) and (be === i))))
        check((i: Int) => returnsNormally(i should (be === i and be === (i))))
        check((i: Int) => returnsNormally(i should (be === i and be === i)))
      }

      def `should do nothing when comparison succeeds and used in a logical-or expression` {

        check((i: Int) => returnsNormally(i should ((be === (i)) or (be === (i)))))
        check((i: Int) => returnsNormally(i should (be === (i) or (be === i))))
        check((i: Int) => returnsNormally(i should (be === i or be === (i))))
        check((i: Int) => returnsNormally(i should (be === i or be === i)))

        check((i: Int) => returnsNormally(i should ((not equal (i)) or (be === (i)))))
        check((i: Int) => returnsNormally(i should (not equal (i) or (be === i))))
        check((i: Int) => returnsNormally(i should ((not equal i or be === (i)))))
        check((i: Int) => returnsNormally(i should ((not equal i) or be === i)))
      }

      def `should do nothing when comparison fails and used in a logical-and expression with not` {

        check((left: Int, right: Int) => left != right ==> returnsNormally(left should (not (be === (right)) and not (be === (right)))))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should ((not be === (right)) and (not be === (right)))))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should (not be === (right) and not be === (right))))
      }

      def `should do nothing when comparison fails and used in a logical-or expression with not` {

        check((left: Int, right: Int) => left != right ==> returnsNormally(left should (not (be === (right)) or not (be === (right)))))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should ((not be === (right)) or (not be === (right)))))
        check((left: Int, right: Int) => left != right ==> returnsNormally(left should (not be === (right) or not be === (right))))
      }

      def `should throw TestFailedException if comparison does not succeed` {

        val caught1 = intercept[TestFailedException] {
          1 should be === (2)
        }
        assert(caught1.getMessage === "1 was not equal to 2")
        check((left: Int, right: Int) => left != right ==> throwsTestFailedException(left should be === (right)))

        val caught2 = intercept[TestFailedException] {
          1 should be === 2
        }
        assert(caught2.getMessage === "1 was not equal to 2")
        check((left: Int, right: Int) => left != right ==> throwsTestFailedException(left should be === right))
      }

      def `should throw TestFailedException if comparison succeeds but used with not` {

        val caught1 = intercept[TestFailedException] {
          1 should not be === (1)
        }
        assert(caught1.getMessage === "1 was equal to 1")
        check((i: Int) => throwsTestFailedException(i should not be === (i)))
      }

      // Comparison with and
      def `should throw TestFailedException when comparison doesn't succeed and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          2 should { be === (2) and (be === (5)) }
        }
        assert(caught1.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught2 = intercept[TestFailedException] {
          2 should ((be === (2)) and (be === (5)))
        }
        assert(caught2.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught3 = intercept[TestFailedException] {
          2 should (be === (2) and be === (5))
        }
        assert(caught3.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught4 = intercept[TestFailedException] {
          2 should (be === 2 and be === 5)
        }
        assert(caught4.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught5 = intercept[TestFailedException] {
          2 should (be === 5 and be === 2)
        }
        assert(caught5.getMessage === "2 was not equal to 5")
      }

      // Comparison with or
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-or expression` {


        val caught1 = intercept[TestFailedException] {
          2 should { be === (3) or (be === (5)) }
        }
        assert(caught1.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught2 = intercept[TestFailedException] {
          2 should ((be === (3)) or (be === (5)))
        }
        assert(caught2.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught3 = intercept[TestFailedException] {
          2 should (be === (3) or be === (5))
        }
        assert(caught3.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught4 = intercept[TestFailedException] {
          2 should (be === 3 or be === 5)
        }
        assert(caught4.getMessage === "2 was not equal to 3, and 2 was not equal to 5")
      }

      // Comparison with and not
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be === (2) } and not { be === (5) }}
        }
        assert(caught1.getMessage === "5 was not equal to 2, but 5 was equal to 5")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be === (2)) and (not be === (5)))
        }
        assert(caught2.getMessage === "5 was not equal to 2, but 5 was equal to 5")

        val caught3 = intercept[TestFailedException] {
          5 should (not be === (2) and not be === (5))
        }
        assert(caught3.getMessage === "5 was not equal to 2, but 5 was equal to 5")
      }

      // Comparison with or not
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-or expression used with not` {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be === (5) } or not { be === (5) }}
        }
        assert(caught1.getMessage === "5 was equal to 5, and 5 was equal to 5")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be === (5)) or (not be === (5)))
        }
        assert(caught2.getMessage === "5 was equal to 5, and 5 was equal to 5")

        val caught3 = intercept[TestFailedException] {
          // 5 should ((not be === (5)).or(not).be(===(5)))
          5 should (not be === (5) or not be === (5))
        }
        assert(caught3.getMessage === "5 was equal to 5, and 5 was equal to 5")
      }
    }

    object `on String` {

      def `should do nothing if the comparison holds true` {
        check((s: String) => returnsNormally(s should be === (s)))
        check((s: String) => returnsNormally(s should be === s))
      }

      def `should do nothing if the comparison fails and used with not` {

        check((left: String, right: String) => left != right ==> returnsNormally(left should not be === (right)))
        check((left: String, right: String) => left != right ==> returnsNormally(left should not (be === (right))))
        check((left: String, right: String) => left != right ==> returnsNormally(left should not (be === right)))
      }

      def `should do nothing when comparison succeeds and used in a logical-and expression` {

        check((s: String) => returnsNormally(s should ((be === (s)) and (be === (s)))))
        check((s: String) => returnsNormally(s should (be === (s) and (be === s))))
        check((s: String) => returnsNormally(s should (be === s and be === (s))))
        check((s: String) => returnsNormally(s should (be === s and be === s)))
      }

      def `should do nothing when comparison succeeds and used in a logical-or expression` {

        check((s: String) => returnsNormally(s should ((be === (s)) or (be === (s)))))
        check((s: String) => returnsNormally(s should (be === (s) or (be === s))))
        check((s: String) => returnsNormally(s should (be === s or be === (s))))
        check((s: String) => returnsNormally(s should (be === s or be === s)))

        check((s: String) => returnsNormally(s should ((not equal (s)) or (be === (s)))))
        check((s: String) => returnsNormally(s should (not equal (s) or (be === s))))
        check((s: String) => returnsNormally(s should ((not equal s or be === (s)))))
        check((s: String) => returnsNormally(s should ((not equal s) or be === s)))
      }

      def `should do nothing when comparison fails and used in a logical-and expression with not` {

        check((left: String, right: String) => left != right ==> returnsNormally(left should (not (be === (right)) and not (be === (right)))))
        check((left: String, right: String) => left != right ==> returnsNormally(left should ((not be === (right)) and (not be === (right)))))
        check((left: String, right: String) => left != right ==> returnsNormally(left should (not be === (right) and not be === (right))))
      }

      def `should do nothing when comparison fails and used in a logical-or expression with not` {

        check((left: String, right: String) => left != right ==> returnsNormally(left should (not (be === (right)) or not (be === (right)))))
        check((left: String, right: String) => left != right ==> returnsNormally(left should ((not be === (right)) or (not be === (right)))))
        check((left: String, right: String) => left != right ==> returnsNormally(left should (not be === (right) or not be === (right))))
      }

      def `should throw TestFailedException if comparison does not succeed` {

        val caught1 = intercept[TestFailedException] {
          1 should be === (2)
        }
        assert(caught1.getMessage === "1 was not equal to 2")
        check((left: String, right: String) => left != right ==> throwsTestFailedException(left should be === (right)))

        val caught2 = intercept[TestFailedException] {
          1 should be === 2
        }
        assert(caught2.getMessage === "1 was not equal to 2")
        check((left: String, right: String) => left != right ==> throwsTestFailedException(left should be === right))
      }

      def `should throw TestFailedException if comparison succeeds but used with not` {

        val caught1 = intercept[TestFailedException] {
          1 should not be === (1)
        }
        assert(caught1.getMessage === "1 was equal to 1")
        check((s: String) => throwsTestFailedException(s should not be === (s)))
      }

      // Comparison with and
      def `should throw TestFailedException when comparison doesn't succeed and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          2 should { be === (2) and (be === (5)) }
        }
        assert(caught1.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught2 = intercept[TestFailedException] {
          2 should ((be === (2)) and (be === (5)))
        }
        assert(caught2.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught3 = intercept[TestFailedException] {
          2 should (be === (2) and be === (5))
        }
        assert(caught3.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught4 = intercept[TestFailedException] {
          2 should (be === 2 and be === 5)
        }
        assert(caught4.getMessage === "2 was equal to 2, but 2 was not equal to 5")

        val caught5 = intercept[TestFailedException] {
          2 should (be === 5 and be === 2)
        }
        assert(caught5.getMessage === "2 was not equal to 5")
      }

      // Comparison with or
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-or expression` {


        val caught1 = intercept[TestFailedException] {
          2 should { be === (3) or (be === (5)) }
        }
        assert(caught1.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught2 = intercept[TestFailedException] {
          2 should ((be === (3)) or (be === (5)))
        }
        assert(caught2.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught3 = intercept[TestFailedException] {
          2 should (be === (3) or be === (5))
        }
        assert(caught3.getMessage === "2 was not equal to 3, and 2 was not equal to 5")

        val caught4 = intercept[TestFailedException] {
          2 should (be === 3 or be === 5)
        }
        assert(caught4.getMessage === "2 was not equal to 3, and 2 was not equal to 5")
      }

      // Comparison with and not
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be === (2) } and not { be === (5) }}
        }
        assert(caught1.getMessage === "5 was not equal to 2, but 5 was equal to 5")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be === (2)) and (not be === (5)))
        }
        assert(caught2.getMessage === "5 was not equal to 2, but 5 was equal to 5")

        val caught3 = intercept[TestFailedException] {
          5 should (not be === (2) and not be === (5))
        }
        assert(caught3.getMessage === "5 was not equal to 2, but 5 was equal to 5")
      }

      // Comparison with or not
      def `should throw throw TestFailedException when comparison doesn't succeed and used in a logical-or expression used with not` {

        val caught1 = intercept[TestFailedException] {
          5 should { not { be === (5) } or not { be === (5) }}
        }
        assert(caught1.getMessage === "5 was equal to 5, and 5 was equal to 5")

        val caught2 = intercept[TestFailedException] {
          5 should ((not be === (5)) or (not be === (5)))
        }
        assert(caught2.getMessage === "5 was equal to 5, and 5 was equal to 5")

        val caught3 = intercept[TestFailedException] {
          // 5 should ((not be === (5)).or(not).be(===(5)))
          5 should (not be === (5) or not be === (5))
        }
        assert(caught3.getMessage === "5 was equal to 5, and 5 was equal to 5")
      }
    }
  }
}
