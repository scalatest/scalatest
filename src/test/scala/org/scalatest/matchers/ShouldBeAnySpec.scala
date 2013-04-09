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

class ShouldBeAnySpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for equality with "be"
  object `The be token` {

    def `should compare arrays structurally` {
      Array(1, 2) should be (Array(1, 2))
    }

    def `should do nothing when equal` {
      1 should be (1)
      1 shouldBe 1

      // objects should equal themselves
      check((s: String) => returnsNormally(s should be (s)))
      check((i: Int) => returnsNormally(i should be (i)))
      
      // a string should equal another string with the same value
      check((s: String) => returnsNormally(s should be (new String(s))))
    }

    def `should do nothing when not equal and used with not` {
      1 should not { be (2) }
      1 should not be (2)

      // unequal objects should not equal each other
      check((s: String, t: String) => s != t ==> returnsNormally(s should not { be (t) }))
      check((s: String, t: String) => s != t ==> returnsNormally(s should not be (t)))
    }

    def `should do nothing when equal and used in a logical-and expression` {
      1 should (be (1) and be (2 - 1))
    }

    def `should do nothing when equal and used in multi-part logical expressions` {

        // Just to make sure these work strung together
        1 should (be (1) and be (1) and be (1) and be (1))
        1 should (be (1) and be (1) or be (1) and be (1) or be (1))
        1 should (
            be (1) and
            be (1) or
            be (1) and
            be (1) or
            be (1)
        )
    }

    def `should do nothing when equal and used in a logical-or expression` {
      1 should { be (1) or be (2 - 1) }
    }

    def `should do nothing when not equal and used in a logical-and expression with not` {
      1 should { not { be (2) } and not { be (3 - 1) }}
      1 should { not be (2) and (not be (3 - 1)) }
      1 should (not be (2) and not be (3 - 1))
    }

    def `should do nothing when not equal and used in a logical-or expression with not` {
      1 should { not { be (2) } or not { be (3 - 1) }}
      1 should { not be (2) or (not be (3 - 1)) }
      1 should (not be (2) or not be (3 - 1))
    }

    def `should throw an assertion error when not equal` {
      val caught1 = intercept[TestFailedException] {
        1 should be (2)
      }
      assert(caught1.getMessage === "1 was not equal to 2")

      // unequal objects used with "a should equal (b)" should throw an TestFailedException
      check((s: String, t: String) => s != t ==> throwsTestFailedException(s should be (t)))

      val caught2 = intercept[TestFailedException] {
        1 should not (not be (2))
      }
      assert(caught2.getMessage === "1 was not equal to 2")

      val s: String = null
      val caught3 = intercept[TestFailedException] {
        s should be ("hi")
      }
      assert(caught3.getMessage === "\"hi\" was not null")
    }

    def `should throw an assertion error when equal but used with should not` {
      val caught1 = intercept[TestFailedException] {
        1 should not { be (1) }
      }
      assert(caught1.getMessage === "1 was equal to 1")

      val caught2 = intercept[TestFailedException] {
        1 should not be (1)
      }
      assert(caught2.getMessage === "1 was equal to 1")

      // the same object used with "a should not { equal (a) } should throw TestFailedException
      check((s: String) => throwsTestFailedException(s should not { be (s) }))
      check((i: Int) => throwsTestFailedException(i should not { be (i) }))
      check((s: String) => throwsTestFailedException(s should not be (s)))
      check((i: Int) => throwsTestFailedException(i should not be (i)))

      // two different strings with the same value used with "s should not { be (t) } should throw TestFailedException
      check((s: String) => throwsTestFailedException(s should not { be (new String(s)) }))
      check((s: String) => throwsTestFailedException(s should not be (new String(s))))

      val caught3 = intercept[TestFailedException] {
        1 should not (not (not be (1)))
      }
      assert(caught3.getMessage === "1 was equal to 1")
    }

    def `should throw an assertion error when not equal and used in a logical-and expression` {
      val caught = intercept[TestFailedException] {
        1 should { be (5) and be (2 - 1) }
      }
      assert(caught.getMessage === "1 was not equal to 5")
    }

    def `should throw an assertion error when not equal and used in a logical-or expression` {
      val caught = intercept[TestFailedException] {
        1 should { be (5) or be (5 - 1) }
      }
      assert(caught.getMessage === "1 was not equal to 5, and 1 was not equal to 4")
    }

    def `should throw an assertion error when equal and used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        1 should { not { be (1) } and not { be (3 - 1) }}
      }
      assert(caught1.getMessage === "1 was equal to 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not be (1) and (not be (3 - 1)) }
      }
      assert(caught2.getMessage === "1 was equal to 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (1) and not be (3 - 1))
      }
      assert(caught3.getMessage === "1 was equal to 1")

      val caught4 = intercept[TestFailedException] {
        1 should { not { be (2) } and not { be (1) }}
      }
      assert(caught4.getMessage === "1 was not equal to 2, but 1 was equal to 1")

      val caught5 = intercept[TestFailedException] {
        1 should { not be (2) and (not be (1)) }
      }
      assert(caught5.getMessage === "1 was not equal to 2, but 1 was equal to 1")

      val caught6 = intercept[TestFailedException] {
        1 should (not be (2) and not be (1))
      }
      assert(caught6.getMessage === "1 was not equal to 2, but 1 was equal to 1")
    }

    def `should throw an assertion error when equal and used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        1 should { not { be (1) } or not { be (2 - 1) }}
      }
      assert(caught1.getMessage === "1 was equal to 1, and 1 was equal to 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not be (1) or { not be (2 - 1) }}
      }
      assert(caught2.getMessage === "1 was equal to 1, and 1 was equal to 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (1) or not be (2 - 1))
      }
      assert(caught3.getMessage === "1 was equal to 1, and 1 was equal to 1")
    }
  }
}
