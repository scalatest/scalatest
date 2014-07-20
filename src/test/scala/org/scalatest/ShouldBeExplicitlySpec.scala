/*
 * Copyright 2001-2013 Artima, Inc.
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

import org.scalatest.exceptions._
import org.scalatest.exceptions.TestFailedException
import org.scalactic.Explicitly
import org.scalactic.Equality
import Matchers._

class ShouldBeExplicitlySpec extends Spec with Explicitly {

  implicit val e = new Equality[Int] {
    def areEqual(a: Int, b: Any): Boolean = a != b
  }

  // Checking for beity with "be"
  object `The be token` {

    def `should do nothing when be` {
      intercept[TestFailedException] { 1 should be (1) }
      1 should be (1) (defaultEquality[Int])
      1 should be (1) (decided by defaultEquality[Int])
      (1 should be (1)) (defaultEquality[Int])
      (1 should be (1)) (decided by defaultEquality[Int])
      intercept[TestFailedException] { 1 shouldBe 1 }
      (1 shouldBe 1) (defaultEquality[Int])
      (1 shouldBe 1) (decided by defaultEquality[Int])
      (1).shouldBe(1)(defaultEquality[Int])
      (1).shouldBe(1)(decided by defaultEquality[Int])
    }

    def `should do nothing when not be and used with not` {
      intercept[TestFailedException] { 1 should not { be (2) } }
      intercept[TestFailedException] { 1 should not be (2) }
      1 should not { be (2) } (defaultEquality[Int])
      1 should not { be (2) } (decided by defaultEquality[Int])
      (1 should not be (2)) (defaultEquality[Int])
      (1 should not be (2)) (decided by defaultEquality[Int])
/*
      intercept[TestFailedException] { 1 shouldNot { be (2) } }
      intercept[TestFailedException] { 1 shouldNot be (2) }
      1 shouldNot be (2) (defaultEquality[Int])
      1 shouldNot be (2) (decided by defaultEquality[Int])
*/
    }

    def `should do nothing when be and used in a logical-and expression` {
      intercept[TestFailedException] { 1 should (be (1) and be (2 - 1)) }
      1 should (be (1) and be (2 - 1)) (decided by defaultEquality[Int])
      1 should (be (1) (decided by defaultEquality[Int]) and be (2 - 1) (decided by defaultEquality[Int])) 
    }

    def `should do nothing when be and used in multi-part logical expressions` {

        // Just to make sure these work strung together
        intercept[TestFailedException] { 1 should (be (1) and be (1) and be (1) and be (1)) }
        intercept[TestFailedException] { 1 should (be (1) and be (1) or be (1) and be (1) or be (1)) }
        intercept[TestFailedException] { 1 should (
            be (1) and
            be (1) or
            be (1) and
            be (1) or
            be (1)
        ) }
        1 should (be (1) and be (1) and be (1) and be (1)) (decided by defaultEquality[Int])
        1 should (be (1) and be (1) or be (1) and be (1) or be (1)) (decided by defaultEquality[Int])
        1 should (
            be (1) and
            be (1) or
            be (1) and
            be (1) or
            be (1)
        ) (decided by defaultEquality[Int])
        1 should (be (1) (decided by defaultEquality[Int]) and be (1) (decided by defaultEquality[Int]) and be (1) (decided by defaultEquality[Int]) and be (1) (decided by defaultEquality[Int]))
        1 should (be (1) (decided by defaultEquality[Int]) and be (1) (decided by defaultEquality[Int]) or be (1) (decided by defaultEquality[Int]) and be (1) (decided by defaultEquality[Int]) or be (1) (decided by defaultEquality[Int]))
        1 should (
            be (1) (decided by defaultEquality[Int]) and
            be (1) (decided by defaultEquality[Int]) or
            be (1) (decided by defaultEquality[Int]) and
            be (1) (decided by defaultEquality[Int]) or
            be (1) (decided by defaultEquality[Int])
        )
    }

    def `should do nothing when be and used in a logical-or expression` {
      intercept[TestFailedException] { 1 should { be (1) or be (2 - 1) } }
      1 should (be (1) or be (2 - 1)) (decided by defaultEquality[Int])
      1 should { be (1) (decided by defaultEquality[Int]) or be (2 - 1) (decided by defaultEquality[Int]) }
    }

    def `should do nothing when not be and used in a logical-and expression with not` {
      intercept[TestFailedException] { 1 should (not (be (2)) and not (be (3 - 1))) }
      intercept[TestFailedException] { 1 should (not be (2) and (not be (3 - 1))) }
      // Will back up on these MatcherGen1 and not ones, and do simpler MatcherGen1 and MatcherGen1 ones first
      // intercept[TestFailedException] { 1 should (not be (2) and not be (3 - 1)) }

      1 should (not (be (2)) and not (be (3 - 1))) (decided by defaultEquality[Int])
      1 should (not be (2) and (not be (3 - 1))) (decided by defaultEquality[Int])
      // 1 should (not be (2) and not be (3 - 1)) (decided by defaultEquality[Int])

      1 should (not (be (2) (decided by defaultEquality[Int])) and not (be (3 - 1) (decided by defaultEquality[Int])))
      1 should ((not be 2) (decided by defaultEquality[Int]) and (not be 3 - 1) (decided by defaultEquality[Int]))
    }

/*
    def `should do nothing when not be and used in a logical-or expression with not` {
      1 should { not { be (2) } or not { be (3 - 1) }}
      1 should { not be (2) or (not be (3 - 1)) }
      1 should (not be (2) or not be (3 - 1))
    }

    def `should throw a TFE when not be` {
      val caught1 = intercept[TestFailedException] {
        1 should be (2)
      }
      assert(caught1.getMessage === "1 did not be 2")

      val caught2 = intercept[TestFailedException] {
        1 should not (not be (2))
      }
      assert(caught2.getMessage === "1 did not be 2")

      val caught3 = intercept[TestFailedException] {
        1 shouldBe 2
      }
      assert(caught3.getMessage === "1 did not be 2")
    }

    def `should throw a TFE when be but used with should not` {
      val caught1 = intercept[TestFailedException] {
        1 should not { be (1) }
      }
      assert(caught1.getMessage === "1 beed 1")

      val caught2 = intercept[TestFailedException] {
        1 should not be (1)
      }
      assert(caught2.getMessage === "1 beed 1")

      val caught3 = intercept[TestFailedException] {
        1 should not (not (not be (1)))
      }
      assert(caught3.getMessage === "1 beed 1")
    }

    def `should throw a TFE when not be and used in a logical-and expression` {
      val caught = intercept[TestFailedException] {
        1 should { be (5) and be (2 - 1) }
      }
      assert(caught.getMessage === "1 did not be 5")
    }

    def `should throw a TFE when not be and used in a logical-or expression` {
      val caught = intercept[TestFailedException] {
        1 should { be (5) or be (5 - 1) }
      }
      assert(caught.getMessage === "1 did not be 5, and 1 did not be 4")
    }

    def `should throw a TFE when be and used in a logical-and expression with not` {

      val caught1 = intercept[TestFailedException] {
        1 should { not { be (1) } and not { be (3 - 1) }}
      }
      assert(caught1.getMessage === "1 beed 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not be (1) and (not be (3 - 1)) }
      }
      assert(caught2.getMessage === "1 beed 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (1) and not be (3 - 1))
      }
      assert(caught3.getMessage === "1 beed 1")

      val caught4 = intercept[TestFailedException] {
        1 should { not { be (2) } and not { be (1) }}
      }
      assert(caught4.getMessage === "1 did not be 2, but 1 beed 1")

      val caught5 = intercept[TestFailedException] {
        1 should { not be (2) and (not be (1)) }
      }
      assert(caught5.getMessage === "1 did not be 2, but 1 beed 1")

      val caught6 = intercept[TestFailedException] {
        1 should (not be (2) and not be (1))
      }
      assert(caught6.getMessage === "1 did not be 2, but 1 beed 1")
    }

    def `should throw a TFE when be and used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        1 should { not { be (1) } or not { be (2 - 1) }}
      }
      assert(caught1.getMessage === "1 beed 1, and 1 beed 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not be (1) or { not be (2 - 1) }}
      }
      assert(caught2.getMessage === "1 beed 1, and 1 beed 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (1) or not be (2 - 1))
      }
      assert(caught3.getMessage === "1 beed 1, and 1 beed 1")
    }
    
    def `should put string differences in square bracket` {
      val caught1 = intercept[TestFailedException] { "dummy" should be ("dunny") }
      caught1.getMessage should be ("\"du[mm]y\" did not be \"du[nn]y\"")
      
      val caught2 = intercept[TestFailedException] { "dummy" should be ("dunny") }
      caught2.getMessage should be ("\"du[mm]y\" was not be to \"du[nn]y\"")
      
      val caught3 = intercept[TestFailedException] { "hi there mom" should be ===  ("high there mom") }
      caught3.getMessage should be ("\"hi[] there mom\" was not be to \"hi[gh] there mom\"")
    }
    
    def `should not put string differences in square bracket` {
      val caught1 = intercept[TestFailedException] { "dummy" should not be "dummy" }
      caught1.getMessage should be ("\"dummy\" beed \"dummy\"")
      
      val caught2 = intercept[TestFailedException] { "dummy" should not be "dummy" }
      caught2.getMessage should be ("\"dummy\" was be to \"dummy\"")
    }

    def `should be usable when the left expression results in null` {
      val npe = new NullPointerException
      npe.getMessage should be (null)
    }

    def `should compare arrays structurally` {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      a1 should not be theSameInstanceAs (a2)
      a1 should be (a2)
      intercept[TestFailedException] {
        a1 should be (a3)
      }
    }

    def `should compare arrays deeply` {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      a1 should not be theSameInstanceAs (a2)
      a1 should be (a2)
      intercept[TestFailedException] {
        a1 should be (a3)
      }
    }

    def `should compare arrays containing nulls fine` {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      a1 should not be theSameInstanceAs (a2)
      a1 should be (a2)
      intercept[TestFailedException] {
        a1 should be (a3)
      }
      intercept[TestFailedException] {
        a3 should be (a1)
      }
    }

    def `should compare nulls in a satisfying manner` {
      val n1: String = null
      val n2: String = null
      n1 should be (n2)
      intercept[TestFailedException] {
        n1 should be ("hi")
      }
      intercept[TestFailedException] {
        "hi" should be (n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        n1 should be (a1)
      }
      intercept[TestFailedException] {
        a1 should be (n1)
      }
    }
*/
  }
}
