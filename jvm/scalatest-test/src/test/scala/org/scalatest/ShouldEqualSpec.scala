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
import org.scalatest.exceptions._
import org.scalatest.exceptions.TestFailedException
import org.scalactic.Good
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldEqualSpec extends AnyFunSpec with PropertyChecks with ReturnsNormallyThrowsAssertion {

  // Checking for equality with "equal"
  describe("The equal token") {

    it("should do nothing when equal") {
      1 should equal (1)
      1 shouldEqual 1

      // objects should equal themselves
      forAll((s: String) => s should equal (s))
      forAll((i: Int) => i should equal (i))

      // a string should equal another string with the same value
      forAll((s: String) => s should equal (new String(s)))
    }

    it("should work correctly with List(Good(3)).combined shouldBe Good(List(3))") {
      //List(Good(3)).combined shouldBe Good(List(3))

      import org.scalactic.Accumulation._

      List(Good(3)).combined shouldEqual Good(List(3))
    }

    it("should do nothing when not equal and used with not") {
      1 should not { equal (2) }
      1 should not equal (2)

      // unequal objects should not equal each other
      forAll((s: String, t: String) => if (s != t) s should not { equal (t) } else succeed)
      forAll((s: String, t: String) => if (s != t) s should not equal (t) else succeed)
    }

    it("should do nothing when equal and used in a logical-and expression") {
      1 should (equal (1) and equal (2 - 1))
    }

    it("should do nothing when equal and used in multi-part logical expressions") {

        // Just to make sure these work strung together
        1 should (equal (1) and equal (1) and equal (1) and equal (1))
        1 should (equal (1) and equal (1) or equal (1) and equal (1) or equal (1))
        1 should (
            equal (1) and
            equal (1) or
            equal (1) and
            equal (1) or
            equal (1)
        )
    }

    it("should do nothing when equal and used in a logical-or expression") {
      1 should { equal (1) or equal (2 - 1) }
    }

    it("should do nothing when not equal and used in a logical-and expression with not") {
      1 should { not { equal (2) } and not { equal (3 - 1) }}
      1 should { not equal (2) and (not equal (3 - 1)) }
      1 should (not equal (2) and not equal (3 - 1))
    }

    it("should do nothing when not equal and used in a logical-or expression with not") {
      1 should { not { equal (2) } or not { equal (3 - 1) }}
      1 should { not equal (2) or (not equal (3 - 1)) }
      1 should (not equal (2) or not equal (3 - 1))
    }

    it("should throw a TFE when not equal") {
      val caught1 = intercept[TestFailedException] {
        1 should equal (2)
      }
      assert(caught1.getMessage === "1 did not equal 2")

      // unequal objects used with "a should equal (b)" should throw an TestFailedException
      forAll((s: String, t: String) => if (s != t) assertThrows[TestFailedException](s should equal (t)) else succeed)

      val caught2 = intercept[TestFailedException] {
        1 should not (not equal (2))
      }
      assert(caught2.getMessage === "1 did not equal 2")

      val caught3 = intercept[TestFailedException] {
        1 shouldEqual 2
      }
      assert(caught3.getMessage === "1 did not equal 2")
    }

    it("should throw a TFE when equal but used with should not") {
      val caught1 = intercept[TestFailedException] {
        1 should not { equal (1) }
      }
      assert(caught1.getMessage === "1 equaled 1")

      val caught2 = intercept[TestFailedException] {
        1 should not equal (1)
      }
      assert(caught2.getMessage === "1 equaled 1")

      // the same object used with "a should not { equal (a) } should throw TestFailedException
      forAll((s: String) => assertThrows[TestFailedException](s should not { equal (s) }))
      forAll((i: Int) => assertThrows[TestFailedException](i should not { equal (i) }))
      forAll((s: String) => assertThrows[TestFailedException](s should not equal (s)))
      forAll((i: Int) => assertThrows[TestFailedException](i should not equal (i)))

      // two different strings with the same value used with "s should not { equal (t) } should throw TestFailedException
      forAll((s: String) => assertThrows[TestFailedException](s should not { equal (new String(s)) }))
      forAll((s: String) => assertThrows[TestFailedException](s should not equal (new String(s))))

      val caught3 = intercept[TestFailedException] {
        1 should not (not (not equal (1)))
      }
      assert(caught3.getMessage === "1 equaled 1")
    }

    it("should throw a TFE when not equal and used in a logical-and expression") {
      val caught = intercept[TestFailedException] {
        1 should { equal (5) and equal (2 - 1) }
      }
      assert(caught.getMessage === "1 did not equal 5")
    }

    it("should throw a TFE when not equal and used in a logical-or expression") {
      val caught = intercept[TestFailedException] {
        1 should { equal (5) or equal (5 - 1) }
      }
      assert(caught.getMessage === "1 did not equal 5, and 1 did not equal 4")
    }

    it("should throw a TFE when equal and used in a logical-and expression with not") {

      val caught1 = intercept[TestFailedException] {
        1 should { not { equal (1) } and not { equal (3 - 1) }}
      }
      assert(caught1.getMessage === "1 equaled 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not equal (1) and (not equal (3 - 1)) }
      }
      assert(caught2.getMessage === "1 equaled 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not equal (1) and not equal (3 - 1))
      }
      assert(caught3.getMessage === "1 equaled 1")

      val caught4 = intercept[TestFailedException] {
        1 should { not { equal (2) } and not { equal (1) }}
      }
      assert(caught4.getMessage === "1 did not equal 2, but 1 equaled 1")

      val caught5 = intercept[TestFailedException] {
        1 should { not equal (2) and (not equal (1)) }
      }
      assert(caught5.getMessage === "1 did not equal 2, but 1 equaled 1")

      val caught6 = intercept[TestFailedException] {
        1 should (not equal (2) and not equal (1))
      }
      assert(caught6.getMessage === "1 did not equal 2, but 1 equaled 1")
    }

    it("should throw a TFE when equal and used in a logical-or expression with not") {

      val caught1 = intercept[TestFailedException] {
        1 should { not { equal (1) } or not { equal (2 - 1) }}
      }
      assert(caught1.getMessage === "1 equaled 1, and 1 equaled 1")

      val caught2 = intercept[TestFailedException] {
        1 should { not equal (1) or { not equal (2 - 1) }}
      }
      assert(caught2.getMessage === "1 equaled 1, and 1 equaled 1")

      val caught3 = intercept[TestFailedException] {
        1 should (not equal (1) or not equal (2 - 1))
      }
      assert(caught3.getMessage === "1 equaled 1, and 1 equaled 1")
    }
    
    it("should put string differences in square bracket") {
      val caught1 = intercept[TestFailedException] { "dummy" should equal ("dunny") }
      caught1.getMessage should equal ("\"du[mm]y\" did not equal \"du[nn]y\"")
      
      val caught2 = intercept[TestFailedException] { "dummy" should be ("dunny") }
      caught2.getMessage should be ("\"du[mm]y\" was not equal to \"du[nn]y\"")
      
//      val caught3 = intercept[TestFailedException] { "hi there mom" should ===  ("high there mom") }
//      caught3.getMessage should be ("\"hi[] there mom\" was not equal to \"hi[gh] there mom\"")
      
      val caught4 = intercept[TestFailedException] { "dummy" should (equal ("dummy") and (equal ("dunny"))) }
      caught4.getMessage should be ("\"dummy\" equaled \"dummy\", but \"du[mm]y\" did not equal \"du[nn]y\"")
    }
    
    it("should not put string differences in square bracket") {
      val caught1 = intercept[TestFailedException] { "dummy" should not equal "dummy" }
      caught1.getMessage should equal ("\"dummy\" equaled \"dummy\"")
      
      val caught2 = intercept[TestFailedException] { "dummy" should not be "dummy" }
      caught2.getMessage should equal ("\"dummy\" was equal to \"dummy\"")
    }

    it("should be usable when the left expression results in null") {
      val npe = new NullPointerException
      npe.getMessage should equal (null)
    }

    it("should compare arrays structurally") {
      val a1 = Array(1, 2, 3)
      val a2 = Array(1, 2, 3)
      val a3 = Array(4, 5, 6)
      a1 should not be theSameInstanceAs (a2)
      a1 should equal (a2)
      intercept[TestFailedException] {
        a1 should equal (a3)
      }
    }

    it("should compare arrays deeply") {
      val a1 = Array(1, Array("a", "b"), 3)
      val a2 = Array(1, Array("a", "b"), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      a1 should not be theSameInstanceAs (a2)
      a1 should equal (a2)
      intercept[TestFailedException] {
        a1 should equal (a3)
      }
    }

    it("should compare arrays containing nulls fine") {
      val a1 = Array(1, Array("a", null), 3)
      val a2 = Array(1, Array("a", null), 3)
      val a3 = Array(1, Array("c", "d"), 3)
      a1 should not be theSameInstanceAs (a2)
      a1 should equal (a2)
      intercept[TestFailedException] {
        a1 should equal (a3)
      }
      intercept[TestFailedException] {
        a3 should equal (a1)
      }
    }

    it("should compare nulls in a satisfying manner") {
      val n1: String = null
      val n2: String = null
      n1 should equal (n2)
      intercept[TestFailedException] {
        n1 should equal ("hi")
      }
      intercept[TestFailedException] {
        "hi" should equal (n1)
      }
      val a1 = Array(1, 2, 3)
      intercept[TestFailedException] {
        n1 should equal (a1)
      }
      intercept[TestFailedException] {
        a1 should equal (n1)
      }
    }

    it("should show escaped string in analysis") {
      val a = "\u0000test"
      val b = "test"
      val e = 
        intercept[TestFailedException] {
          a should equal (b)
        }
      e.analysis should be (Vector("\"[\\u0000]test\" -> \"[]test\""))
      val e2 = 
        intercept[TestFailedException] {
          a shouldEqual (b)
        }
      e2.analysis should be (Vector("\"[\\u0000]test\" -> \"[]test\""))
    }
  }
}
