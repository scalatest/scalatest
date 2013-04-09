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
import scala.reflect.BeanProperty
import org.scalatest.exceptions.TestFailedException

class ShouldBeMatcherSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion with BookPropertyMatchers {

  class OddMatcher extends BeMatcher[Int] {
    def apply(left: Int): MatchResult = {
      MatchResult(
        left % 2 == 1,
        left.toString + " was even",
        left.toString + " was odd"
      )
    }
  }
  val odd = new OddMatcher
  val even = not (odd)

  object `The BeMatcher syntax` {

    def `should do nothing if a BeMatcher matches` {
      1 should be (odd)
      2 should be (even)
      
      1 shouldBe odd
      2 shouldBe even
    }

    def `should throw TestFailedException if a BeMatcher does not match` {

      val caught1 = intercept[TestFailedException] {
        4 should be (odd)
        4 shouldBe odd
      }
      assert(caught1.getMessage === "4 was even")

      val caught2 = intercept[TestFailedException] {
        5 should be (even)
        5 shouldBe even
      }
      assert(caught2.getMessage === "5 was odd")
    }

    def `should do nothing if a BeMatcher does not match, when used with not` {
      2 should not be (odd)
      1 should not be (even)
      22 should not (not (be (even)))
      1 should not (not (be (odd)))
    }

    def `should throw TestFailedException if a BeMatcher matches, when used with not` {

      val caught1 = intercept[TestFailedException] {
        3 should not be (odd)
      }
      assert(caught1.getMessage === "3 was odd")

      val caught2 = intercept[TestFailedException] {
        6 should not be (even)
      }
      assert(caught2.getMessage === "6 was even")

      val caught3 = intercept[TestFailedException] {
        6 should not (not (be (odd)))
      }
      assert(caught3.getMessage === "6 was even")
    }

    def `should do nothing if a BeMatcher matches, when used in a logical-and expression` {
      1 should (be (odd) and be (odd))
      1 should (be (odd) and (be (odd)))
      2 should (be (even) and be (even))
      2 should (be (even) and (be (even)))
    }

    def `should throw TestFailedException if at least one BeMatcher does not match, when used in a logical-or expression` {

      // both false
      val caught1 = intercept[TestFailedException] {
        2 should (be (odd) and be (odd))
      }
      assert(caught1.getMessage === "2 was even")

      val caught2 = intercept[TestFailedException] {
        2 should (be (odd) and (be (odd)))
      }
      assert(caught2.getMessage === "2 was even")

      val caught3 = intercept[TestFailedException] {
        1 should (be (even) and be (even))
      }
      assert(caught3.getMessage === "1 was odd")

      val caught4 = intercept[TestFailedException] {
        1 should (be (even) and (be (even)))
      }
      assert(caught4.getMessage === "1 was odd")


      // first false
      val caught5 = intercept[TestFailedException] {
        1 should (be (even) and be (odd))
      }
      assert(caught5.getMessage === "1 was odd")

      val caught6 = intercept[TestFailedException] {
        1 should (be (even) and (be (odd)))
      }
      assert(caught6.getMessage === "1 was odd")

      val caught7 = intercept[TestFailedException] {
        2 should (be (odd) and be (even))
      }
      assert(caught7.getMessage === "2 was even")

      val caught8 = intercept[TestFailedException] {
        2 should (be (odd) and (be (even)))
      }
      assert(caught8.getMessage === "2 was even")


// TODO: Remember to try a BeMatcher[Any] one, to make sure it works on an Int

      // second false
      val caught9 = intercept[TestFailedException] {
        1 should (be (odd) and be (even))
      }
      assert(caught9.getMessage === "1 was odd, but 1 was odd")

      val caught10 = intercept[TestFailedException] {
        1 should (be (odd) and (be (even)))
      }
      assert(caught10.getMessage === "1 was odd, but 1 was odd")

      val caught11 = intercept[TestFailedException] {
        2 should (be (even) and be (odd))
      }
      assert(caught11.getMessage === "2 was even, but 2 was even")

      val caught12 = intercept[TestFailedException] {
        2 should (be (even) and (be (odd)))
      }
      assert(caught12.getMessage === "2 was even, but 2 was even")
    }

    def `should do nothing if at least one BeMatcher matches, when used in a logical-or expression` {

      // both true
      1 should (be (odd) or be (odd))
      1 should (be (odd) or (be (odd)))
      2 should (be (even) or be (even))
      2 should (be (even) or (be (even)))

      // first false
      1 should (be (even) or be (odd))
      1 should (be (even) or (be (odd)))
      2 should (be (odd) or be (even))
      2 should (be (odd) or (be (even)))

      // second false
      1 should (be (odd) or be (even))
      1 should (be (odd) or (be (even)))
      2 should (be (even) or be (odd))
      2 should (be (even) or (be (odd)))
    }

    def `should throw TestFailedException if a BeMatcher does not match, when used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        2 should (be (odd) or be (odd))
      }
      assert(caught1.getMessage === "2 was even, and 2 was even")

      val caught2 = intercept[TestFailedException] {
        2 should (be (odd) or (be (odd)))
      }
      assert(caught2.getMessage === "2 was even, and 2 was even")

      val caught3 = intercept[TestFailedException] {
        1 should (be (even) or be (even))
      }
      assert(caught3.getMessage === "1 was odd, and 1 was odd")

      val caught4 = intercept[TestFailedException] {
        1 should (be (even) or (be (even)))
      }
      assert(caught4.getMessage === "1 was odd, and 1 was odd")
    }

    def `should do nothing if a BeMatcher does not match, when used in a logical-and expression with not` {
      2 should (not be (odd) and not be (odd))
      2 should (not be (odd) and not (be (odd)))
      2 should (not be (odd) and (not (be (odd))))
      1 should (not be (even) and not be (even))
      1 should (not be (even) and not (be (even)))
      1 should (not be (even) and (not (be (even))))
    }

    def `should throw TestFailedException if at least one BeMatcher matches, when used in a logical-and expression with not` {

      // both true
      val caught1 = intercept[TestFailedException] {
        1 should (not be (odd) and not be (odd))
      }
      assert(caught1.getMessage === "1 was odd")

      val caught2 = intercept[TestFailedException] {
        1 should (not be (odd) and not (be (odd)))
      }
      assert(caught2.getMessage === "1 was odd")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (odd) and (not (be (odd))))
      }
      assert(caught3.getMessage === "1 was odd")

      val caught4 = intercept[TestFailedException] {
        2 should (not be (even) and not be (even))
      }
      assert(caught4.getMessage === "2 was even")

      val caught5 = intercept[TestFailedException] {
        2 should (not be (even) and not (be (even)))
      }
      assert(caught5.getMessage === "2 was even")

      val caught6 = intercept[TestFailedException] {
        2 should (not be (even) and (not (be (even))))
      }
      assert(caught6.getMessage === "2 was even")


      // first false
      val caught7 = intercept[TestFailedException] {
        1 should (not be (even) and not be (odd))
      }
      assert(caught7.getMessage === "1 was odd, but 1 was odd")

      val caught8 = intercept[TestFailedException] {
        1 should (not be (even) and not (be (odd)))
      }
      assert(caught8.getMessage === "1 was odd, but 1 was odd")

      val caught9 = intercept[TestFailedException] {
        1 should (not be (even) and (not (be (odd))))
      }
      assert(caught9.getMessage === "1 was odd, but 1 was odd")

      val caught10 = intercept[TestFailedException] {
        2 should (not be (odd) and not be (even))
      }
      assert(caught10.getMessage === "2 was even, but 2 was even")

      val caught11 = intercept[TestFailedException] {
        2 should (not be (odd) and not (be (even)))
      }
      assert(caught11.getMessage === "2 was even, but 2 was even")

      val caught12 = intercept[TestFailedException] {
        2 should (not be (odd) and (not (be (even))))
      }
      assert(caught12.getMessage === "2 was even, but 2 was even")


      // second false
      val caught13 = intercept[TestFailedException] {
        1 should (not be (odd) and not be (even))
      }
      assert(caught13.getMessage === "1 was odd")

      val caught14 = intercept[TestFailedException] {
        1 should (not be (odd) and not (be (even)))
      }
      assert(caught14.getMessage === "1 was odd")

      val caught15 = intercept[TestFailedException] {
        1 should (not be (odd) and (not (be (even))))
      }
      assert(caught15.getMessage === "1 was odd")

      val caught16 = intercept[TestFailedException] {
        2 should (not be (even) and not be (odd))
      }
      assert(caught16.getMessage === "2 was even")

      val caught17 = intercept[TestFailedException] {
        2 should (not be (even) and not (be (odd)))
      }
      assert(caught17.getMessage === "2 was even")

      val caught18 = intercept[TestFailedException] {
        2 should (not be (even) and (not (be (odd))))
      }
      assert(caught18.getMessage === "2 was even")
    }

    def `should do nothing if at least one BeMatcher doesn't match, when used in a logical-or expression when used with not` {

      // both false
      2 should (not be (odd) or not be (odd))
      2 should (not be (odd) or not (be (odd)))
      2 should (not be (odd) or (not (be (odd))))
      1 should (not be (even) or not be (even))
      1 should (not be (even) or not (be (even)))
      1 should (not be (even) or (not (be (even))))

      // first false
      1 should (not be (even) or not be (odd))
      1 should (not be (even) or not (be (odd)))
      1 should (not be (even) or (not (be (odd))))
      2 should (not be (odd) or not be (even))
      2 should (not be (odd) or not (be (even)))
      2 should (not be (odd) or (not (be (even))))

      // second false
      1 should (not be (odd) or not be (even))
      1 should (not be (odd) or not (be (even)))
      1 should (not be (odd) or (not (be (even))))
      2 should (not be (even) or not be (odd))
      2 should (not be (even) or not (be (odd)))
      2 should (not be (even) or (not (be (odd))))
    }

    def `should throw TestFailedException if both BeMatcher match, when used in a logical-or expression with not` {

      val caught1 = intercept[TestFailedException] {
        1 should (not be (odd) or not be (odd))
      }
      assert(caught1.getMessage === "1 was odd, and 1 was odd")

      val caught2 = intercept[TestFailedException] {
        1 should (not be (odd) or not (be (odd)))
      }
      assert(caught2.getMessage === "1 was odd, and 1 was odd")

      val caught3 = intercept[TestFailedException] {
        1 should (not be (odd) or (not (be (odd))))
      }
      assert(caught3.getMessage === "1 was odd, and 1 was odd")

      val caught4 = intercept[TestFailedException] {
        2 should (not be (even) or not be (even))
      }
      assert(caught4.getMessage === "2 was even, and 2 was even")

      val caught5 = intercept[TestFailedException] {
        2 should (not be (even) or not (be (even)))
      }
      assert(caught5.getMessage === "2 was even, and 2 was even")

      val caught6 = intercept[TestFailedException] {
        2 should (not be (even) or (not (be (even))))
      }
      assert(caught6.getMessage === "2 was even, and 2 was even")
    }

    def `should work when the types aren't exactly the same` {

      class UnlikableMatcher extends BeMatcher[Any] {
        def apply(left: Any): MatchResult = {
          MatchResult(
            false,
            left.toString + " was not to my liking",
            left.toString + " was to my liking"
          )
        }
      }
      val unlikable = new UnlikableMatcher
      val likable = not (unlikable)

      1 should be (likable)
      2 should not be (unlikable)
      
      1 shouldBe likable

      val caught1 = intercept[TestFailedException] {
        1 should be (unlikable)
      }
      assert(caught1.getMessage === "1 was not to my liking")

      val caught2 = intercept[TestFailedException] {
        "The dish" should not be (likable)
      }
      assert(caught2.getMessage === "The dish was not to my liking")
    }
  }
  object `the compose method on BeMatcher` {
    def `should return another BeMatcher` {
      val oddAsInt = odd compose { (s: String) => s.toInt }
      "3" should be (oddAsInt)
      "4" should not be (oddAsInt)
      
      "3" shouldBe oddAsInt
    }
  }
  object `A factory method on BeMatcher's companion object` {
    def `should produce a be-matcher that executes the passed function when its apply is called` {
      val f = { (s: String) => MatchResult(s.length < 3, "s was not less than 3", "s was less than 3") }
      val lessThanThreeInLength = BeMatcher(f)
      "" should be (lessThanThreeInLength)
      "x" should be (lessThanThreeInLength)
      "xx" should be (lessThanThreeInLength)
      "xxx" should not be (lessThanThreeInLength)
      "xxxx" should not be (lessThanThreeInLength)
      
      "" shouldBe lessThanThreeInLength
      "x" shouldBe lessThanThreeInLength
      "xx" shouldBe lessThanThreeInLength
    }
  }
}
