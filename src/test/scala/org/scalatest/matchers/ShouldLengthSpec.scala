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
import Integer.MIN_VALUE
import org.scalatest.exceptions.TestFailedException
import org.scalatest.enablers.Length
import org.scalatest.enablers.Size

class ShouldLengthSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  // Checking for a specific length
  object `The 'have length (Int)' syntax` {

    object `on String` {

      def `should do nothing if string length matches specified length` {
        "hi" should have length (2)
        check((s: String) => returnsNormally(s should have length (s.length)))
      }

      def `should do nothing if string length does not match and used with should not` {
        "hi" should not { have length (3) }
        "hi" should not have length (3)
        check((s: String, i: Int) => i != s.length ==> returnsNormally(s should not { have length (i) }))
        check((s: String, i: Int) => i != s.length ==> returnsNormally(s should not have length (i)))
      }

      def `should do nothing when string length matches and used in a logical-and expression` {
        "hi" should (have length (2) and (have length (3 - 1)))
        "hi" should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when string length matches and used in a logical-or expression` {
        "hi" should { have length (77) or (have length (3 - 1)) }
        "hi" should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when string length doesn't match and used in a logical-and expression with not` {
        "hi" should (not (have length (5)) and not (have length (3)))
        "hi" should { not have length (5) and (not have length (3)) }
        "hi" should (not have length (5) and not have length (3))
      }

      def `should do nothing when string length doesn't match and used in a logical-or expression with not` {
        "hi" should (not (have length (2)) or not (have length (3)))
        "hi" should ((not have length (2)) or (not have length (3)))
        "hi" should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if string length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          "hi" should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 3))
        check((s: String) => throwsTestFailedException(s should have length (s.length + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          "hi" should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, -2))
        check((s: String) => throwsTestFailedException(s should have length (if (s.length == 0) -1 else -s.length)))
      }

      def `should throw an assertion error when string length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          "hi" should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 5))

        val caught2 = intercept[TestFailedException] {
          "hi" should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 5))

        val caught3 = intercept[TestFailedException] {
          "hi" should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 5))
      }

      def `should throw an assertion error when string length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          "hi" should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          "hi" should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          "hi" should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 22))))
      }

      def `should throw an assertion error when string length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          "hi" should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("\"hi\""), 2))))

        val caught2 = intercept[TestFailedException] {
          "hi" should { not have length (3) and (not have length (2)) }
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("\"hi\""), 2))))

        val caught3 = intercept[TestFailedException] {
          "hi" should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("\"hi\""), 2))))
      }

      def `should throw an assertion error when string length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          "hi" should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "\"hi\" had length 2, and \"hi\" had length 2")

        val caught2 = intercept[TestFailedException] {
          "hi" should { not have length (2) or (not have length (2)) }
        }
        assert(caught2.getMessage === "\"hi\" had length 2, and \"hi\" had length 2")

        val caught3 = intercept[TestFailedException] {
          "hi" should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "\"hi\" had length 2, and \"hi\" had length 2")
      }

      def `should give good error messages when more than two clauses are used with logical connectors` {

        val caught1 = intercept[TestFailedException] {
          "hi" should (not have length (1) and not have length (3) and not have length (2))
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 1)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("\"hi\""), 2, 3)))), UnquotedString(FailureMessages("hadLength", UnquotedString("\"hi\""), 2))))

        val caught2 = intercept[TestFailedException] {
          "hi" should (not have length (2) or not equal ("hi") or equal ("frog"))
        }
        assert(caught2.getMessage === "\"hi\" had length 2, and \"hi\" equaled \"hi\", and \"[hi]\" did not equal \"[frog]\"")
      }
    }

    object `on Array` {

      def `should do nothing if array length matches specified length` {
        Array(1, 2) should have length (2)
        // check((arr: Array[Int]) => returnsNormally(arr should have length (arr.length)))
      }

      def `should do nothing if array length does not match and used with should not` {
        Array(1, 2) should not { have length (3) }
        Array(1, 2) should not have length (3)
        // check((arr: Array[Int], i: Int) => i != arr.length ==> returnsNormally(arr should not { have length (i) }))
        // check((arr: Array[Int], i: Int) => i != arr.length ==> returnsNormally(arr should not have length (i)))
      }

      def `should do nothing when array length matches and used in a logical-and expression` {
        Array(1, 2) should { have length (2) and (have length (3 - 1)) }
        Array(1, 2) should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when array length matches and used in a logical-or expression` {
        Array(1, 2) should { have length (77) or (have length (3 - 1)) }
        Array(1, 2) should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when array length doesn't match and used in a logical-and expression with not` {
        Array(1, 2) should { not { have length (5) } and not { have length (3) }}
        Array(1, 2) should { not have length (5) and (not have length (3)) }
        Array(1, 2) should (not have length (5) and not have length (3))
      }

      def `should do nothing when array length doesn't match and used in a logical-or expression with not` {
        Array(1, 2) should { not { have length (2) } or not { have length (3) }}
        Array(1, 2) should { not have length (2) or (not have length (3)) }
        Array(1, 2) should (not have length (5) and not have length (3))
      }

      def `should throw TestFailedException if array length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should have length (3)
        }
        assert(caught1.getMessage.endsWith(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 3)))
        // check((arr: Array[String]) => throwsTestFailedException(arr should have length (arr.length + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should have length (-2)
        }
        assert(caught1.getMessage.endsWith(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, -2)))
        // check((arr: Array[Int]) => throwsTestFailedException(arr should have length (if (arr.length == 0) -1 else -arr.length)))
      }

      def `should throw an assertion error when array length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 5))
      }

      def `should throw an assertion error when array length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 22))))
      }

      def `should throw an assertion error when array length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("Array(1, 2)"), 2))))

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should { not have length (3) and (not have length (2)) }
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("Array(1, 2)"), 2))))

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("Array(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("Array(1, 2)"), 2))))
      }

      def `should throw an assertion error when array length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          Array(1, 2) should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "Array(1, 2) had length 2, and Array(1, 2) had length 2")

        val caught2 = intercept[TestFailedException] {
          Array(1, 2) should { not have length (2) or (not have length (2)) }
        }
        assert(caught2.getMessage === "Array(1, 2) had length 2, and Array(1, 2) had length 2")

        val caught3 = intercept[TestFailedException] {
          Array(1, 2) should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "Array(1, 2) had length 2, and Array(1, 2) had length 2")
      }

      def `should work on parallel form` {
        Array(1, 2).par should have length (2)
      }
    }

    object `on scala.List` {

      def `should do nothing if list length matches specified length` {
        List(1, 2) should have length (2)
        check((lst: List[Int]) => returnsNormally(lst should have length (lst.length)))
      }

      def `should do nothing if list length does not match and used with should not` {
        List(1, 2) should not { have length (3) }
        List(1, 2) should not have length (3)
        check((lst: List[Int], i: Int) => i != lst.length ==> returnsNormally(lst should not { have length (i) }))
        check((lst: List[Int], i: Int) => i != lst.length ==> returnsNormally(lst should not have length (i)))
      }

      def `should do nothing when list length matches and used in a logical-and expression` {
        List(1, 2) should { have length (2) and (have length (3 - 1)) }
        List(1, 2) should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when list length matches and used in a logical-or expression` {
        List(1, 2) should { have length (77) or (have length (3 - 1)) }
        List(1, 2) should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when list length doesn't match and used in a logical-and expression with not` {
        List(1, 2) should { not { have length (5) } and not { have length (3) }}
        List(1, 2) should { not have length (5) and (not have length (3)) }  
      }

      def `should do nothing when list length doesn't match and used in a logical-or expression with not` {
        List(1, 2) should { not { have length (2) } or not { have length (3) }}
        List(1, 2) should { not have length (2) or (not have length (3)) }
        List(1, 2) should (not have length (5) and not have length (3))
      }

      def `should throw TestFailedException if list length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          List(1, 2) should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 3))
        check((lst: List[String]) => throwsTestFailedException(lst should have length (lst.length + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          List(1, 2) should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, -2))
        check((lst: List[Int]) => throwsTestFailedException(lst should have length (if (lst.length == 0) -1 else -lst.length)))
      }

      def `should throw an assertion error when list length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 5))
      }

      def `should throw an assertion error when list length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 22))))
      }

      def `should throw an assertion error when list length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("List(1, 2)"), 2))))

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should { not have length (3) and (not have length (2)) }
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("List(1, 2)"), 2))))

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("List(1, 2)"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("List(1, 2)"), 2))))
      }

      def `should throw an assertion error when list length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          List(1, 2) should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "List(1, 2) had length 2, and List(1, 2) had length 2")

        val caught2 = intercept[TestFailedException] {
          List(1, 2) should { not have length (2) or (not have length (2)) }
        }
        assert(caught2.getMessage === "List(1, 2) had length 2, and List(1, 2) had length 2")

        val caught3 = intercept[TestFailedException] {
          List(1, 2) should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "List(1, 2) had length 2, and List(1, 2) had length 2")
      }

      def `should work on parallel form` {
        List(1, 2).par should have length (2)
      }
    }

    object `on java.util.List` {

      val javaList: java.util.List[Int] = new java.util.ArrayList
      javaList.add(1)
      javaList.add(2)
      
      def `should do nothing if list length matches specified length` {
        javaList should have length (2)
        // check((lst: java.util.List[Int]) => returnsNormally(lst should have length (lst.length)))
      }

      def `should do nothing if list length does not match and used with should not` {
        javaList should not { have length (3) }
        javaList should not have length (3)
        // check((lst: List[Int], i: Int) => i != lst.length ==> returnsNormally(lst should not { have length (i) }))
      }

      def `should do nothing when list length matches and used in a logical-and expression` {
        javaList should { have length (2) and (have length (3 - 1)) }
        javaList should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when list length matches and used in a logical-or expression` {
        javaList should { have length (77) or (have length (3 - 1)) }
        javaList should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when list length doesn't match and used in a logical-and expression with not` {
        javaList should { not { have length (5) } and not { have length (3) }}
        javaList should (not have length (5) and not have length (3))
      }

      def `should do nothing when list length doesn't match and used in a logical-or expression with not` {
        javaList should { not { have length (2) } or not { have length (3) }}
        javaList should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if list length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          javaList should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 3))
        // check((lst: List[String]) => throwsTestFailedException(lst should have length (lst.length + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          javaList should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, -2))
        // check((lst: List[Int]) => throwsTestFailedException(lst should have length (if (lst.length == 0) -1 else -lst.length)))
      }

      def `should throw an assertion error when list length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          javaList should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          javaList should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          javaList should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 5))
      }

      def `should throw an assertion error when list length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          javaList should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          javaList should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          javaList should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 22))))
      }

      def `should throw an assertion error when list length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaList should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("[1, 2]"), 2))))

        val caught2 = intercept[TestFailedException] {
          javaList should { not have length (3) and (not have length (2)) }
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("[1, 2]"), 2))))

        val caught3 = intercept[TestFailedException] {
          javaList should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("[1, 2]"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("[1, 2]"), 2))))
      }

      def `should throw an assertion error when list length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          javaList should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "[1, 2] had length 2, and [1, 2] had length 2")

        val caught2 = intercept[TestFailedException] {
          javaList should { not have length (2) or (not have length (2)) }
        }
        assert(caught2.getMessage === "[1, 2] had length 2, and [1, 2] had length 2")

        val caught3 = intercept[TestFailedException] {
          javaList should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "[1, 2] had length 2, and [1, 2] had length 2")
      }
    }

    // I repeat these with copy and paste, becuase I need to test that each static structural type works, and
    // that makes it hard to pass them to a common "behaves like" method
    object `on an arbitrary object that has an empty-paren Int length method` {
  
      class Lengthy(len: Int) {
        def length(): Int = len
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)
  
      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length()
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }
  
      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }
  
      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }
  
      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }
  
      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }
  
      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }
  
      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }
  
      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }
  
      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }
  
      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }
  
      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }
  
      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a parameterless Int length method` {

      class Lengthy(len: Int) {
        def length: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a Int length field` {

      class Lengthy(len: Int) {
        val length: Int = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has an empty-paren Int getLength method` {

      class Lengthy(len: Int) {
        def getLength(): Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength()
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a parameterless Int getLength method` {

      class Lengthy(len: Int) {
        def getLength: Int = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has an Int getLength field` {

      class Lengthy(len: Int) {
        val getLength: Int = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has an empty-paren Long length method` {

      class Lengthy(len: Long) {
        def length(): Long = len
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length()
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should { have length (2L) and (have length (3 - 1)) }
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (2L)) }
        obj should { have length (77L) or (have length (2)) }
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a parameterless Long length method` {

      class Lengthy(len: Long) {
        def length: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a Long length field` {

      class Lengthy(len: Long) {
        val length: Long = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has an empty-paren Long getLength method` {

      class Lengthy(len: Long) {
        def getLength(): Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength()
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a parameterless Long getLength method` {

      class Lengthy(len: Long) {
        def getLength: Long = len  // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has a Long getLength field` {

      class Lengthy(len: Long) {
        val getLength: Long = len // The only difference between the previous is the structure of this member
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.getLength
        }

      def `should do nothing if object length matches specified length` {
        obj should have length (2)
        obj should have length (2L)
        check((len: Int) => returnsNormally(new Lengthy(len) should have length (len)))
        check((len: Long) => returnsNormally(new Lengthy(len) should have length (len)))
      }

      def `should do nothing if object length does not match and used with should not` {
        obj should not { have length (3) }
        obj should not { have length (3L) }
        obj should not have length (3)
        obj should not have length (3L)
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not { have length (wrongLen) }))
        check((len: Int, wrongLen: Int) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
        check((len: Long, wrongLen: Long) => len != wrongLen ==> returnsNormally(new Lengthy(len) should not have length (wrongLen)))
      }

      def `should do nothing when object length matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
      }

      def `should do nothing when object length matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
      }

      def `should do nothing when object length doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
      }

      def `should do nothing when object length doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
      }

      def `should throw TestFailedException if object length does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (len + 1)))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        check((len: Int) => throwsTestFailedException(new Lengthy(len) should have length (if ((len == 0) || (len == MIN_VALUE)) -1 else -len)))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")
      }
    }

    object `on an arbitrary object that has both parameterless Int length and parameterless Int size methods` {

      class Lengthy(len: Int) {
        def length: Int = len
        def size: Int = len  
        override def toString = "lengthy"
      }
      val obj = new Lengthy(2)

      implicit val lengthOfLengthy =
        new Length[Lengthy] with Size[Lengthy] {
          def lengthOf(o: Lengthy): Long = o.length
          def sizeOf(o: Lengthy): Long = o.size
        }

      def `should do nothing if object length or size matches specified length` {
        obj should have length (2)
        obj should have size (2)
      }

      def `should do nothing if object length or size does not match and used with should not` {
        obj should not { have length (3) }
        obj should not have length (3)
        obj should not { have size (3) }
        obj should not have size (3)
      }

      def `should do nothing when object length or size matches and used in a logical-and expression` {
        obj should { have length (2) and (have length (3 - 1)) }
        obj should (have length (2) and have length (3 - 1))
        obj should { have size (2) and (have size (3 - 1)) }
        obj should (have size (2) and have size (3 - 1))
      }

      def `should do nothing when object length or size matches and used in a logical-or expression` {
        obj should { have length (77) or (have length (3 - 1)) }
        obj should (have length (77) or have length (3 - 1))
        obj should { have size (77) or (have size (3 - 1)) }
        obj should (have size (77) or have size (3 - 1))
      }

      def `should do nothing when object length or size doesn't match and used in a logical-and expression with not` {
        obj should { not { have length (5) } and not { have length (3) }}
        obj should (not have length (5) and not have length (3))
        obj should { not { have size (5) } and not { have size (3) }}
        obj should (not have size (5) and not have size (3))
      }

      def `should do nothing when object length or size doesn't match and used in a logical-or expression with not` {
        obj should { not { have length (2) } or not { have length (3) }}
        obj should (not have length (2) or not have length (3))
        obj should { not { have size (2) } or not { have size (3) }}
        obj should (not have size (2) or not have size (3))
      }

      def `should throw TestFailedException if object length or size does not match specified length` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (3)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3))
        val caught2 = intercept[TestFailedException] {
          obj should have size (3)
        }
        assert(caught2.getMessage === FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 3))
      }

      def `should throw TestFailedException with normal error message if specified length is negative` {
        val caught1 = intercept[TestFailedException] {
          obj should have length (-2)
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, -2))
        val caught2 = intercept[TestFailedException] {
          obj should have size (-2)
        }
        assert(caught2.getMessage === FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, -2))
      }

      def `should throw an assertion error when object length or size doesn't match and used in a logical-and expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (5) and (have length (2 - 1)) }
        }
        assert(caught1.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (5)) and (have length (2 - 1)))
        }
        assert(caught2.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (5) and have length (2 - 1))
        }
        assert(caught3.getMessage === FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 5))

        val caught1b = intercept[TestFailedException] {
          obj should { have size (5) and (have size (2 - 1)) }
        }
        assert(caught1b.getMessage === FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 5))

        val caughtb2 = intercept[TestFailedException] {
          obj should ((have size (5)) and (have size (2 - 1)))
        }
        assert(caughtb2.getMessage === FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 5))

        val caughtb3 = intercept[TestFailedException] {
          obj should (have size (5) and have size (2 - 1))
        }
        assert(caughtb3.getMessage === FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 5))
      }

      def `should throw an assertion error when object length or size doesn't match and used in a logical-or expression` {

        val caught1 = intercept[TestFailedException] {
          obj should { have length (55) or (have length (22)) }
        }
        assert(caught1.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught2 = intercept[TestFailedException] {
          obj should ((have length (55)) or (have length (22)))
        }
        assert(caught2.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught3 = intercept[TestFailedException] {
          obj should (have length (55) or have length (22))
        }
        assert(caught3.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 22))))

        val caught1b = intercept[TestFailedException] {
          obj should { have size (55) or (have size (22)) }
        }
        assert(caught1b.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 22))))

        val caught2b = intercept[TestFailedException] {
          obj should ((have size (55)) or (have size (22)))
        }
        assert(caught2b.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 22))))

        val caught3b = intercept[TestFailedException] {
          obj should (have size (55) or have size (22))
        }
        assert(caught3b.getMessage === FailureMessages("commaAnd", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 55)), UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 22))))
      }

      def `should throw an assertion error when object length or size matches and used in a logical-and expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (3) } and not { have length (2) }}
        }
        assert(caught1.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (3) } and { not have length (2) }}
        }
        assert(caught2.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (3) and not have length (2))
        }
        assert(caught3.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadLengthInsteadOfExpectedLength", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadLength", UnquotedString("lengthy"), 2))))

        val caught1b = intercept[TestFailedException] {
          obj should { not { have size (3) } and not { have size (2) }}
        }
        assert(caught1b.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadSize", UnquotedString("lengthy"), 2))))

        val caught2b = intercept[TestFailedException] {
          obj should { { not have size (3) } and { not have size (2) }}
        }
        assert(caught2b.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadSize", UnquotedString("lengthy"), 2))))

        val caught3b = intercept[TestFailedException] {
          obj should (not have size (3) and not have size (2))
        }
        assert(caught3b.getMessage === FailureMessages("commaBut", UnquotedString(FailureMessages("hadSizeInsteadOfExpectedSize", UnquotedString("lengthy"), 2, 3)), UnquotedString(FailureMessages("hadSize", UnquotedString("lengthy"), 2))))
      }

      def `should throw an assertion error when object length or size matches and used in a logical-or expression with not` {

        val caught1 = intercept[TestFailedException] {
          obj should { not { have length (2) } or not { have length (2) }}
        }
        assert(caught1.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught2 = intercept[TestFailedException] {
          obj should { { not have length (2) } or { not have length (2) }}
        }
        assert(caught2.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught3 = intercept[TestFailedException] {
          obj should (not have length (2) or not have length (2))
        }
        assert(caught3.getMessage === "lengthy had length 2, and lengthy had length 2")

        val caught1b = intercept[TestFailedException] {
          obj should { not { have size (2) } or not { have size (2) }}
        }
        assert(caught1b.getMessage === "lengthy had size 2, and lengthy had size 2")

        val caught2b = intercept[TestFailedException] {
          obj should { { not have size (2) } or { not have size (2) }}
        }
        assert(caught2b.getMessage === "lengthy had size 2, and lengthy had size 2")

        val caught3b = intercept[TestFailedException] {
          obj should (not have size (2) or not have size (2))
        }
        assert(caught3b.getMessage === "lengthy had size 2, and lengthy had size 2")
      }
    }

    def `should allow multiple implicits of the same type class (such as Length) to be resolve so long as the type param is not ambiguous` {
      import java.net.DatagramPacket
      val dp = new DatagramPacket(Array(0x0, 0x1, 0x2, 0x3), 4)
      dp.getLength
      implicit val lengthOfDatagramPacket =
        new Length[DatagramPacket] {
          def lengthOf(dp: DatagramPacket): Long = dp.getLength
        }
      dp should have length 4
      import java.awt.image.DataBufferByte
      val db = new DataBufferByte(4)
      implicit val sizeOfDataBufferByte =
        new Length[DataBufferByte] {
          def lengthOf(db: DataBufferByte): Long = db.getSize
        }
      db should have length 4
    }
  }
}
