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

class ShouldStartWithSubstringSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

  object `The startWith substring syntax` {

    def `should do nothing if the string starts with the specified substring` {

      "1.78" should startWith ("1.7")
      "21.7" should startWith ("2")
      "21.78" should startWith ("21.78")
      check((s: String, t: String) => returnsNormally(s + t should startWith (s)))
    }

    def `should do nothing if the string does not start with the specified substring when used with not` {

      "eight" should not { startWith ("1.7") }
      "eight" should not startWith ("1.7")
      check((s: String, t: String) => (s + t).indexOf(t) != 0 ==> returnsNormally(s + t should not (startWith (t))))
      check((s: String, t: String) => (s + t).indexOf(t) != 0 ==> returnsNormally(s + t should not startWith (t)))
    }

    def `should do nothing if the string does not start with the specified substring when used in a logical-and expression` {

      "1.7b" should (startWith ("1.7") and (startWith ("1.7b")))
      "1.7b" should ((startWith ("1.7")) and (startWith ("1.7b")))
      "1.7b" should (startWith ("1.7") and startWith ("1.7b"))

      check((s: String, t: String) => returnsNormally(s + t should (startWith (s) and startWith (s))))
    }

    def `should do nothing if the string does not start with the specified substring when used in a logical-or expression` {

      "1.7" should (startWith ("hello") or (startWith ("1")))
      "1.7" should ((startWith ("hello")) or (startWith ("1")))
      "1.7" should (startWith ("hello") or startWith ("1"))

      "1.7" should (startWith ("hello") or (startWith ("1.7")))
      "1.7" should ((startWith ("hello")) or (startWith ("1.7")))
      "1.7" should (startWith ("hello") or startWith ("1.7"))

      check((s: String, t: String) => returnsNormally(s + t should (startWith ("hi") or startWith (s))))
    }

    def `should do nothing if the string does not start with the specified substring when used in a logical-and expression with not` {
      "fred" should (not (startWith ("red")) and not (startWith ("1.7")))
      "fred" should ((not startWith ("red")) and (not startWith ("1.7")))
      "fred" should (not startWith ("red") and not startWith ("1.7"))
      check((s: String) => s.indexOf("bob") != 0 && s.indexOf("1.7") != 0 ==> returnsNormally(s should (not startWith ("bob") and not startWith ("1.7"))))
    }

    def `should do nothing if the string does not start with the specified substring when used in a logical-or expression with not` {
      "fred" should (not (startWith ("fred")) or not (startWith ("1.7")))
      "fred" should ((not startWith ("fred")) or (not startWith ("1.7")))
      "fred" should (not startWith ("fred") or not startWith ("1.7"))
      check((s: String) => s.indexOf("a") != 0 || s.indexOf("b") != 0 ==> returnsNormally(s should (not startWith ("a") or not startWith ("b"))))
    }

    def `should throw TestFailedException if the string does not match substring specified as a string` {

      val caught1 = intercept[TestFailedException] {
        "1.7" should startWith ("1.78")
      }
      assert(caught1.getMessage === "\"1.7\" did not start with substring \"1.78\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" should startWith ("21.7")
      }
      assert(caught2.getMessage === "\"1.7\" did not start with substring \"21.7\"")

      val caught3 = intercept[TestFailedException] {
        "-one.eight" should startWith ("1.7")
      }
      assert(caught3.getMessage === "\"-one.eight\" did not start with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "eight" should startWith ("1.7")
      }
      assert(caught6.getMessage === "\"eight\" did not start with substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "one.eight" should startWith ("1.7")
      }
      assert(caught7.getMessage === "\"one.eight\" did not start with substring \"1.7\"")

      val caught8 = intercept[TestFailedException] {
        "onedoteight" should startWith ("1.7")
      }
      assert(caught8.getMessage === "\"onedoteight\" did not start with substring \"1.7\"")

      val caught9 = intercept[TestFailedException] {
        "***" should startWith ("1.7")
      }
      assert(caught9.getMessage === "\"***\" did not start with substring \"1.7\"")

      check((s: String) => s.indexOf("1.7") == -1 ==> throwsTestFailedException(s should startWith ("1.7")))
    }

    def `should throw TestFailedException if the string does matches substring specified as a string when used with not` {

      val caught1 = intercept[TestFailedException] {
        "1.7" should not { startWith ("1.7") }
      }
      assert(caught1.getMessage === "\"1.7\" started with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" should not { startWith ("1.7") }
      }
      assert(caught2.getMessage === "\"1.7\" started with substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "-1.8" should not { startWith ("-1") }
      }
      assert(caught3.getMessage === "\"-1.8\" started with substring \"-1\"")

      val caught4 = intercept[TestFailedException] {
        "8" should not { startWith ("8") }
      }
      assert(caught4.getMessage === "\"8\" started with substring \"8\"")

      val caught5 = intercept[TestFailedException] {
        "1." should not { startWith ("1") }
      }
      assert(caught5.getMessage === "\"1.\" started with substring \"1\"")

      val caught11 = intercept[TestFailedException] {
        "1.7" should not startWith ("1.7")
      }
      assert(caught11.getMessage === "\"1.7\" started with substring \"1.7\"")

      val caught13 = intercept[TestFailedException] {
        "-1.8" should not startWith ("-")
      }
      assert(caught13.getMessage === "\"-1.8\" started with substring \"-\"")

      val caught14 = intercept[TestFailedException] {
        "8" should not startWith ("")
      }
      assert(caught14.getMessage === "\"8\" started with substring \"\"")

      val caught15 = intercept[TestFailedException] {
        "1." should not startWith ("1.")
      }
      assert(caught15.getMessage === "\"1.\" started with substring \"1.\"")

      val caught21 = intercept[TestFailedException] {
        "a1.7" should not { startWith ("a1") }
      }
      assert(caught21.getMessage === "\"a1.7\" started with substring \"a1\"")

      val caught22 = intercept[TestFailedException] {
        "1.7b" should not { startWith ("1.7") }
      }
      assert(caught22.getMessage === "\"1.7b\" started with substring \"1.7\"")

      val caught23 = intercept[TestFailedException] {
        "a-1.8b" should not { startWith ("a-1.8") }
      }
      assert(caught23.getMessage === "\"a-1.8b\" started with substring \"a-1.8\"")

      check((s: String) => s.length != 0 ==> throwsTestFailedException(s should not startWith (s.substring(0, 1))))
    }

    def `should throw TestFailedException if the string starts with the specified substring when used in a logical-and expression` {

      val caught1 = intercept[TestFailedException] {
        "1.7" should (startWith ("1.7") and (startWith ("1.8")))
      }
      assert(caught1.getMessage === "\"1.7\" started with substring \"1.7\", but \"1.7\" did not start with substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" should ((startWith ("1")) and (startWith ("1.8")))
      }
      assert(caught2.getMessage === "\"1.7\" started with substring \"1\", but \"1.7\" did not start with substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" should (startWith ("1.7") and startWith ("1.8"))
      }
      assert(caught3.getMessage === "\"1.7\" started with substring \"1.7\", but \"1.7\" did not start with substring \"1.8\"")

      // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
      val caught4 = intercept[TestFailedException] {
        "one.eight" should (startWith ("1.7") and (startWith ("1.8")))
      }
      assert(caught4.getMessage === "\"one.eight\" did not start with substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "one.eight" should ((startWith ("1.7")) and (startWith ("1.8")))
      }
      assert(caught5.getMessage === "\"one.eight\" did not start with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "one.eight" should (startWith ("1.7") and startWith ("1.8"))
      }
      assert(caught6.getMessage === "\"one.eight\" did not start with substring \"1.7\"")

      check((s: String, t: String, u: String) => (s + u).indexOf(t) != 0 ==> throwsTestFailedException(s + u should (startWith (s) and startWith (t))))
    }

    def `should throw TestFailedException if the string starts with the specified substring when used in a logical-or expression` {

      val caught1 = intercept[TestFailedException] {
        "one.seven" should (startWith ("1.7") or (startWith ("1.8")))
      }
      assert(caught1.getMessage === "\"one.seven\" did not start with substring \"1.7\", and \"one.seven\" did not start with substring \"1.8\"")

      val caught2 = intercept[TestFailedException] {
        "one.seven" should ((startWith ("1.7")) or (startWith ("1.8")))
      }
      assert(caught2.getMessage === "\"one.seven\" did not start with substring \"1.7\", and \"one.seven\" did not start with substring \"1.8\"")

      val caught3 = intercept[TestFailedException] {
        "one.seven" should (startWith ("1.7") or startWith ("1.8"))
      }
      assert(caught3.getMessage === "\"one.seven\" did not start with substring \"1.7\", and \"one.seven\" did not start with substring \"1.8\"")

      check(
        (s: String, t: String, u: String, v: String) => {
          (t.length != 0 && v.length != 0 && (s + u).indexOf(t) != 0 && (s + u).indexOf(v) != 0) ==>
            throwsTestFailedException(s + u should (startWith (t) or startWith (v)))
        }
      )
    }

    def `should throw TestFailedException if the string starts with the specified substring when used in a logical-and expression used with not` {

      val caught1 = intercept[TestFailedException] {
        "1.7" should (not startWith ("1.8") and (not startWith ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" did not start with substring \"1.8\", but \"1.7\" started with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" should ((not startWith ("1.8")) and (not startWith ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" did not start with substring \"1.8\", but \"1.7\" started with substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" should (not startWith ("1.8") and not startWith ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" did not start with substring \"1.8\", but \"1.7\" started with substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "a1.7" should (not startWith ("1.8") and (not startWith ("a1.7")))
      }
      assert(caught4.getMessage === "\"a1.7\" did not start with substring \"1.8\", but \"a1.7\" started with substring \"a1.7\"")

      val caught5 = intercept[TestFailedException] {
        "1.7b" should ((not startWith ("1.8")) and (not startWith ("1.7")))
      }
      assert(caught5.getMessage === "\"1.7b\" did not start with substring \"1.8\", but \"1.7b\" started with substring \"1.7\"")

      val caught6 = intercept[TestFailedException] {
        "a1.7b" should (not startWith ("1.8") and not startWith ("a1.7"))
      }
      assert(caught6.getMessage === "\"a1.7b\" did not start with substring \"1.8\", but \"a1.7b\" started with substring \"a1.7\"")

      check(
        (s: String, t: String, u: String) =>
          (s + t + u).indexOf("hi") != 0 ==>
            throwsTestFailedException(s + t + u should (not startWith ("hi") and not startWith (s)))
      )
    }

    def `should throw TestFailedException if the string starts with the specified substring when used in a logical-or expression used with not` {

      val caught1 = intercept[TestFailedException] {
        "1.7" should (not startWith ("1.7") or (not startWith ("1.7")))
      }
      assert(caught1.getMessage === "\"1.7\" started with substring \"1.7\", and \"1.7\" started with substring \"1.7\"")

      val caught2 = intercept[TestFailedException] {
        "1.7" should ((not startWith ("1.7")) or (not startWith ("1.7")))
      }
      assert(caught2.getMessage === "\"1.7\" started with substring \"1.7\", and \"1.7\" started with substring \"1.7\"")

      val caught3 = intercept[TestFailedException] {
        "1.7" should (not startWith ("1.7") or not startWith ("1.7"))
      }
      assert(caught3.getMessage === "\"1.7\" started with substring \"1.7\", and \"1.7\" started with substring \"1.7\"")

      val caught4 = intercept[TestFailedException] {
        "1.7" should (not (startWith ("1.7")) or not (startWith ("1.7")))
      }
      assert(caught4.getMessage === "\"1.7\" started with substring \"1.7\", and \"1.7\" started with substring \"1.7\"")

      val caught5 = intercept[TestFailedException] {
        "a1.7" should (not startWith ("a1.") or (not startWith ("a1.7")))
      }
      assert(caught5.getMessage === "\"a1.7\" started with substring \"a1.\", and \"a1.7\" started with substring \"a1.7\"")

      val caught6 = intercept[TestFailedException] {
        "1.7b" should ((not startWith ("1.7")) or (not startWith ("1.7")))
      }
      assert(caught6.getMessage === "\"1.7b\" started with substring \"1.7\", and \"1.7b\" started with substring \"1.7\"")

      val caught7 = intercept[TestFailedException] {
        "a1.7b" should (not startWith ("a1.7") or not startWith ("a1"))
      }
      assert(caught7.getMessage === "\"a1.7b\" started with substring \"a1.7\", and \"a1.7b\" started with substring \"a1\"")

      val caught8 = intercept[TestFailedException] {
        "a1.7b" should (not (startWith ("a1.7")) or not (startWith ("a1")))
      }
      assert(caught8.getMessage === "\"a1.7b\" started with substring \"a1.7\", and \"a1.7b\" started with substring \"a1\"")

      check(
        (s: String, t: String) =>
          throwsTestFailedException(s + t should (not startWith (s) or not startWith ("")))
      )
    }
  }
}

