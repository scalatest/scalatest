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

class ShouldStartWithRegexSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should startWith substring t
s should startWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  object `The startWith regex syntax` {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    object `(when the regex is specified by a string)` {

      def `should do nothing if the string starts with substring that matched the regular expression specified as a string` {

        "1.78" should startWith regex ("1.7")
        "1.7" should startWith regex (decimal)
        "21.7" should startWith regex (decimal)
        "1.78" should startWith regex (decimal)
        "8x" should startWith regex (decimal)
        "1.x" should startWith regex (decimal)

        // The remaining are full matches, which should also work with "startWith"
        "1.7" should startWith regex ("1.7")
        "1.7" should startWith regex (decimal)
        "-1.8" should startWith regex (decimal)
        "8" should startWith regex (decimal)
        "1." should startWith regex (decimal)
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { startWith regex (decimal) }
        "one.eight" should not { startWith regex (decimal) }

        "eight" should not startWith regex (decimal)
        "one.eight" should not startWith regex (decimal)
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "1.7b" should (startWith regex (decimal) and (startWith regex (decimal)))
        "1.7b" should ((startWith regex (decimal)) and (startWith regex (decimal)))
        "1.7b" should (startWith regex (decimal) and startWith regex (decimal))

        "1.7" should (startWith regex (decimal) and (startWith regex (decimal)))
        "1.7" should ((startWith regex (decimal)) and (startWith regex (decimal)))
        "1.7" should (startWith regex (decimal) and startWith regex (decimal))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "1.7b" should (startWith regex ("hello") or (startWith regex (decimal)))
        "1.7b" should ((startWith regex ("hello")) or (startWith regex (decimal)))
        "1.7b" should (startWith regex ("hello") or startWith regex (decimal))
  
        "1.7" should (startWith regex ("hello") or (startWith regex (decimal)))
        "1.7" should ((startWith regex ("hello")) or (startWith regex (decimal)))
        "1.7" should (startWith regex ("hello") or startWith regex (decimal))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (startWith regex ("bob")) and not (startWith regex (decimal)))
        "fred" should ((not startWith regex ("bob")) and (not startWith regex (decimal)))
        "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (startWith regex ("fred")) or not (startWith regex (decimal)))
        "fred" should ((not startWith regex ("fred")) or (not startWith regex (decimal)))
        "fred" should (not startWith regex ("fred") or not startWith regex (decimal))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should startWith regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should startWith regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should startWith regex (decimal)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should startWith regex (decimal)
        }
        assert(caught6.getMessage === "\"eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.8" should startWith regex (decimal)
        }
        assert(caught7.getMessage === "\"one.8\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should startWith regex (decimal)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should startWith regex (decimal)
        }
        assert(caught9.getMessage === "\"***\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { startWith regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { startWith regex (decimal) }
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { startWith regex (decimal) }
        }
        assert(caught3.getMessage === "\"-1.8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { startWith regex (decimal) }
        }
        assert(caught4.getMessage === "\"8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { startWith regex (decimal) }
        }
        assert(caught5.getMessage === "\"1.\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not startWith regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not startWith regex (decimal)
        }
        assert(caught12.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not startWith regex (decimal)
        }
        assert(caught13.getMessage === "\"-1.8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not startWith regex (decimal)
        }
        assert(caught14.getMessage === "\"8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not startWith regex (decimal)
        }
        assert(caught15.getMessage === "\"1.\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "1.7a" should not { startWith regex ("1.7") }
        }
        assert(caught21.getMessage === "\"1.7a\" started with a substring that matched the regular expression 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" should not { startWith regex (decimal) }
        }
        assert(caught22.getMessage === "\"1.7b\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "-1.8b" should not { startWith regex (decimal) }
        }
        assert(caught23.getMessage === "\"-1.8b\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (startWith regex (decimal) and (startWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((startWith regex (decimal)) and (startWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (startWith regex (decimal) and startWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" should (startWith regex (decimal) and (startWith regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" should ((startWith regex (decimal)) and (startWith regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" should (startWith regex (decimal) and startWith regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" should (startWith regex (decimal) or (startWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" should ((startWith regex (decimal)) or (startWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" should (startWith regex (decimal) or startWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not startWith regex ("1.8") and (not startWith regex (decimal)))
        }
        assert(caught1.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex ("1.8")) and (not startWith regex (decimal)))
        }
        assert(caught2.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not startWith regex ("1.8") and not startWith regex (decimal))
        }
        assert(caught3.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "1.7a" should (not startWith regex ("1.8") and (not startWith regex (decimal)))
        }
        assert(caught4.getMessage === "\"1.7a\" did not start with a substring that matched the regular expression 1.8, but \"1.7a\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex ("1.8")) and (not startWith regex (decimal)))
        }
        assert(caught5.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not startWith regex (decimal) or (not startWith regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex (decimal)) or (not startWith regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not startWith regex (decimal) or not startWith regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (startWith regex (decimal)) or not (startWith regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "1.7a" should (not startWith regex (decimal) or (not startWith regex ("1.7")))
        }
        assert(caught5.getMessage === "\"1.7a\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7a\" started with a substring that matched the regular expression 1.7")
      }
    }

    object `(when the regex is specified by an actual Regex)` {

      def `should do nothing if the string starts with substring that matched the regular expression specified as a string` {

        "1.7" should startWith regex (decimalRegex)
        "21.7" should startWith regex (decimalRegex)
        "1.78" should startWith regex (decimalRegex)
        "8x" should startWith regex (decimalRegex)
        "1.x" should startWith regex (decimalRegex)

        // The remaining are full matches, which should also work with "startWith"
        "1.7" should startWith regex (decimalRegex)
        "-1.8" should startWith regex (decimalRegex)
        "8" should startWith regex (decimalRegex)
        "1." should startWith regex (decimalRegex)
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { startWith regex (decimalRegex) }
        "one.eight" should not { startWith regex (decimalRegex) }

        "eight" should not startWith regex (decimalRegex)
        "one.eight" should not startWith regex (decimalRegex)
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "1.7b" should (startWith regex (decimalRegex) and (startWith regex (decimalRegex)))
        "1.7b" should ((startWith regex (decimalRegex)) and (startWith regex (decimalRegex)))
        "1.7b" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))

        "1.7" should (startWith regex (decimalRegex) and (startWith regex (decimalRegex)))
        "1.7" should ((startWith regex (decimalRegex)) and (startWith regex (decimalRegex)))
        "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "1.7b" should (startWith regex ("hello") or (startWith regex (decimalRegex)))
        "1.7b" should ((startWith regex ("hello")) or (startWith regex (decimalRegex)))
        "1.7b" should (startWith regex ("hello") or startWith regex (decimalRegex))
  
        "1.7" should (startWith regex ("hello") or (startWith regex (decimalRegex)))
        "1.7" should ((startWith regex ("hello")) or (startWith regex (decimalRegex)))
        "1.7" should (startWith regex ("hello") or startWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (startWith regex ("bob")) and not (startWith regex (decimalRegex)))
        "fred" should ((not startWith regex ("bob")) and (not startWith regex (decimalRegex)))
        "fred" should (not startWith regex ("bob") and not startWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (startWith regex ("fred")) or not (startWith regex (decimalRegex)))
        "fred" should ((not startWith regex ("fred")) or (not startWith regex (decimalRegex)))
        "fred" should (not startWith regex ("fred") or not startWith regex (decimalRegex))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should startWith regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should startWith regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should startWith regex (decimalRegex)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should startWith regex (decimalRegex)
        }
        assert(caught6.getMessage === "\"eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.8" should startWith regex (decimalRegex)
        }
        assert(caught7.getMessage === "\"one.8\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should startWith regex (decimalRegex)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should startWith regex (decimalRegex)
        }
        assert(caught9.getMessage === "\"***\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { startWith regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { startWith regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { startWith regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"-1.8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { startWith regex (decimalRegex) }
        }
        assert(caught4.getMessage === "\"8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { startWith regex (decimalRegex) }
        }
        assert(caught5.getMessage === "\"1.\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not startWith regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not startWith regex (decimalRegex)
        }
        assert(caught12.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not startWith regex (decimalRegex)
        }
        assert(caught13.getMessage === "\"-1.8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not startWith regex (decimalRegex)
        }
        assert(caught14.getMessage === "\"8\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not startWith regex (decimalRegex)
        }
        assert(caught15.getMessage === "\"1.\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "1.7a" should not { startWith regex ("1.7") }
        }
        assert(caught21.getMessage === "\"1.7a\" started with a substring that matched the regular expression 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" should not { startWith regex (decimalRegex) }
        }
        assert(caught22.getMessage === "\"1.7b\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "-1.8b" should not { startWith regex (decimalRegex) }
        }
        assert(caught23.getMessage === "\"-1.8b\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (startWith regex (decimalRegex) and (startWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((startWith regex (decimalRegex)) and (startWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (startWith regex (decimalRegex) and startWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not start with a substring that matched the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" should (startWith regex (decimalRegex) and (startWith regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" should ((startWith regex (decimalRegex)) and (startWith regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" should (startWith regex (decimalRegex) and startWith regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" should (startWith regex (decimalRegex) or (startWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" should ((startWith regex (decimalRegex)) or (startWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" should (startWith regex (decimalRegex) or startWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not start with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not start with a substring that matched the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not startWith regex ("1.8") and (not startWith regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex ("1.8")) and (not startWith regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not startWith regex ("1.8") and not startWith regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "1.7a" should (not startWith regex ("1.8") and (not startWith regex (decimalRegex)))
        }
        assert(caught4.getMessage === "\"1.7a\" did not start with a substring that matched the regular expression 1.8, but \"1.7a\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex ("1.8")) and (not startWith regex (decimalRegex)))
        }
        assert(caught5.getMessage === "\"1.7\" did not start with a substring that matched the regular expression 1.8, but \"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not startWith regex (decimalRegex) or (not startWith regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not startWith regex (decimalRegex)) or (not startWith regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not startWith regex (decimalRegex) or not startWith regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (startWith regex (decimalRegex)) or not (startWith regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" started with a substring that matched the regular expression 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "1.7a" should (not startWith regex (decimalRegex) or (not startWith regex ("1.7")))
        }
        assert(caught5.getMessage === "\"1.7a\" started with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7a\" started with a substring that matched the regular expression 1.7")
      }
    }
  }
}

