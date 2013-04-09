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

class ShouldEndWithRegexSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should endWith substring t
s should endWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  object `The endWith regex syntax` {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    object `(when the regex is specified by a string)` {

      def `should do nothing if the string ends with substring that matched the regular expression specified as a string` {

        "1.78" should endWith regex (".78")
        "1.7" should endWith regex (decimal)
        "21.7" should endWith regex (decimal)
        "1.78" should endWith regex (decimal)
        "x8" should endWith regex (decimal)
        "x1." should endWith regex (decimal)

        // The remaining are full matches, which should also work with "endWith"
        "1.7" should endWith regex ("1.7")
        "1.7" should endWith regex (decimal)
        "-1.8" should endWith regex (decimal)
        "8" should endWith regex (decimal)
        "1." should endWith regex (decimal)
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { endWith regex (decimal) }
        "one.eight" should not { endWith regex (decimal) }

        "eight" should not endWith regex (decimal)
        "one.eight" should not endWith regex (decimal)
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "b1.7" should (endWith regex (decimal) and (endWith regex (decimal)))
        "b1.7" should ((endWith regex (decimal)) and (endWith regex (decimal)))
        "b1.7" should (endWith regex (decimal) and endWith regex (decimal))

        "1.7" should (endWith regex (decimal) and (endWith regex (decimal)))
        "1.7" should ((endWith regex (decimal)) and (endWith regex (decimal)))
        "1.7" should (endWith regex (decimal) and endWith regex (decimal))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "b1.7" should (endWith regex ("hello") or (endWith regex (decimal)))
        "b1.7" should ((endWith regex ("hello")) or (endWith regex (decimal)))
        "b1.7" should (endWith regex ("hello") or endWith regex (decimal))
  
        "1.7" should (endWith regex ("hello") or (endWith regex (decimal)))
        "1.7" should ((endWith regex ("hello")) or (endWith regex (decimal)))
        "1.7" should (endWith regex ("hello") or endWith regex (decimal))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (endWith regex ("bob")) and not (endWith regex (decimal)))
        "fred" should ((not endWith regex ("bob")) and (not endWith regex (decimal)))
        "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (endWith regex ("fred")) or not (endWith regex (decimal)))
        "fred" should ((not endWith regex ("fred")) or (not endWith regex (decimal)))
        "fred" should (not endWith regex ("fred") or not endWith regex (decimal))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should endWith regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should endWith regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should endWith regex (decimal)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should endWith regex (decimal)
        }
        assert(caught6.getMessage === "\"eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "1.eight" should endWith regex (decimal)
        }
        assert(caught7.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should endWith regex (decimal)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should endWith regex (decimal)
        }
        assert(caught9.getMessage === "\"***\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { endWith regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { endWith regex (decimal) }
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { endWith regex (decimal) }
        }
        assert(caught3.getMessage === "\"-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { endWith regex (decimal) }
        }
        assert(caught4.getMessage === "\"8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { endWith regex (decimal) }
        }
        assert(caught5.getMessage === "\"1.\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not endWith regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not endWith regex (decimal)
        }
        assert(caught12.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not endWith regex (decimal)
        }
        assert(caught13.getMessage === "\"-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not endWith regex (decimal)
        }
        assert(caught14.getMessage === "\"8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not endWith regex (decimal)
        }
        assert(caught15.getMessage === "\"1.\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" should not { endWith regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "b1.7" should not { endWith regex (decimal) }
        }
        assert(caught22.getMessage === "\"b1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "b-1.8" should not { endWith regex (decimal) }
        }
        assert(caught23.getMessage === "\"b-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (endWith regex (decimal) and (endWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((endWith regex (decimal)) and (endWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (endWith regex (decimal) and endWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "1.eight" should (endWith regex (decimal) and (endWith regex ("1.8")))
        }
        assert(caught4.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1.eight" should ((endWith regex (decimal)) and (endWith regex ("1.8")))
        }
        assert(caught5.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "1.eight" should (endWith regex (decimal) and endWith regex ("1.8"))
        }
        assert(caught6.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.seven" should (endWith regex (decimal) or (endWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.seven" should ((endWith regex (decimal)) or (endWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.seven" should (endWith regex (decimal) or endWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not endWith regex ("1.8") and (not endWith regex (decimal)))
        }
        assert(caught1.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex ("1.8")) and (not endWith regex (decimal)))
        }
        assert(caught2.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not endWith regex ("1.8") and not endWith regex (decimal))
        }
        assert(caught3.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" should (not endWith regex ("1.8") and (not endWith regex (decimal)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not end with a substring that matched the regular expression 1.8, but \"a1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex ("1.8")) and (not endWith regex (decimal)))
        }
        assert(caught5.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not endWith regex (decimal) or (not endWith regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex (decimal)) or (not endWith regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not endWith regex (decimal) or not endWith regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (endWith regex (decimal)) or not (endWith regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" should (not endWith regex (decimal) or (not endWith regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" ended with a substring that matched the regular expression 1.7")
      }
    }

    object `(when the regex is specified by an actual Regex)` {

      def `should do nothing if the string ends with substring that matched the regular expression specified as a string` {

        "1.78" should endWith regex (".78")
        "1.7" should endWith regex (decimalRegex)
        "21.7" should endWith regex (decimalRegex)
        "1.78" should endWith regex (decimalRegex)
        "x8" should endWith regex (decimalRegex)
        "x1." should endWith regex (decimalRegex)

        // The remaining are full matches, which should also work with "endWith"
        "1.7" should endWith regex ("1.7")
        "1.7" should endWith regex (decimalRegex)
        "-1.8" should endWith regex (decimalRegex)
        "8" should endWith regex (decimalRegex)
        "1." should endWith regex (decimalRegex)
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { endWith regex (decimalRegex) }
        "one.eight" should not { endWith regex (decimalRegex) }

        "eight" should not endWith regex (decimalRegex)
        "one.eight" should not endWith regex (decimalRegex)
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "b1.7" should (endWith regex (decimalRegex) and (endWith regex (decimalRegex)))
        "b1.7" should ((endWith regex (decimalRegex)) and (endWith regex (decimalRegex)))
        "b1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))

        "1.7" should (endWith regex (decimalRegex) and (endWith regex (decimalRegex)))
        "1.7" should ((endWith regex (decimalRegex)) and (endWith regex (decimalRegex)))
        "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "b1.7" should (endWith regex ("hello") or (endWith regex (decimalRegex)))
        "b1.7" should ((endWith regex ("hello")) or (endWith regex (decimalRegex)))
        "b1.7" should (endWith regex ("hello") or endWith regex (decimalRegex))
  
        "1.7" should (endWith regex ("hello") or (endWith regex (decimalRegex)))
        "1.7" should ((endWith regex ("hello")) or (endWith regex (decimalRegex)))
        "1.7" should (endWith regex ("hello") or endWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (endWith regex ("bob")) and not (endWith regex (decimalRegex)))
        "fred" should ((not endWith regex ("bob")) and (not endWith regex (decimalRegex)))
        "fred" should (not endWith regex ("bob") and not endWith regex (decimalRegex))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (endWith regex ("fred")) or not (endWith regex (decimalRegex)))
        "fred" should ((not endWith regex ("fred")) or (not endWith regex (decimalRegex)))
        "fred" should (not endWith regex ("fred") or not endWith regex (decimalRegex))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should endWith regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should endWith regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should endWith regex (decimalRegex)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should endWith regex (decimalRegex)
        }
        assert(caught6.getMessage === "\"eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "1.eight" should endWith regex (decimalRegex)
        }
        assert(caught7.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should endWith regex (decimalRegex)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should endWith regex (decimalRegex)
        }
        assert(caught9.getMessage === "\"***\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { endWith regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { endWith regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { endWith regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { endWith regex (decimalRegex) }
        }
        assert(caught4.getMessage === "\"8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { endWith regex (decimalRegex) }
        }
        assert(caught5.getMessage === "\"1.\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not endWith regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not endWith regex (decimalRegex)
        }
        assert(caught12.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not endWith regex (decimalRegex)
        }
        assert(caught13.getMessage === "\"-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not endWith regex (decimalRegex)
        }
        assert(caught14.getMessage === "\"8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not endWith regex (decimalRegex)
        }
        assert(caught15.getMessage === "\"1.\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" should not { endWith regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "b1.7" should not { endWith regex (decimalRegex) }
        }
        assert(caught22.getMessage === "\"b1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "b-1.8" should not { endWith regex (decimalRegex) }
        }
        assert(caught23.getMessage === "\"b-1.8\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (endWith regex (decimalRegex) and (endWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((endWith regex (decimalRegex)) and (endWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (endWith regex (decimalRegex) and endWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not end with a substring that matched the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "1.eight" should (endWith regex (decimalRegex) and (endWith regex ("1.8")))
        }
        assert(caught4.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1.eight" should ((endWith regex (decimalRegex)) and (endWith regex ("1.8")))
        }
        assert(caught5.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "1.eight" should (endWith regex (decimalRegex) and endWith regex ("1.8"))
        }
        assert(caught6.getMessage === "\"1.eight\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.seven" should (endWith regex (decimalRegex) or (endWith regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.seven" should ((endWith regex (decimalRegex)) or (endWith regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.seven" should (endWith regex (decimalRegex) or endWith regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.seven\" did not end with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not end with a substring that matched the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not endWith regex ("1.8") and (not endWith regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex ("1.8")) and (not endWith regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not endWith regex ("1.8") and not endWith regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" should (not endWith regex ("1.8") and (not endWith regex (decimalRegex)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not end with a substring that matched the regular expression 1.8, but \"a1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex ("1.8")) and (not endWith regex (decimalRegex)))
        }
        assert(caught5.getMessage === "\"1.7\" did not end with a substring that matched the regular expression 1.8, but \"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }

      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not endWith regex (decimalRegex) or (not endWith regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not endWith regex (decimalRegex)) or (not endWith regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not endWith regex (decimalRegex) or not endWith regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (endWith regex (decimalRegex)) or not (endWith regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" ended with a substring that matched the regular expression 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" should (not endWith regex (decimalRegex) or (not endWith regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" ended with a substring that matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" ended with a substring that matched the regular expression 1.7")
      }
    }
  }
}

