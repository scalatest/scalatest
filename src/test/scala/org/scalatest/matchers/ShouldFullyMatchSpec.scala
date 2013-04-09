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

class ShouldFullyMatchSpec extends Spec with ShouldMatchers with Checkers with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should startWith substring t
s should startWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  object `The fullyMatch regex syntax` {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    object `(when the regex is specified by a string)` {

      def `should do nothing if the string fully matches the regular expression specified as a string` {
        "1.7" should fullyMatch regex ("1.7")
        "1.7" should fullyMatch regex (decimal)
        "-1.8" should fullyMatch regex (decimal)
        "8" should fullyMatch regex (decimal)
        "1." should fullyMatch regex (decimal)
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used with not` {
  
        "eight" should not { fullyMatch regex (decimal) }
        "1.eight" should not { fullyMatch regex (decimal) }
        "one.8" should not { fullyMatch regex (decimal) }
  
        "eight" should not fullyMatch regex (decimal)
        "1.eight" should not fullyMatch regex (decimal)
        "one.8" should not fullyMatch regex (decimal)
        "1.8-" should not fullyMatch regex (decimal)
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression` {
        "1.7" should (fullyMatch regex (decimal) and (fullyMatch regex (decimal)))
        "1.7" should ((fullyMatch regex (decimal)) and (fullyMatch regex (decimal)))
        "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression` {
        "1.7" should (fullyMatch regex ("hello") or (fullyMatch regex (decimal)))
        "1.7" should ((fullyMatch regex ("hello")) or (fullyMatch regex (decimal)))
        "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (fullyMatch regex ("bob")) and not (fullyMatch regex (decimal)))
        "fred" should ((not fullyMatch regex ("bob")) and (not fullyMatch regex (decimal)))
        "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (fullyMatch regex ("fred")) or not (fullyMatch regex (decimal)))
        "fred" should ((not fullyMatch regex ("fred")) or (not fullyMatch regex (decimal)))
        "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
      }
  
      def `should throw TestFailedException if the string does not match the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should fullyMatch regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not fully match the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should fullyMatch regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not fully match the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-1.eight" should fullyMatch regex (decimal)
        }
        assert(caught3.getMessage === "\"-1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should fullyMatch regex (decimal)
        }
        assert(caught6.getMessage === "\"eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "1.eight" should fullyMatch regex (decimal)
        }
        assert(caught7.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "one.8" should fullyMatch regex (decimal)
        }
        assert(caught8.getMessage === "\"one.8\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "1.8-" should fullyMatch regex (decimal)
        }
        assert(caught9.getMessage === "\"1.8-\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { fullyMatch regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { fullyMatch regex (decimal) }
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { fullyMatch regex (decimal) }
        }
        assert(caught3.getMessage === "\"-1.8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { fullyMatch regex (decimal) }
        }
        assert(caught4.getMessage === "\"8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { fullyMatch regex (decimal) }
        }
        assert(caught5.getMessage === "\"1.\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not fullyMatch regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" fully matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not fullyMatch regex (decimal)
        }
        assert(caught12.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not fullyMatch regex (decimal)
        }
        assert(caught13.getMessage === "\"-1.8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not fullyMatch regex (decimal)
        }
        assert(caught14.getMessage === "\"8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not fullyMatch regex (decimal)
        }
        assert(caught15.getMessage === "\"1.\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (fullyMatch regex (decimal) and (fullyMatch regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((fullyMatch regex (decimal)) and (fullyMatch regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (fullyMatch regex (decimal) and fullyMatch regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "1.eight" should (fullyMatch regex (decimal) and (fullyMatch regex ("1.8")))
        }
        assert(caught4.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1.eight" should ((fullyMatch regex (decimal)) and (fullyMatch regex ("1.8")))
        }
        assert(caught5.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "1.eight" should (fullyMatch regex (decimal) and fullyMatch regex ("1.8"))
        }
        assert(caught6.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.seven" should (fullyMatch regex (decimal) or (fullyMatch regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.seven" should ((fullyMatch regex (decimal)) or (fullyMatch regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.seven" should (fullyMatch regex (decimal) or fullyMatch regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex ("1.8") and (not fullyMatch regex (decimal)))
        }
        assert(caught1.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not fullyMatch regex ("1.8")) and (not fullyMatch regex (decimal)))
        }
        assert(caught2.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex ("1.8") and not fullyMatch regex (decimal))
        }
        assert(caught3.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex (decimal) or (not fullyMatch regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not fullyMatch regex (decimal)) or (not fullyMatch regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex (decimal) or not fullyMatch regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (fullyMatch regex (decimal)) or not (fullyMatch regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
      }
    }

    object `(when the regex is specified by an actual Regex)` {

      def `should do nothing if the string fully matches the regular expression specified as a string` {
        "1.7" should fullyMatch regex ("1.7")
        "1.7" should fullyMatch regex (decimalRegex)
        "-1.8" should fullyMatch regex (decimalRegex)
        "8" should fullyMatch regex (decimalRegex)
        "1." should fullyMatch regex (decimalRegex)
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used with not` {
  
        "eight" should not { fullyMatch regex (decimalRegex) }
        "1.eight" should not { fullyMatch regex (decimalRegex) }
        "one.8" should not { fullyMatch regex (decimalRegex) }
  
        "eight" should not fullyMatch regex (decimalRegex)
        "1.eight" should not fullyMatch regex (decimalRegex)
        "one.8" should not fullyMatch regex (decimalRegex)
        "1.8-" should not fullyMatch regex (decimalRegex)
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression` {
        "1.7" should (fullyMatch regex (decimalRegex) and (fullyMatch regex (decimalRegex)))
        "1.7" should ((fullyMatch regex (decimalRegex)) and (fullyMatch regex (decimalRegex)))
        "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
      }

      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression` {
        "1.7" should (fullyMatch regex ("hello") or (fullyMatch regex (decimalRegex)))
        "1.7" should ((fullyMatch regex ("hello")) or (fullyMatch regex (decimalRegex)))
        "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimalRegex))
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (fullyMatch regex ("bob")) and not (fullyMatch regex (decimalRegex)))
        "fred" should ((not fullyMatch regex ("bob")) and (not fullyMatch regex (decimalRegex)))
        "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimalRegex))
      }
  
      def `should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (fullyMatch regex ("fred")) or not (fullyMatch regex (decimalRegex)))
        "fred" should ((not fullyMatch regex ("fred")) or (not fullyMatch regex (decimalRegex)))
        "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimalRegex))
      }
  
      def `should throw TestFailedException if the string does not match the regular expression specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should fullyMatch regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not fully match the regular expression 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should fullyMatch regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not fully match the regular expression 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-1.eight" should fullyMatch regex (decimalRegex)
        }
        assert(caught3.getMessage === "\"-1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should fullyMatch regex (decimalRegex)
        }
        assert(caught6.getMessage === "\"eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "1.eight" should fullyMatch regex (decimalRegex)
        }
        assert(caught7.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "one.8" should fullyMatch regex (decimalRegex)
        }
        assert(caught8.getMessage === "\"one.8\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "1.8-" should fullyMatch regex (decimalRegex)
        }
        assert(caught9.getMessage === "\"1.8-\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string does matches the regular expression specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { fullyMatch regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { fullyMatch regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { fullyMatch regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"-1.8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { fullyMatch regex (decimalRegex) }
        }
        assert(caught4.getMessage === "\"8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { fullyMatch regex (decimalRegex) }
        }
        assert(caught5.getMessage === "\"1.\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not fullyMatch regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" fully matched the regular expression 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not fullyMatch regex (decimalRegex)
        }
        assert(caught12.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not fullyMatch regex (decimalRegex)
        }
        assert(caught13.getMessage === "\"-1.8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not fullyMatch regex (decimalRegex)
        }
        assert(caught14.getMessage === "\"8\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not fullyMatch regex (decimalRegex)
        }
        assert(caught15.getMessage === "\"1.\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (fullyMatch regex (decimalRegex) and (fullyMatch regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((fullyMatch regex (decimalRegex)) and (fullyMatch regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not fully match the regular expression 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "1.eight" should (fullyMatch regex (decimalRegex) and (fullyMatch regex ("1.8")))
        }
        assert(caught4.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1.eight" should ((fullyMatch regex (decimalRegex)) and (fullyMatch regex ("1.8")))
        }
        assert(caught5.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "1.eight" should (fullyMatch regex (decimalRegex) and fullyMatch regex ("1.8"))
        }
        assert(caught6.getMessage === "\"1.eight\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.seven" should (fullyMatch regex (decimalRegex) or (fullyMatch regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.seven" should ((fullyMatch regex (decimalRegex)) or (fullyMatch regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.seven" should (fullyMatch regex (decimalRegex) or fullyMatch regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.seven\" did not fully match the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.seven\" did not fully match the regular expression 1.8")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex ("1.8") and (not fullyMatch regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not fullyMatch regex ("1.8")) and (not fullyMatch regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex ("1.8") and not fullyMatch regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"1.7\" did not fully match the regular expression 1.8, but \"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex (decimalRegex) or (not fullyMatch regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not fullyMatch regex (decimalRegex)) or (not fullyMatch regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not fullyMatch regex (decimalRegex) or not fullyMatch regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (fullyMatch regex (decimalRegex)) or not (fullyMatch regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" fully matched the regular expression (-)?(\\d+)(\\.\\d*)?, and \"1.7\" fully matched the regular expression 1.7")
      }
    }
  }
}
