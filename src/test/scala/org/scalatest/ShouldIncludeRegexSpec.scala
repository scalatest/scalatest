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

import org.scalatest.prop.Checkers
import org.scalacheck._
import Arbitrary._
import Prop._
import org.scalatest.exceptions.TestFailedException
import SharedHelpers._

class ShouldIncludeRegexSpec extends Spec with Matchers with Checkers with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should startWith substring t
s should startWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  object `The include regex syntax` {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    object `(when the regex is specified by a string)` {

      def `should do nothing if the string includes substring that matched regex specified as a string` {

        "1.78" should include regex ("1.7")
        "21.7" should include regex ("1.7")
        "21.78" should include regex ("1.7")
        "1.7" should include regex (decimal)
        "21.7" should include regex (decimal)
        "1.78" should include regex (decimal)
        "a -1.8 difference" should include regex (decimal)
        "b8" should include regex (decimal)
        "8x" should include regex (decimal)
        "1.x" should include regex (decimal)

        // The remaining are full matches, which should also work with "include"
        "1.7" should include regex ("1.7")
        "1.7" should include regex (decimal)
        "-1.8" should include regex (decimal)
        "8" should include regex (decimal)
        "1." should include regex (decimal)
      }
      
      def `should do nothing if the string includes substring that matched regex specified as a string and withGroup` {

        "abccde" should include regex("b(c*)d" withGroup "cc")
        "bccde" should include regex("b(c*)d" withGroup "cc")
        "abccd" should include regex("b(c*)d" withGroup "cc")
        // full matches, which should also work with "include"
        "bccd" should include regex("b(c*)d" withGroup "cc")
      }
      
      def `should do nothing if the string includes substring that matched regex specified as a string and withGroups` {

        "abccdde" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        "bccdde" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        "abccdd" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        // full matches, which should also work with "include"
        "bccdd" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used with not` {

        "eight" should not { include regex (decimal) }
        "one.eight" should not { include regex (decimal) }

        "eight" should not include regex (decimal)
        "one.eight" should not include regex (decimal)
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used with not` {

        "bccde" should not { include regex ("b(c*)d" withGroup "c") }
        "abccde" should not { include regex ("b(c*)d" withGroup "c") }

        "bccde" should not include regex ("b(c*)d" withGroup "c")
        "abccde" should not include regex ("b(c*)d" withGroup "c")
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used with not` {

        "bccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "d")) }
        "abccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "d")) }

        "bccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "d"))
        "abccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "d"))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression` {

        "a1.7" should (include regex (decimal) and (include regex (decimal)))
        "a1.7" should (include regex (decimal) and (include regex (decimal)))
        "a1.7" should (include regex (decimal) and (include regex (decimal)))

        "1.7b" should ((include regex (decimal)) and (include regex (decimal)))
        "1.7b" should ((include regex (decimal)) and (include regex (decimal)))
        "1.7b" should ((include regex (decimal)) and (include regex (decimal)))

        "a1.7b" should (include regex (decimal) and include regex (decimal))
        "a1.7b" should (include regex (decimal) and include regex (decimal))
        "a1.7b" should (include regex (decimal) and include regex (decimal))

        "1.7" should (include regex (decimal) and (include regex (decimal)))
        "1.7" should ((include regex (decimal)) and (include regex (decimal)))
        "1.7" should (include regex (decimal) and include regex (decimal))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression` {

        "abccd" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "cc")))

        "bccde" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "cc")))

        "abccde" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "cc"))

        "bccd" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "cc"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression` {

        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))

        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression` {

        "a1.7" should (include regex ("hello") or (include regex (decimal)))
        "a1.7" should (include regex ("hello") or (include regex (decimal)))
        "a1.7" should (include regex ("hello") or (include regex (decimal)))

        "1.7b" should ((include regex ("hello")) or (include regex (decimal)))
        "1.7b" should ((include regex ("hello")) or (include regex (decimal)))
        "a1.7b" should ((include regex ("hello")) or (include regex (decimal)))

        "a1.7b" should (include regex ("hello") or include regex (decimal))
        "a1.7b" should (include regex ("hello") or include regex (decimal))
        "a1.7b" should (include regex ("hello") or include regex (decimal))
  
        "1.7" should (include regex ("hello") or (include regex (decimal)))
        "1.7" should ((include regex ("hello")) or (include regex (decimal)))
        "1.7" should (include regex ("hello") or include regex (decimal))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression` {

        "abccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))

        "bccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))

        "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
  
        "bccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression` {

        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
  
        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not` {
        "fred" should (not (include regex ("bob")) and not (include regex (decimal)))
        "fred" should ((not include regex ("bob")) and (not include regex (decimal)))
        "fred" should (not include regex ("bob") and not include regex (decimal))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression with not` {
        "abccde" should (not (include regex ("b(c*)d" withGroup "c")) and not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "c"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression with not` {
        "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not` {
        "fred" should (not (include regex ("fred")) or not (include regex (decimal)))
        "fred" should ((not include regex ("fred")) or (not include regex (decimal)))
        "fred" should (not include regex ("fred") or not include regex (decimal))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression with not` {
        "abccde" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "c"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression with not` {
        "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should include regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should include regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should include regex (decimal)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should include regex (decimal)
        }
        assert(caught6.getMessage === "\"eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.eight" should include regex (decimal)
        }
        assert(caught7.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should include regex (decimal)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should include regex (decimal)
        }
        assert(caught9.getMessage === "\"***\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should include regex ("b(c*)d" withGroup "c")
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should include regex ("b(c*)(d*)" withGroups ("cc", "d"))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { include regex (decimal) }
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { include regex (decimal) }
        }
        assert(caught3.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { include regex (decimal) }
        }
        assert(caught4.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { include regex (decimal) }
        }
        assert(caught5.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not include regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not include regex (decimal)
        }
        assert(caught12.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not include regex (decimal)
        }
        assert(caught13.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not include regex (decimal)
        }
        assert(caught14.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not include regex (decimal)
        }
        assert(caught15.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" should not { include regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" should not { include regex (decimal) }
        }
        assert(caught22.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "a-1.8b" should not { include regex (decimal) }
        }
        assert(caught23.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should not { include regex ("b(c*)d" withGroup "cc") }
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should not include regex ("b(c*)d" withGroup "cc")
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "dd")) }
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (include regex (decimal) and (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((include regex (decimal)) and (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (include regex (decimal) and include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" should (include regex (decimal) and (include regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" should ((include regex (decimal)) and (include regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" should (include regex (decimal) and include regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "c")) and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") and include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" should (include regex (decimal) or (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" should ((include regex (decimal)) or (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" should (include regex (decimal) or include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not include regex ("1.8") and (not include regex (decimal)))
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not include regex ("1.8")) and (not include regex (decimal)))
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not include regex ("1.8") and not include regex (decimal))
        }
        assert(caught3.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" should (not include regex ("1.8") and (not include regex (decimal)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7b" should ((not include regex ("1.8")) and (not include regex (decimal)))
        }
        assert(caught5.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught6 = intercept[TestFailedException] {
          "a1.7b" should (not include regex ("1.8") and not include regex (decimal))
        }
        assert(caught6.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "c") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d" withGroup "c") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"abccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"bccde\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not include regex (decimal) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not include regex (decimal)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not include regex (decimal) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (include regex (decimal)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" should (not include regex (decimal) or (not include regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "1.7b" should ((not include regex (decimal)) or (not include regex ("1.7")))
        }
        assert(caught6.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "a1.7b" should (not include regex (decimal) or not include regex ("1.7"))
        }
        assert(caught7.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "a1.7b" should (not (include regex (decimal)) or not (include regex ("1.7")))
        }
        assert(caught8.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "cc") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccd" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d" withGroup "cc") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccd\" included substring that matched regex b(c*)d and group \"cc\", and \"abccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught6.getMessage === "\"bccde\" included substring that matched regex b(c*)d and group \"cc\", and \"bccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught7.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", and \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccde" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught8.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", and \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccdd" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught6.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught7.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    object `(when the regex is specified by an actual Regex)` {

      def `should do nothing if the string includes substring that matched regex specified as a string` {

        "1.78" should include regex ("1.7")
        "21.7" should include regex ("1.7")
        "21.78" should include regex ("1.7")
        "1.7" should include regex (decimalRegex)
        "21.7" should include regex (decimalRegex)
        "1.78" should include regex (decimalRegex)
        "a -1.8 difference" should include regex (decimalRegex)
        "b8" should include regex (decimalRegex)
        "8x" should include regex (decimalRegex)
        "1.x" should include regex (decimalRegex)

        // The remaining are full matches, which should also work with "include"
        "1.7" should include regex ("1.7")
        "1.7" should include regex (decimalRegex)
        "-1.8" should include regex (decimalRegex)
        "8" should include regex (decimalRegex)
        "1." should include regex (decimalRegex)
      }
      
      def `should do nothing if the string includes substring that matched regex specified as a string and withGroup` {

        "abccde" should include regex("b(c*)d".r withGroup "cc")
        "bccde" should include regex("b(c*)d".r withGroup "cc")
        "abccd" should include regex("b(c*)d".r withGroup "cc")
        // full matches, which should also work with "include"
        "bccd" should include regex("b(c*)d".r withGroup "cc")
      }
      
      def `should do nothing if the string includes substring that matched regex specified as a string and withGroups` {

        "abccdde" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        "bccdde" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        "abccdd" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        // full matches, which should also work with "include"
        "bccdd" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used with not` {

        "eight" should not { include regex (decimalRegex) }
        "one.eight" should not { include regex (decimalRegex) }

        "eight" should not include regex (decimalRegex)
        "one.eight" should not include regex (decimalRegex)
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used with not` {

        "bccde" should not { include regex ("b(c*)d".r withGroup "c") }
        "abccde" should not { include regex ("b(c*)d".r withGroup "c") }

        "bccde" should not include regex ("b(c*)d".r withGroup "c")
        "abccde" should not include regex ("b(c*)d".r withGroup "c")
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used with not` {

        "bccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "d")) }
        "abccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "d")) }

        "bccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
        "abccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression` {

        "a1.7" should (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7b" should (include regex (decimalRegex) and (include regex (decimalRegex)))
        "a1.7b" should (include regex (decimalRegex) and (include regex (decimalRegex)))

        "a1.7" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7b" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "a1.7b" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))

        "a1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
        "1.7b" should (include regex (decimalRegex) and include regex (decimalRegex))
        "a1.7b" should (include regex (decimalRegex) and include regex (decimalRegex))

        "1.7" should (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression` {

        "abccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))
        "abccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))
        "abccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))

        "bccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))

        "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))
        "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))
        "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))

        "bccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression` {

        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))

        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))

        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))

        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression` {

        "a1.7" should (include regex ("hello") or (include regex (decimalRegex)))
        "1.7b" should (include regex ("hello") or (include regex (decimalRegex)))
        "a1.7b" should (include regex ("hello") or (include regex (decimalRegex)))

        "a1.7" should ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7b" should ((include regex ("hello")) or (include regex (decimalRegex)))
        "a1.7b" should ((include regex ("hello")) or (include regex (decimalRegex)))

        "a1.7" should (include regex ("hello") or include regex (decimalRegex))
        "1.7b" should (include regex ("hello") or include regex (decimalRegex))
        "a1.7b" should (include regex ("hello") or include regex (decimalRegex))
  
        "1.7" should (include regex ("hello") or (include regex (decimalRegex)))
        "1.7" should ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7" should (include regex ("hello") or include regex (decimalRegex))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression` {

        "abccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))
        "abccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))
        "abccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))

        "bccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))

        "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
        "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
        "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
  
        "bccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression` {

        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))

        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))

        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
  
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not` {
        "fred" should (not (include regex ("bob")) and not (include regex (decimalRegex)))
        "fred" should ((not include regex ("bob")) and (not include regex (decimalRegex)))
        "fred" should (not include regex ("bob") and not include regex (decimalRegex))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression with not` {
        "abccde" should (not (include regex ("b(c*)d".r withGroup "c")) and not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "c"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression with not` {
        "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not` {
        "fred" should (not (include regex ("fred")) or not (include regex (decimalRegex)))
        "fred" should ((not include regex ("fred")) or (not include regex (decimalRegex)))
        "fred" should (not include regex ("fred") or not include regex (decimalRegex))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression with not` {
        "abccde" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "c"))
      }
      
      def `should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression with not` {
        "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
      }
  
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should include regex ("1.78")
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.78")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should include regex ("21.7")
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 21.7")
  
        val caught3 = intercept[TestFailedException] {
          "-one.eight" should include regex (decimalRegex)
        }
        assert(caught3.getMessage === "\"-one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "eight" should include regex (decimalRegex)
        }
        assert(caught6.getMessage === "\"eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught7 = intercept[TestFailedException] {
          "one.eight" should include regex (decimalRegex)
        }
        assert(caught7.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught8 = intercept[TestFailedException] {
          "onedoteight" should include regex (decimalRegex)
        }
        assert(caught8.getMessage === "\"onedoteight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught9 = intercept[TestFailedException] {
          "***" should include regex (decimalRegex)
        }
        assert(caught9.getMessage === "\"***\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should include regex ("b(c*)d".r withGroup "c")
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should not { include regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "-1.8" should not { include regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "8" should not { include regex (decimalRegex) }
        }
        assert(caught4.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "1." should not { include regex (decimalRegex) }
        }
        assert(caught5.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught11 = intercept[TestFailedException] {
          "1.7" should not include regex ("1.7")
        }
        assert(caught11.getMessage === "\"1.7\" included substring that matched regex 1.7")
  
        val caught12 = intercept[TestFailedException] {
          "1.7" should not include regex (decimalRegex)
        }
        assert(caught12.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught13 = intercept[TestFailedException] {
          "-1.8" should not include regex (decimalRegex)
        }
        assert(caught13.getMessage === "\"-1.8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught14 = intercept[TestFailedException] {
          "8" should not include regex (decimalRegex)
        }
        assert(caught14.getMessage === "\"8\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught15 = intercept[TestFailedException] {
          "1." should not include regex (decimalRegex)
        }
        assert(caught15.getMessage === "\"1.\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        // The rest are non-exact matches
        val caught21 = intercept[TestFailedException] {
          "a1.7" should not { include regex ("1.7") }
        }
        assert(caught21.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught22 = intercept[TestFailedException] {
          "1.7b" should not { include regex (decimalRegex) }
        }
        assert(caught22.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught23 = intercept[TestFailedException] {
          "a-1.8b" should not { include regex (decimalRegex) }
        }
        assert(caught23.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should not { include regex ("b(c*)d".r withGroup "cc") }
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should not include regex ("b(c*)d".r withGroup "cc")
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) }
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (include regex (decimalRegex) and (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((include regex (decimalRegex)) and (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (include regex (decimalRegex) and include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, but \"1.7\" did not include substring that matched regex 1.8")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "one.eight" should (include regex (decimalRegex) and (include regex ("1.8")))
        }
        assert(caught4.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught5 = intercept[TestFailedException] {
          "one.eight" should ((include regex (decimalRegex)) and (include regex ("1.8")))
        }
        assert(caught5.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught6 = intercept[TestFailedException] {
          "one.eight" should (include regex (decimalRegex) and include regex ("1.8"))
        }
        assert(caught6.getMessage === "\"one.eight\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", but \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "c")) and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") and include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", but \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "one.seven" should (include regex (decimalRegex) or (include regex ("1.8")))
        }
        assert(caught1.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught2 = intercept[TestFailedException] {
          "one.seven" should ((include regex (decimalRegex)) or (include regex ("1.8")))
        }
        assert(caught2.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
  
        val caught3 = intercept[TestFailedException] {
          "one.seven" should (include regex (decimalRegex) or include regex ("1.8"))
        }
        assert(caught3.getMessage === "\"one.seven\" did not include substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"one.seven\" did not include substring that matched regex 1.8")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", and \"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "1.7" should (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "1.7" should (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"1.7\" did not include substring that matched regex 1.8, but \"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7" should (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught4.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught5 = intercept[TestFailedException] {
          "1.7b" should ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught5.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught6 = intercept[TestFailedException] {
          "a1.7b" should (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught6.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "c") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d".r withGroup "c") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccd\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"abccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"bccde\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"bccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but did not match group \"c\", but \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but did not match group \"d\" at index 1, but \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "1.7" should (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7" should ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "1.7" should (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "1.7" should (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7\" included substring that matched regex 1.7")
  
        val caught5 = intercept[TestFailedException] {
          "a1.7" should (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught5.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
  
        val caught6 = intercept[TestFailedException] {
          "1.7b" should ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught6.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
  
        val caught7 = intercept[TestFailedException] {
          "a1.7b" should (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught7.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
  
        val caught8 = intercept[TestFailedException] {
          "a1.7b" should (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught8.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "cc") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccd" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group \"cc\", and \"bccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d".r withGroup "cc") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccd\" included substring that matched regex b(c*)d and group \"cc\", and \"abccd\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught6.getMessage === "\"bccde\" included substring that matched regex b(c*)d and group \"cc\", and \"bccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught7.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", and \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccde" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught8.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group \"cc\", and \"abccde\" included substring that matched regex b(c*)d and group \"cc\"")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccdd" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdd\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught6.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"bccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught7.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group \"cc\", \"dd\"")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}



/*
      def `should do nothing if the string includes substring that matched regex specified as a string` {
        "1.78" should include regex ("1.7")
        "21.7" should include regex ("1.7")
        "21.78" should include regex ("1.7")
        "1.7" should include regex (decimalRegex)
        "21.7" should include regex (decimalRegex)
        "1.78" should include regex (decimalRegex)
        "a -1.8 difference" should include regex (decimalRegex)
        "b8" should include regex (decimalRegex)
        "8x" should include regex (decimalRegex)
        "1.x" should include regex (decimalRegex)
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression` {

        "a1.7" should (include regex (decimalRegex) and (include regex (decimalRegex)))
        "1.7b" should (include regex (decimalRegex) and (include regex (decimalRegex)))
        "a1.7b" should (include regex (decimalRegex) and (include regex (decimalRegex)))

        "a1.7" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "1.7b" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))
        "a1.7b" should ((include regex (decimalRegex)) and (include regex (decimalRegex)))

        "a1.7" should (include regex (decimalRegex) and include regex (decimalRegex))
        "1.7b" should (include regex (decimalRegex) and include regex (decimalRegex))
        "a1.7b" should (include regex (decimalRegex) and include regex (decimalRegex))
      }
  
      def `should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression` {

        "a1.7" should (include regex ("hello") or (include regex (decimalRegex)))
        "1.7b" should (include regex ("hello") or (include regex (decimalRegex)))
        "a1.7b" should (include regex ("hello") or (include regex (decimalRegex)))

        "a1.7" should ((include regex ("hello")) or (include regex (decimalRegex)))
        "1.7b" should ((include regex ("hello")) or (include regex (decimalRegex)))
        "a1.7b" should ((include regex ("hello")) or (include regex (decimalRegex)))

        "a1.7" should (include regex ("hello") or include regex (decimalRegex))
        "1.7b" should (include regex ("hello") or include regex (decimalRegex))
        "a1.7b" should (include regex ("hello") or include regex (decimalRegex))
      }
  
      def `should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "a1.7" should not { include regex ("1.7") }
        }
        assert(caught1.getMessage === "\"a1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7b" should not { include regex (decimalRegex) }
        }
        assert(caught2.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
  
        val caught3 = intercept[TestFailedException] {
          "a-1.8b" should not { include regex (decimalRegex) }
        }
        assert(caught3.getMessage === "\"a-1.8b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "a1.7" should (not include regex ("1.8") and (not include regex (decimalRegex)))
        }
        assert(caught1.getMessage === "\"a1.7\" did not include substring that matched regex 1.8, but \"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught2 = intercept[TestFailedException] {
          "1.7b" should ((not include regex ("1.8")) and (not include regex (decimalRegex)))
        }
        assert(caught2.getMessage === "\"1.7b\" did not include substring that matched regex 1.8, but \"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")

        val caught3 = intercept[TestFailedException] {
          "a1.7b" should (not include regex ("1.8") and not include regex (decimalRegex))
        }
        assert(caught3.getMessage === "\"a1.7b\" did not include substring that matched regex 1.8, but \"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?")
      }
  
      def `should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "a1.7" should (not include regex (decimalRegex) or (not include regex ("1.7")))
        }
        assert(caught1.getMessage === "\"a1.7\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7\" included substring that matched regex 1.7")
  
        val caught2 = intercept[TestFailedException] {
          "1.7b" should ((not include regex (decimalRegex)) or (not include regex ("1.7")))
        }
        assert(caught2.getMessage === "\"1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"1.7b\" included substring that matched regex 1.7")
  
        val caught3 = intercept[TestFailedException] {
          "a1.7b" should (not include regex (decimalRegex) or not include regex ("1.7"))
        }
        assert(caught3.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
  
        val caught4 = intercept[TestFailedException] {
          "a1.7b" should (not (include regex (decimalRegex)) or not (include regex ("1.7")))
        }
        assert(caught4.getMessage === "\"a1.7b\" included substring that matched regex (-)?(\\d+)(\\.\\d*)?, and \"a1.7b\" included substring that matched regex 1.7")
      }
*/
