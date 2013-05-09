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

class ShouldStartWithRegexSpec extends Spec with Matchers with Checkers with ReturnsNormallyThrowsAssertion with SharedHelpers {

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
      
      def `should do nothing if the string starts with substring that matched the regular expression specified as a string and withGroup` {

        "abbc" should startWith regex ("a(b*)c" withGroup "bb")
        "aabbc" should startWith regex ("aa(b*)c" withGroup "bb")
        "abbcc" should startWith regex ("a(b*)cc" withGroup "bb")
        
      }
      
      def `should do nothing if the string starts with substring that matched the regular expression specified as a string and withGroups` {

        "abbcc" should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
        "aabbcc" should startWith regex ("aa(b*)(c*)" withGroups ("bb", "cc"))
        "abbccd" should startWith regex ("a(b*)(c*)d" withGroups ("bb", "cc"))
        
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { startWith regex (decimal) }
        "one.eight" should not { startWith regex (decimal) }

        "eight" should not startWith regex (decimal)
        "one.eight" should not startWith regex (decimal)
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used with not` {

        "abcdef" should not { startWith regex ("a(b*)c" withGroup "bb") }
        "abcdef" should not startWith regex ("a(b*)c" withGroup "bb")
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used with not` {

        "abbcdef" should not { startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) }
        "abbcdef" should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "1.7b" should (startWith regex (decimal) and (startWith regex (decimal)))
        "1.7b" should ((startWith regex (decimal)) and (startWith regex (decimal)))
        "1.7b" should (startWith regex (decimal) and startWith regex (decimal))

        "1.7" should (startWith regex (decimal) and (startWith regex (decimal)))
        "1.7" should ((startWith regex (decimal)) and (startWith regex (decimal)))
        "1.7" should (startWith regex (decimal) and startWith regex (decimal))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {

        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))

        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {

        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))

        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "1.7b" should (startWith regex ("hello") or (startWith regex (decimal)))
        "1.7b" should ((startWith regex ("hello")) or (startWith regex (decimal)))
        "1.7b" should (startWith regex ("hello") or startWith regex (decimal))
  
        "1.7" should (startWith regex ("hello") or (startWith regex (decimal)))
        "1.7" should ((startWith regex ("hello")) or (startWith regex (decimal)))
        "1.7" should (startWith regex ("hello") or startWith regex (decimal))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {

        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bbb")) or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
  
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bbb")) or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {

        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
  
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (startWith regex ("bob")) and not (startWith regex (decimal)))
        "fred" should ((not startWith regex ("bob")) and (not startWith regex (decimal)))
        "fred" should (not startWith regex ("bob") and not startWith regex (decimal))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression with not` {
        "abbbcdef" should (not (startWith regex ("a(b*)c" withGroup "b")) and not (startWith regex ("a(b*)c" withGroup "bb")))
        "abbbcdef" should ((not startWith regex ("a(b*)c" withGroup "b")) and (not startWith regex ("a(b*)c" withGroup "bb")))
        "abbbcdef" should (not startWith regex ("a(b*)c" withGroup "b") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression with not` {
        "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bb", "c"))) and not (startWith regex ("a(b*)(c*)" withGroups ("bb", "c"))))
        "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "c"))) and (not startWith regex ("a(b*)(c*)" withGroups ("bb", "c"))))
        "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "c")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "c")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (startWith regex ("fred")) or not (startWith regex (decimal)))
        "fred" should ((not startWith regex ("fred")) or (not startWith regex (decimal)))
        "fred" should (not startWith regex ("fred") or not startWith regex (decimal))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression with not` {
        "abbcdef" should (not (startWith regex ("a(b*)c" withGroup "bb")) or not (startWith regex ("a(b*)c" withGroup "bbb")))
        "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bb")) or (not startWith regex ("a(b*)c" withGroup "bbb")))
        "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bbb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression with not` {
        "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or not (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
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
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should startWith regex ("a(b*)c" withGroup "b")
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"b\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should not { startWith regex ("a(b*)c" withGroup "bb") }
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should not startWith regex ("a(b*)c" withGroup "bb")
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should not { startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) }
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bbb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") and (startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught4.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bbb")) and (startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught5.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught5.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") and startWith regex ("a(b*)c" withGroup "bb"))
        }
        assert(caught6.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught6.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught5.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught6.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "b") or (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"b\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((startWith regex ("a(b*)c" withGroup "b")) or (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"b\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "b") or startWith regex ("a(b*)c" withGroup "bbb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"b\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "c")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"c\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "c"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"c\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "c")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"c\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bbb") and (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bbb")) and (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bbb") and not startWith regex ("a(b*)c" withGroup "bb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bb")) or (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbcdef" should (not (startWith regex ("a(b*)c" withGroup "bb")) or not (startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught4.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should do nothing if the string starts with substring that matched the regular expression specified as a string withGroup` {

        "abbcdef" should startWith regex ("a(b*)c".r withGroup "bb")
        // full matches, which should also work with "startWith"
        "abbc" should startWith regex ("a(b*)c".r withGroup "bb")
      }
      
      def `should do nothing if the string starts with substring that matched the regular expression specified as a string withGroups` {

        "abbccdef" should startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
        // full matches, which should also work with "startWith"
        "abbcc" should startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { startWith regex (decimalRegex) }
        "one.eight" should not { startWith regex (decimalRegex) }

        "eight" should not startWith regex (decimalRegex)
        "one.eight" should not startWith regex (decimalRegex)
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string withGroup when used with not` {

        "abbcdef" should not { startWith regex ("a(b*)c".r withGroup "bbb") }
        "abbcdef" should not startWith regex ("a(b*)c".r withGroup "bbb")
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string withGroups when used with not` {

        "abbccdef" should not { startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) }
        "abbccdef" should not startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "1.7b" should (startWith regex (decimalRegex) and (startWith regex (decimalRegex)))
        "1.7b" should ((startWith regex (decimalRegex)) and (startWith regex (decimalRegex)))
        "1.7b" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))

        "1.7" should (startWith regex (decimalRegex) and (startWith regex (decimalRegex)))
        "1.7" should ((startWith regex (decimalRegex)) and (startWith regex (decimalRegex)))
        "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string withGroup when used in a logical-and expression` {

        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))

        "abbc" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbc" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bb")))
        "abbc" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string withGroups when used in a logical-and expression` {

        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))

        "abbcc" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbcc" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbcc" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "1.7b" should (startWith regex ("hello") or (startWith regex (decimalRegex)))
        "1.7b" should ((startWith regex ("hello")) or (startWith regex (decimalRegex)))
        "1.7b" should (startWith regex ("hello") or startWith regex (decimalRegex))
  
        "1.7" should (startWith regex ("hello") or (startWith regex (decimalRegex)))
        "1.7" should ((startWith regex ("hello")) or (startWith regex (decimalRegex)))
        "1.7" should (startWith regex ("hello") or startWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {

        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bbb")) or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
  
        "abbc" should (startWith regex ("a(b*)c" withGroup "bbb") or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbc" should ((startWith regex ("a(b*)c" withGroup "bbb")) or (startWith regex ("a(b*)c" withGroup "bb")))
        "abbc" should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {

        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
  
        "abbcc" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbcc" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        "abbcc" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (startWith regex ("bob")) and not (startWith regex (decimalRegex)))
        "fred" should ((not startWith regex ("bob")) and (not startWith regex (decimalRegex)))
        "fred" should (not startWith regex ("bob") and not startWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression with not` {
        "abbcdef" should (not (startWith regex ("a(b*)c" withGroup "bob")) and not (startWith regex ("a(b*)c" withGroup "bob")))
        "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bob")) and (not startWith regex ("a(b*)c" withGroup "bob")))
        "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bob") and not startWith regex ("a(b*)c" withGroup "bob"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression with not` {
        "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bob", "cat"))) and not (startWith regex ("a(b*)(c*)" withGroups ("bob", "cat"))))
        "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bob", "cat"))) and (not startWith regex ("a(b*)(c*)" withGroups ("bob", "cat"))))
        "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bob", "cat")) and not startWith regex ("a(b*)(c*)" withGroups ("bob", "cat")))
      }
  
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (startWith regex ("fred")) or not (startWith regex (decimalRegex)))
        "fred" should ((not startWith regex ("fred")) or (not startWith regex (decimalRegex)))
        "fred" should (not startWith regex ("fred") or not startWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression with not` {
        "abbcdef" should (not (startWith regex ("a(b*)c" withGroup "bb")) or not (startWith regex ("a(b*)c" withGroup "bbb")))
        "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bb")) or (not startWith regex ("a(b*)c" withGroup "bbb")))
        "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bbb"))
      }
      
      def `should do nothing if the string does not start with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression with not` {
        "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or not (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
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
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should startWith regex ("a(b*)c" withGroup "bbb")
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should startWith regex ("a(b*)c".r withGroup "bbb")
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should not { startWith regex ("a(b*)c" withGroup "bb") }
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should not { startWith regex ("a(b*)c".r withGroup "bb") }
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should not startWith regex ("a(b*)c" withGroup "bb")
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbcdef" should not startWith regex ("a(b*)c".r withGroup "bb")
        }
        assert(caught4.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should not { startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) }
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should not { startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")) }
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bb")) and (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bbb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught5.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught6.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
  
        val caught5 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught5.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
  
        val caught6 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught6.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((startWith regex ("a(b*)c" withGroup "bbb")) or (startWith regex ("a(b*)c" withGroup "bbb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bbb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bbb") and (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bbb")) and (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bbb") and not startWith regex ("a(b*)c" withGroup "bb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but did not match group \"bbb\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but did not match group \"ccc\" at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught1.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbcdef" should ((not startWith regex ("a(b*)c" withGroup "bb")) or (not startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught2.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbcdef" should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bb"))
        }
        assert(caught3.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbcdef" should (not (startWith regex ("a(b*)c" withGroup "bb")) or not (startWith regex ("a(b*)c" withGroup "bb")))
        }
        assert(caught4.getMessage === "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group \"bb\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string starts with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught1.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught1.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abbccdef" should ((not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught2.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught2.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abbccdef" should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
        assert(caught3.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught3.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abbccdef" should (not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))) or not (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
        }
        assert(caught4.getMessage === "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"")
        assert(caught4.failedCodeFileName === Some("ShouldStartWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}

