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
import Matchers._

class ShouldEndWithRegexSpec extends Spec with Checkers with ReturnsNormallyThrowsAssertion {

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
      
      def `should do nothing if the string ends with substring that matched the regular expression specified as a string and withGroup` {
        "abcdeef" should endWith regex ("d(e*)f" withGroup "ee")
        // full matches, which should also work with "endWith"
        "deef" should endWith regex ("d(e*)f" withGroup "ee")
      }
      
      def `should do nothing if the string ends with substring that matched the regular expression specified as a string and withGroups` {
        "abcdeeff" should endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))
        // full matches, which should also work with "endWith"
        "deeff" should endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { endWith regex (decimal) }
        "one.eight" should not { endWith regex (decimal) }

        "eight" should not endWith regex (decimal)
        "one.eight" should not endWith regex (decimal)
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used with not` {

        "abc" should not { endWith regex ("d(e*)f" withGroup "ee") }
        "abcdeeef" should not { endWith regex ("d(e*)f" withGroup "ee") }

        "abc" should not endWith regex ("d(e*)f" withGroup "ee")
        "abcdeeef" should not endWith regex ("d(e*)f" withGroup "ee")
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used with not` {

        "abc" should not { endWith regex ("d(e*)f" withGroups ("ee", "fff")) }
        "abcdeeff" should not { endWith regex ("d(e*)f" withGroups ("ee", "fff")) }

        "abc" should not endWith regex ("d(e*)f" withGroups ("ee", "fff"))
        "abcdeeff" should not endWith regex ("d(e*)f" withGroups ("ee", "fff"))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "b1.7" should (endWith regex (decimal) and (endWith regex (decimal)))
        "b1.7" should ((endWith regex (decimal)) and (endWith regex (decimal)))
        "b1.7" should (endWith regex (decimal) and endWith regex (decimal))

        "1.7" should (endWith regex (decimal) and (endWith regex (decimal)))
        "1.7" should ((endWith regex (decimal)) and (endWith regex (decimal)))
        "1.7" should (endWith regex (decimal) and endWith regex (decimal))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {

        "abcdeef" should (endWith regex ("d(e*)f" withGroup "ee") and (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should ((endWith regex ("d(e*)f" withGroup "ee")) and (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should (endWith regex ("d(e*)f" withGroup "ee") and endWith regex ("d(e*)f" withGroup "ee"))

        "deef" should (endWith regex ("d(e*)f" withGroup "ee") and (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should ((endWith regex ("d(e*)f" withGroup "ee")) and (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should (endWith regex ("d(e*)f" withGroup "ee") and endWith regex ("d(e*)f" withGroup "ee"))
        
        "abcdeef" should (equal ("abcdeef") and (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should ((equal ("abcdeef")) and (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should (equal ("abcdeef") and endWith regex ("d(e*)f" withGroup "ee"))

        "deef" should (equal ("deef") and (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should ((equal ("deef")) and (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should (equal ("deef") and endWith regex ("d(e*)f" withGroup "ee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {

        "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))

        "deeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        
        "abcdeeff" should (equal ("abcdeeff") and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should ((equal ("abcdeeff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should (equal ("abcdeeff") and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))

        "deeff" should (equal ("deeff") and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should ((equal ("deeff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should (equal ("deeff") and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "b1.7" should (endWith regex ("hello") or (endWith regex (decimal)))
        "b1.7" should ((endWith regex ("hello")) or (endWith regex (decimal)))
        "b1.7" should (endWith regex ("hello") or endWith regex (decimal))
  
        "1.7" should (endWith regex ("hello") or (endWith regex (decimal)))
        "1.7" should ((endWith regex ("hello")) or (endWith regex (decimal)))
        "1.7" should (endWith regex ("hello") or endWith regex (decimal))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {

        "abcdeef" should (endWith regex ("d(e*)f" withGroup "eee") or (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should ((endWith regex ("d(e*)f" withGroup "eee")) or (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should (endWith regex ("d(e*)f" withGroup "eee") or endWith regex ("d(e*)f" withGroup "ee"))
  
        "deef" should (endWith regex ("d(e*)f" withGroup "eee") or (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should ((endWith regex ("d(e*)f" withGroup "eee")) or (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should (endWith regex ("d(e*)f" withGroup "eee") or endWith regex ("d(e*)f" withGroup "ee"))
        
        "abcdeef" should (equal ("abcdeeef") or (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should ((equal ("abcdeeef")) or (endWith regex ("d(e*)f" withGroup "ee")))
        "abcdeef" should (equal ("abcdeeef") or endWith regex ("d(e*)f" withGroup "ee"))
  
        "deef" should (equal ("deef") or (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should ((equal ("deef")) or (endWith regex ("d(e*)f" withGroup "ee")))
        "deef" should (equal ("deef") or endWith regex ("d(e*)f" withGroup "ee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {

        "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) or endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
  
        "deeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) or endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        
        "abcdeeff" should (equal ("abcdeefff") or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should ((equal ("abcdeefff")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "abcdeeff" should (equal ("abcdeefff") or endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
  
        "deeff" should (equal ("deeff") or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should ((equal ("deeff")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        "deeff" should (equal ("deeff") or endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (endWith regex ("bob")) and not (endWith regex (decimal)))
        "fred" should ((not endWith regex ("bob")) and (not endWith regex (decimal)))
        "fred" should (not endWith regex ("bob") and not endWith regex (decimal))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression with not` {
        "abcdeef" should (not (endWith regex ("d(e*)f" withGroup "e")) and not (endWith regex ("d(e*)f" withGroup "eee")))
        "abcdeef" should ((not endWith regex ("d(e*)f" withGroup "e")) and (not endWith regex ("d(e*)f" withGroup "eee")))
        "abcdeef" should (not endWith regex ("d(e*)f" withGroup "e") and not endWith regex ("d(e*)f" withGroup "eee"))
        
        "abcdeef" should (not (equal ("abcdef")) and not (endWith regex ("d(e*)f" withGroup "eee")))
        "abcdeef" should ((not equal ("abcdef")) and (not endWith regex ("d(e*)f" withGroup "eee")))
        "abcdeef" should (not equal ("abcdef") and not endWith regex ("d(e*)f" withGroup "eee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression with not` {
        "abcdeeff" should (not (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))) and not (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "abcdeeff" should ((not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "abcdeeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) and not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        
        "abcdeeff" should (not (equal ("abcdeefff")) and not (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "abcdeeff" should ((not equal ("abcdeefff")) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "abcdeeff" should (not equal ("abcdeefff") and not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (endWith regex ("fred")) or not (endWith regex (decimal)))
        "fred" should ((not endWith regex ("fred")) or (not endWith regex (decimal)))
        "fred" should (not endWith regex ("fred") or not endWith regex (decimal))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression with not` {
        "deef" should (not (endWith regex ("d(e*)f" withGroup "ee")) or not (endWith regex ("d(e*)f" withGroup "eee")))
        "deef" should ((not endWith regex ("d(e*)f" withGroup "ee")) or (not endWith regex ("d(e*)f" withGroup "eee")))
        "deef" should (not endWith regex ("d(e*)f" withGroup "ee") or not endWith regex ("d(e*)f" withGroup "eee"))
        
        "deef" should (not (equal ("deef")) or not (endWith regex ("d(e*)f" withGroup "eee")))
        "deef" should ((not equal ("deef")) or (not endWith regex ("d(e*)f" withGroup "eee")))
        "deef" should (not equal ("deef") or not endWith regex ("d(e*)f" withGroup "eee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression with not` {
        "deeff" should (not (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) or not (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "deeff" should ((not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "deeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) or not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        
        "deeff" should (not (equal ("deeff")) or not (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "deeff" should ((not equal ("deeff")) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        "deeff" should (not equal ("deeff") or not endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
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
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeef" should endWith regex ("d(e*)f" withGroup "eee")
        }
        assert(caught1.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
      }
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeeff" should endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))
        }
        assert(caught1.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
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
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deef" should not { endWith regex ("d(e*)f" withGroup "ee") }
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "deef" should not endWith regex ("d(e*)f" withGroup "ee")
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // The rest are non-exact matches
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should not { endWith regex ("d(e*)f" withGroup "ee") }
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should not endWith regex ("d(e*)f" withGroup "ee")
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))  
        
      }
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deeff" should not { endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) }
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "deeff" should not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // The rest are non-exact matches
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should not { endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) }
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))  
        
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "ee") and (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught1.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f" withGroup "ee")) and (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught2.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "ee") and endWith regex ("d(e*)f" withGroup "eee"))
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "eee") and (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f" withGroup "eee")) and (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught5.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "eee") and endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught6.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeef") and (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught7.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdeef")) and (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught8.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeef") and endWith regex ("d(e*)f" withGroup "eee"))
        }
        assert(caught9.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeeef") and (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught10.getMessage === "\"abcdee[]f\" did not equal \"abcdee[e]f\"")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdeeef")) and (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught11.getMessage === "\"abcdee[]f\" did not equal \"abcdee[e]f\"")
        assert(caught11.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeeef") and endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught12.getMessage === "\"abcdee[]f\" did not equal \"abcdee[e]f\"")
        assert(caught12.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught1.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught2.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")) and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught6.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeeff") and (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught7.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeeff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught8.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeeff") and endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        }
        assert(caught9.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeefff") and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught10.getMessage === "\"abcdeeff[]\" did not equal \"abcdeeff[f]\"")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeefff")) and (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught11.getMessage === "\"abcdeeff[]\" did not equal \"abcdeeff[f]\"")
        assert(caught11.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeefff") and endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught12.getMessage === "\"abcdeeff[]\" did not equal \"abcdeeff[f]\"")
        assert(caught12.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "e") or (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught1.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f" withGroup "e")) or (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught2.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f" withGroup "e") or endWith regex ("d(e*)f" withGroup "eee"))
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") or (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught4.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdef")) or (endWith regex ("d(e*)f" withGroup "eee")))
        }
        assert(caught5.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") or endWith regex ("d(e*)f" withGroup "eee"))
        }
        assert(caught6.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "f")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught1.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)" withGroups ("ee", "f"))) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught2.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)" withGroups ("ee", "f")) or endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") or (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught4.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeef")) or (endWith regex ("d(e*)(f*)" withGroups ("ee", "fff"))))
        }
        assert(caught5.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") or endWith regex ("d(e*)(f*)" withGroups ("ee", "fff")))
        }
        assert(caught6.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f" withGroup "e") and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "deef" should ((not endWith regex ("d(e*)f" withGroup "e")) and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f" withGroup "e") and not endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught3.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (not endWith regex ("d(e*)f" withGroup "e") and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          "deef" should (not equal ("def") and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught5.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "deef" should ((not equal ("def")) and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught6.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught7 = intercept[TestFailedException] {
          "deef" should (not equal ("def") and not endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught7.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeef" should (not equal ("abcdef") and (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught8.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "f")) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "deeff" should ((not endWith regex ("d(e*)(f*)" withGroups ("ee", "f"))) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "f")) and not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught3.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "f")) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          "deeff" should (not equal ("deef") and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "deeff" should ((not equal ("deef")) and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught6.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught7 = intercept[TestFailedException] {
          "deeff" should (not equal ("deef") and not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught7.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeeff" should (not equal ("abcdeef") and (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught8.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f" withGroup "ee") or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deef" should ((not endWith regex ("d(e*)f" withGroup "ee")) or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f" withGroup "ee") or not endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught3.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "deef" should (not (endWith regex ("d(e*)f" withGroup "ee")) or not (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught4.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should (not endWith regex ("d(e*)f" withGroup "ee") or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught5.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          "deef" should (not equal ("deef") or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught6.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "deef" should ((not equal ("deef")) or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught7.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "deef" should (not equal ("deef") or not endWith regex ("d(e*)f" withGroup "ee"))
        }
        assert(caught8.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "deef" should (not (equal ("deef")) or not (endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught9.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abcdeef" should (not equal ("abcdeef") or (not endWith regex ("d(e*)f" withGroup "ee")))
        }
        assert(caught10.getMessage === "\"abcdeef\" equaled \"abcdeef\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deeff" should ((not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) or not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught3.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "deeff" should (not (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))) or not (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          "deeff" should (not equal ("deeff") or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught6.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "deeff" should ((not equal ("deeff")) or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught7.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "deeff" should (not equal ("deeff") or not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff")))
        }
        assert(caught8.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "deeff" should (not (equal ("deeff")) or not (endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught9.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abcdeeff" should (not equal ("abcdeeff") or (not endWith regex ("d(e*)(f*)" withGroups ("ee", "ff"))))
        }
        assert(caught10.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should do nothing if the string ends with substring that matched the regular expression specified as a string and withGroup` {

        "abcdeef" should endWith regex ("d(e*)f".r withGroup "ee")
        "deef" should endWith regex ("d(e*)f".r withGroup "ee")
        
      }
      
      def `should do nothing if the string ends with substring that matched the regular expression specified as a string and withGroups` {

        "abcdeeff" should endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))
        "deeff" should endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))
        
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used with not` {

        "eight" should not { endWith regex (decimalRegex) }
        "one.eight" should not { endWith regex (decimalRegex) }

        "eight" should not endWith regex (decimalRegex)
        "one.eight" should not endWith regex (decimalRegex)
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used with not` {

        "deef" should not { endWith regex ("d(e*)f".r withGroup "e") }
        "abcdeef" should not { endWith regex ("d(e*)f".r withGroup "e") }

        "deef" should not endWith regex ("d(e*)f".r withGroup "e")
        "abcdeef" should not endWith regex ("d(e*)f".r withGroup "e")
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used with not` {

        "deeff" should not { endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) }
        "abcdeeff" should not { endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) }

        "deeff" should not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))
        "abcdeeff" should not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression` {

        "b1.7" should (endWith regex (decimalRegex) and (endWith regex (decimalRegex)))
        "b1.7" should ((endWith regex (decimalRegex)) and (endWith regex (decimalRegex)))
        "b1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))

        "1.7" should (endWith regex (decimalRegex) and (endWith regex (decimalRegex)))
        "1.7" should ((endWith regex (decimalRegex)) and (endWith regex (decimalRegex)))
        "1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {

        "abcdeef" should (endWith regex ("d(e*)f".r withGroup "ee") and (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should ((endWith regex ("d(e*)f".r withGroup "ee")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should (endWith regex ("d(e*)f".r withGroup "ee") and endWith regex ("d(e*)f".r withGroup "ee"))

        "deef" should (endWith regex ("d(e*)f".r withGroup "ee") and (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should ((endWith regex ("d(e*)f".r withGroup "ee")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should (endWith regex ("d(e*)f".r withGroup "ee") and endWith regex ("d(e*)f".r withGroup "ee"))
        
        "abcdeef" should (equal ("abcdeef") and (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should ((equal ("abcdeef")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should (equal ("abcdeef") and endWith regex ("d(e*)f".r withGroup "ee"))

        "deef" should (equal ("deef") and (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should ((equal ("deef")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should (equal ("deef") and endWith regex ("d(e*)f".r withGroup "ee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {

        "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))

        "deeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        
        "abcdeeff" should (equal ("abcdeeff") and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should ((equal ("abcdeeff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should (equal ("abcdeeff") and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))

        "deeff" should (equal ("deeff") and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should ((equal ("deeff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should (equal ("deeff") and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression` {

        "b1.7" should (endWith regex ("hello") or (endWith regex (decimalRegex)))
        "b1.7" should ((endWith regex ("hello")) or (endWith regex (decimalRegex)))
        "b1.7" should (endWith regex ("hello") or endWith regex (decimalRegex))
  
        "1.7" should (endWith regex ("hello") or (endWith regex (decimalRegex)))
        "1.7" should ((endWith regex ("hello")) or (endWith regex (decimalRegex)))
        "1.7" should (endWith regex ("hello") or endWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {

        "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") or (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should ((endWith regex ("d(e*)f".r withGroup "e")) or (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") or endWith regex ("d(e*)f".r withGroup "ee"))
  
        "deef" should (endWith regex ("d(e*)f".r withGroup "e") or (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should ((endWith regex ("d(e*)f".r withGroup "e")) or (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should (endWith regex ("d(e*)f".r withGroup "e") or endWith regex ("d(e*)f".r withGroup "ee"))
        
        "abcdeef" should (equal ("abcdef") or (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should ((equal ("abcdef")) or (endWith regex ("d(e*)f".r withGroup "ee")))
        "abcdeef" should (equal ("abcdef") or endWith regex ("d(e*)f".r withGroup "ee"))
  
        "deef" should (equal ("def") or (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should ((equal ("def")) or (endWith regex ("d(e*)f".r withGroup "ee")))
        "deef" should (equal ("def") or endWith regex ("d(e*)f".r withGroup "ee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {

        "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
  
        "deeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        
        "abcdeeff" should (equal ("abcdeef") or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should ((equal ("abcdeef")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "abcdeeff" should (equal ("abcdeef") or endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
  
        "deeff" should (equal ("deef") or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should ((equal ("deef")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        "deeff" should (equal ("deef") or endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-and expression with not` {
        "fred" should (not (endWith regex ("bob")) and not (endWith regex (decimalRegex)))
        "fred" should ((not endWith regex ("bob")) and (not endWith regex (decimalRegex)))
        "fred" should (not endWith regex ("bob") and not endWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression with not` {
        "abcdeef" should (not (endWith regex ("d(e*)f".r withGroup "e")) and not (endWith regex ("d(e*)f".r withGroup "eee")))
        "abcdeef" should ((not endWith regex ("d(e*)f".r withGroup "e")) and (not endWith regex ("d(e*)f".r withGroup "eee")))
        "abcdeef" should (not endWith regex ("d(e*)f".r withGroup "e") and not endWith regex ("d(e*)f".r withGroup "eee"))
        
        "abcdeef" should (not (equal ("abcdef")) and not (endWith regex ("d(e*)f".r withGroup "eee")))
        "abcdeef" should ((not equal ("abcdef")) and (not endWith regex ("d(e*)f".r withGroup "eee")))
        "abcdeef" should (not equal ("abcdef") and not endWith regex ("d(e*)f".r withGroup "eee"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression with not` {
        "abcdeeff" should (not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) and not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        "abcdeeff" should ((not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        "abcdeeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and not endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff")))
        
        "abcdeeff" should (not (equal ("abcdeef")) and not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        "abcdeeff" should ((not equal ("abcdeef")) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        "abcdeeff" should (not equal ("abcdeef") and not endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff")))
      }
  
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string when used in a logical-or expression with not` {
        "fred" should (not (endWith regex ("fred")) or not (endWith regex (decimalRegex)))
        "fred" should ((not endWith regex ("fred")) or (not endWith regex (decimalRegex)))
        "fred" should (not endWith regex ("fred") or not endWith regex (decimalRegex))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression with not` {
        "abcdeef" should (not (endWith regex ("d(e*)f".r withGroup "ee")) or not (endWith regex ("d(e*)f".r withGroup "e")))
        "abcdeef" should ((not endWith regex ("d(e*)f".r withGroup "ee")) or (not endWith regex ("d(e*)f".r withGroup "e")))
        "abcdeef" should (not endWith regex ("d(e*)f".r withGroup "ee") or not endWith regex ("d(e*)f".r withGroup "e"))
        
        "abcdeef" should (not (equal ("abcdeef")) or not (endWith regex ("d(e*)f".r withGroup "e")))
        "abcdeef" should ((not equal ("abcdeef")) or (not endWith regex ("d(e*)f".r withGroup "e")))
        "abcdeef" should (not equal ("abcdeef") or not endWith regex ("d(e*)f".r withGroup "e"))
      }
      
      def `should do nothing if the string does not end with a substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression with not` {
        "abcdeeff" should (not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) or not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        "abcdeeff" should ((not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        "abcdeeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) or not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")))
        
        "abcdeeff" should (not (equal ("abcdeeff")) or not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        "abcdeeff" should ((not equal ("abcdeeff")) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        "abcdeeff" should (not equal ("abcdeeff") or not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")))
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
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroup` {
  
        val caught1 = intercept[TestFailedException] {
          "deef" should endWith regex ("d(e*)f".r withGroup "e")
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abcdeef" should endWith regex ("d(e*)f".r withGroup "e")
        }
        assert(caught2.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string does not match substring that matched the regular expression specified as a string and withGroups` {
  
        val caught1 = intercept[TestFailedException] {
          "deeff" should endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abcdeeff" should endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))
        }
        assert(caught2.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroup when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deef" should not { endWith regex ("d(e*)f".r withGroup "ee") }
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deef" should not endWith regex ("d(e*)f".r withGroup "ee")
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // The rest are non-exact matches
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should not { endWith regex ("d(e*)f".r withGroup "ee") }
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should not endWith regex ("d(e*)f".r withGroup "ee")
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
      }
      
      def `should throw TestFailedException if the string does matches substring that matched the regular expression specified as a string and withGroups when used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deeff" should not { endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) }
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deeff" should not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // The rest are non-exact matches
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should not { endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) }
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "ee") and (endWith regex ("d(e*)f".r withGroup "e")))
        }
        assert(caught1.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f".r withGroup "ee")) and (endWith regex ("d(e*)f".r withGroup "e")))
        }
        assert(caught2.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "ee") and endWith regex ("d(e*)f".r withGroup "e"))
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") and (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f".r withGroup "e")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught5.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") and endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught6.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeef") and (endWith regex ("d(e*)f".r withGroup "e")))
        }
        assert(caught7.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdeef")) and (endWith regex ("d(e*)f".r withGroup "e")))
        }
        assert(caught8.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdeef") and endWith regex ("d(e*)f".r withGroup "e"))
        }
        assert(caught9.getMessage === "\"abcdeef\" equaled \"abcdeef\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") and (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught10.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\"")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdef")) and (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught11.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\"")
        assert(caught11.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") and endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught12.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\"")
        assert(caught12.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        }
        assert(caught1.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        }
        assert(caught2.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) and endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")))
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught6.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeeff") and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        }
        assert(caught7.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeeff")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))))
        }
        assert(caught8.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeeff") and endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")))
        }
        assert(caught9.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught10.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\"")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeef")) and (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught11.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\"")
        assert(caught11.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") and endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught12.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\"")
        assert(caught12.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") or (endWith regex ("d(e*)f".r withGroup "eee")))
        }
        assert(caught1.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeef" should ((endWith regex ("d(e*)f".r withGroup "e")) or (endWith regex ("d(e*)f".r withGroup "eee")))
        }
        assert(caught2.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeef" should (endWith regex ("d(e*)f".r withGroup "e") or endWith regex ("d(e*)f".r withGroup "eee"))
        }
        assert(caught3.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") or (endWith regex ("d(e*)f".r withGroup "eee")))
        }
        assert(caught4.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should ((equal ("abcdef")) or (endWith regex ("d(e*)f".r withGroup "eee")))
        }
        assert(caught5.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeef" should (equal ("abcdef") or endWith regex ("d(e*)f".r withGroup "eee"))
        }
        assert(caught6.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group eee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression` {
  
        val caught1 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        }
        assert(caught1.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abcdeeff" should ((endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        }
        assert(caught2.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abcdeeff" should (endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) or endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff")))
        }
        assert(caught3.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        }
        assert(caught4.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should ((equal ("abcdeef")) or (endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff"))))
        }
        assert(caught5.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abcdeeff" should (equal ("abcdeef") or endWith regex ("d(e*)(f*)".r withGroups ("ee", "fff")))
        }
        assert(caught6.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group fff at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f".r withGroup "e") and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "deef" should ((not endWith regex ("d(e*)f".r withGroup "e")) and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f".r withGroup "e") and not endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught3.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abcdeef" should (not endWith regex ("d(e*)f".r withGroup "e") and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught4.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f, but \"ee\" did not match group e, but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          "deef" should (not equal ("def") and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught5.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "deef" should ((not equal ("def")) and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught6.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught7 = intercept[TestFailedException] {
          "deef" should (not equal ("def") and not endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught7.getMessage === "\"de[e]f\" did not equal \"de[]f\", but \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeef" should (not equal ("abcdef") and (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught8.getMessage === "\"abcde[e]f\" did not equal \"abcde[]f\", but \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-and expression used with not` {

        val caught1 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "deeff" should ((not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f"))) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught3.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abcdeeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "f")) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*), but \"ff\" did not match group f at index 1, but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught5 = intercept[TestFailedException] {
          "deeff" should (not equal ("deef") and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "deeff" should ((not equal ("deef")) and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught6.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught7 = intercept[TestFailedException] {
          "deeff" should (not equal ("deef") and not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught7.getMessage === "\"deef[f]\" did not equal \"deef[]\", but \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abcdeeff" should (not equal ("abcdeef") and (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught8.getMessage === "\"abcdeef[f]\" did not equal \"abcdeef[]\", but \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
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
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroup when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f".r withGroup "ee") or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught1.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deef" should ((not endWith regex ("d(e*)f".r withGroup "ee")) or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught2.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "deef" should (not endWith regex ("d(e*)f".r withGroup "ee") or not endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught3.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "deef" should (not (endWith regex ("d(e*)f".r withGroup "ee")) or not (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught4.getMessage === "\"deef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeef" should (not endWith regex ("d(e*)f".r withGroup "ee") or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught5.getMessage === "\"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee, and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          "deef" should (not equal ("deef") or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught6.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "deef" should ((not equal ("deef")) or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught7.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "deef" should (not equal ("deef") or not endWith regex ("d(e*)f".r withGroup "ee"))
        }
        assert(caught8.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "deef" should (not (equal ("deef")) or not (endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught9.getMessage === "\"deef\" equaled \"deef\", and \"deef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abcdeef" should (not equal ("abcdeef") or (not endWith regex ("d(e*)f".r withGroup "ee")))
        }
        assert(caught10.getMessage === "\"abcdeef\" equaled \"abcdeef\", and \"abcdeef\" ended with a substring that matched the regular expression d(e*)f and group ee")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      def `should throw TestFailedException if the string ends with substring that matched the regular expression specified as a string and withGroups when used in a logical-or expression used with not` {
  
        val caught1 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught1.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught1.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "deeff" should ((not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught2.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught2.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "deeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) or not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught3.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught3.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "deeff" should (not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))) or not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught4.getMessage === "\"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught4.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abcdeeff" should (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught5.getMessage === "\"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff, and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught5.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught6 = intercept[TestFailedException] {
          "deeff" should (not equal ("deeff") or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught6.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught6.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "deeff" should ((not equal ("deeff")) or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught7.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught7.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "deeff" should (not equal ("deeff") or not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff")))
        }
        assert(caught8.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught8.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught9 = intercept[TestFailedException] {
          "deeff" should (not (equal ("deeff")) or not (endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught9.getMessage === "\"deeff\" equaled \"deeff\", and \"deeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught9.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abcdeeff" should (not equal ("abcdeeff") or (not endWith regex ("d(e*)(f*)".r withGroups ("ee", "ff"))))
        }
        assert(caught10.getMessage === "\"abcdeeff\" equaled \"abcdeeff\", and \"abcdeeff\" ended with a substring that matched the regular expression d(e*)(f*) and group ee, ff")
        assert(caught10.failedCodeFileName === Some("ShouldEndWithRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}

