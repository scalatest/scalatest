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

import org.scalatest.exceptions.TestFailedException
import SharedHelpers._
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class ShouldIncludeRegexSpec extends AnyFunSpec with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should startWith substring t
s should startWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  describe("The include regex syntax") {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    describe("(when the regex is specified by a string)") {

      it("should do nothing if the string includes substring that matched regex specified as a string") {

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
      
      it("should do nothing if the string includes substring that matched regex specified as a string and withGroup") {

        "abccde" should include regex("b(c*)d" withGroup "cc")
        "bccde" should include regex("b(c*)d" withGroup "cc")
        "abccd" should include regex("b(c*)d" withGroup "cc")
        // full matches, which should also work with "include"
        "bccd" should include regex("b(c*)d" withGroup "cc")
      }
      
      it("should do nothing if the string includes substring that matched regex specified as a string and withGroups") {

        "abccdde" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        "bccdde" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        "abccdd" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
        // full matches, which should also work with "include"
        "bccdd" should include regex("b(c*)(d*)" withGroups ("cc", "dd"))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used with not") {

        "eight" should not { include regex (decimal) }
        "one.eight" should not { include regex (decimal) }

        "eight" should not include regex (decimal)
        "one.eight" should not include regex (decimal)
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used with not") {

        "bccde" should not { include regex ("b(c*)d" withGroup "c") }
        "abccde" should not { include regex ("b(c*)d" withGroup "c") }

        "bccde" should not include regex ("b(c*)d" withGroup "c")
        "abccde" should not include regex ("b(c*)d" withGroup "c")
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used with not") {

        "bccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "d")) }
        "abccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "d")) }

        "bccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "d"))
        "abccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "d"))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

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
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression") {

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
        
        "abccd" should (equal ("abccd") and (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (equal ("abccd") and (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (equal ("abccd") and (include regex ("b(c*)d" withGroup "cc")))

        "bccde" should ((equal ("bccde")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((equal ("bccde")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((equal ("bccde")) and (include regex ("b(c*)d" withGroup "cc")))

        "abccde" should (equal ("abccde") and include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (equal ("abccde") and include regex ("b(c*)d" withGroup "cc"))
        "abccde" should (equal ("abccde") and include regex ("b(c*)d" withGroup "cc"))

        "bccd" should (equal ("bccd") and (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should ((equal ("bccd")) and (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should (equal ("bccd") and include regex ("b(c*)d" withGroup "cc"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression") {

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
        
        "abccdd" should (equal ("abccdd") and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (equal ("abccdd") and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdd" should (equal ("abccdd") and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "bccdde" should ((equal ("bccdde")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdde" should ((equal ("bccdde")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdde" should ((equal ("bccdde")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))

        "bccdd" should (equal ("bccdd") and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should ((equal ("bccdd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should (equal ("bccdd") and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

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
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression") {

        "abccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))

        "bccde" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))

        "abccde" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
  
        "bccd" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "cc"))
        
        "abccd" should (equal ("abcd") or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should ((equal ("abcd")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccd" should (equal ("abcd") or include regex ("b(c*)d" withGroup "cc"))

        "bccde" should (equal ("bcde") or (include regex ("b(c*)d" withGroup "cc")))
        "bccde" should ((equal ("bcde")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should ((equal ("bcde")) or (include regex ("b(c*)d" withGroup "cc")))

        "abccde" should (equal ("abcde") or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should ((equal ("abcde")) or (include regex ("b(c*)d" withGroup "cc")))
        "abccde" should (equal ("abcde") or include regex ("b(c*)d" withGroup "cc"))
  
        "bccd" should (equal ("bcd") or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should ((equal ("bcd")) or (include regex ("b(c*)d" withGroup "cc")))
        "bccd" should (equal ("bcd") or include regex ("b(c*)d" withGroup "cc"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression") {

        "abccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "bccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
  
        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        
        "abccdd" should (equal ("abccd") or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "bccdde" should ((equal ("bccde")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "abccdde" should ((equal ("bccde")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))

        "abccdde" should (equal ("abccde") or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
  
        "bccdd" should (equal ("bccd") or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should ((equal ("bccd")) or (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        "bccdd" should (equal ("bccd") or include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not") {
        "fred" should (not (include regex ("bob")) and not (include regex (decimal)))
        "fred" should ((not include regex ("bob")) and (not include regex (decimal)))
        "fred" should (not include regex ("bob") and not include regex (decimal))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression with not") {
        "abccde" should (not (include regex ("b(c*)d" withGroup "c")) and not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "c"))
        
        "abccde" should (not (equal ("abcde")) and not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not equal ("abcde")) and (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not equal ("abcde") and not include regex ("b(c*)d" withGroup "c"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression with not") {
        "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        
        "abccdde" should (not (equal ("abccde")) and not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not equal ("abccde")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not equal ("abccde") and not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not") {
        "fred" should (not (include regex ("fred")) or not (include regex (decimal)))
        "fred" should ((not include regex ("fred")) or (not include regex (decimal)))
        "fred" should (not include regex ("fred") or not include regex (decimal))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression with not") {
        "abccde" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "c"))
        
        "abccde" should (not (equal ("abccde")) or not (include regex ("b(c*)d" withGroup "c")))
        "abccde" should ((not equal ("abccde")) or (not include regex ("b(c*)d" withGroup "c")))
        "abccde" should (not equal ("abccde") or not include regex ("b(c*)d" withGroup "c"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression with not") {
        "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        
        "abccdde" should (not (equal ("abccdde")) or not (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should ((not equal ("abccdde")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        "abccdde" should (not equal ("abccdde") or not include regex ("b(c*)(d*)" withGroups ("cc", "d")))
      }
  
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string") {
  
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
      
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroup") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should include regex ("b(c*)d" withGroup "c")
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroups") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should include regex ("b(c*)(d*)" withGroups ("cc", "d"))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
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
      
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroup when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should not { include regex ("b(c*)d" withGroup "cc") }
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should not include regex ("b(c*)d" withGroup "cc")
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroups when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should not { include regex ("b(c*)(d*)" withGroups ("cc", "dd")) }
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "cc") and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "cc")) and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "cc") and include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "c")) and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") and include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abccde" should (equal ("abccde") and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught7.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          "abccde" should ((equal ("abccde")) and (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught8.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "abccde" should (equal ("abccde") and include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught9.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught10.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abccde" should ((equal ("abcde")) and (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught11.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") and include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught12.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "dd")) and include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccdde") and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught7.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccdde")) and (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught8.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught9.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccde")) and (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught11.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") and include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught12.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d" withGroup "c")) or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d" withGroup "c") or include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught4.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((equal ("abcde")) or (include regex ("b(c*)d" withGroup "c")))
        }
        assert(caught5.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") or include regex ("b(c*)d" withGroup "c"))
        }
        assert(caught6.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)" withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)" withGroups ("cc", "d")) or include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught4.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccde")) or (include regex ("b(c*)(d*)" withGroups ("cc", "d"))))
        }
        assert(caught5.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") or include regex ("b(c*)(d*)" withGroups ("cc", "d")))
        }
        assert(caught6.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "c") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d" withGroup "c") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d" withGroup "c")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"bccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d" withGroup "c") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "bccd" should (not equal ("bcd") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught7.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught8 = intercept[TestFailedException] {
          "bccd" should ((not equal ("bcd")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught8.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught9 = intercept[TestFailedException] {
          "bccd" should (not equal ("bcd") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught9.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abccd" should (not equal ("abcd") and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught10.getMessage === "\"abc[c]d\" did not equal \"abc[]d\", but \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught11 = intercept[TestFailedException] {
          "bccde" should ((not equal ("bcde")) and (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught11.getMessage === "\"bc[c]de\" did not equal \"bc[]de\", but \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught12 = intercept[TestFailedException] {
          "abccde" should (not equal ("abcde") and not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught12.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", but \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "d")) and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccd") and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught7.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught8 = intercept[TestFailedException] {
          "bccdd" should ((not equal ("bccd")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught9 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccd") and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught9.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abccdd" should (not equal ("abccd") and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"abccd[d]\" did not equal \"abccd[]\", but \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught11 = intercept[TestFailedException] {
          "bccdde" should ((not equal ("bccde")) and (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught11.getMessage === "\"bccd[d]e\" did not equal \"bccd[]e\", but \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught12 = intercept[TestFailedException] {
          "abccdde" should (not equal ("abccde") and not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught12.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", but \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "cc") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccd" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught4.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d" withGroup "cc") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccd\" included substring that matched regex b(c*)d and group cc, and \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d" withGroup "cc")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught6.getMessage === "\"bccde\" included substring that matched regex b(c*)d and group cc, and \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d" withGroup "cc") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught7.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccde" should (not (include regex ("b(c*)d" withGroup "cc")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught8.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "bccd" should (not equal ("bccd") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught9.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "bccd" should ((not equal ("bccd")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught10.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "bccd" should (not equal ("bccd") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught11.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "bccd" should (not (equal ("bccd")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught12.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught13 = intercept[TestFailedException] {
          "abccd" should (not equal ("abccd") or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught13.getMessage === "\"abccd\" equaled \"abccd\", and \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught13.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught14 = intercept[TestFailedException] {
          "bccde" should ((not equal ("bccde")) or (not include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught14.getMessage === "\"bccde\" equaled \"bccde\", and \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught14.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught15 = intercept[TestFailedException] {
          "abccde" should (not equal ("abccde") or not include regex ("b(c*)d" withGroup "cc"))
        }
        assert(caught15.getMessage === "\"abccde\" equaled \"abccde\", and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught15.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught16 = intercept[TestFailedException] {
          "abccde" should (not (equal ("abccde")) or not (include regex ("b(c*)d" withGroup "cc")))
        }
        assert(caught16.getMessage === "\"abccde\" equaled \"abccde\", and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught16.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccdd" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught6.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)" withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught7.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccdde" should (not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccdd") or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught9.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "bccdd" should ((not equal ("bccdd")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccdd") or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught11.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "bccdd" should (not (equal ("bccdd")) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught12.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught13 = intercept[TestFailedException] {
          "abccdd" should (not equal ("abccdd") or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught13.getMessage === "\"abccdd\" equaled \"abccdd\", and \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught13.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught14 = intercept[TestFailedException] {
          "bccdde" should ((not equal ("bccdde")) or (not include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught14.getMessage === "\"bccdde\" equaled \"bccdde\", and \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught14.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught15 = intercept[TestFailedException] {
          "abccdde" should (not equal ("abccdde") or not include regex ("b(c*)(d*)" withGroups ("cc", "dd")))
        }
        assert(caught15.getMessage === "\"abccdde\" equaled \"abccdde\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught15.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught16 = intercept[TestFailedException] {
          "abccdde" should (not (equal ("abccdde")) or not (include regex ("b(c*)(d*)" withGroups ("cc", "dd"))))
        }
        assert(caught16.getMessage === "\"abccdde\" equaled \"abccdde\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught16.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }

    describe("(when the regex is specified by an actual Regex)") {

      it("should do nothing if the string includes substring that matched regex specified as a string") {

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
      
      it("should do nothing if the string includes substring that matched regex specified as a string and withGroup") {

        "abccde" should include regex("b(c*)d".r withGroup "cc")
        "bccde" should include regex("b(c*)d".r withGroup "cc")
        "abccd" should include regex("b(c*)d".r withGroup "cc")
        // full matches, which should also work with "include"
        "bccd" should include regex("b(c*)d".r withGroup "cc")
      }
      
      it("should do nothing if the string includes substring that matched regex specified as a string and withGroups") {

        "abccdde" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        "bccdde" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        "abccdd" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
        // full matches, which should also work with "include"
        "bccdd" should include regex("b(c*)(d*)".r withGroups ("cc", "dd"))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used with not") {

        "eight" should not { include regex (decimalRegex) }
        "one.eight" should not { include regex (decimalRegex) }

        "eight" should not include regex (decimalRegex)
        "one.eight" should not include regex (decimalRegex)
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used with not") {

        "bccde" should not { include regex ("b(c*)d".r withGroup "c") }
        "abccde" should not { include regex ("b(c*)d".r withGroup "c") }

        "bccde" should not include regex ("b(c*)d".r withGroup "c")
        "abccde" should not include regex ("b(c*)d".r withGroup "c")
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used with not") {

        "bccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "d")) }
        "abccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "d")) }

        "bccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
        "abccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

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
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression") {

        "abccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))

        "bccd" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "cc"))
        
        "abccd" should (equal ("abccd") and (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((equal ("bccde")) and (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should (equal ("abccde") and include regex ("b(c*)d".r withGroup "cc"))

        "bccd" should (equal ("bccd") and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((equal ("bccd")) and (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (equal ("bccd") and include regex ("b(c*)d".r withGroup "cc"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression") {

        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))

        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        
        "abccdd" should (equal ("abccdd") and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((equal ("bccdde")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))

        "bccdd" should (equal ("bccdd") and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((equal ("bccdd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (equal ("bccdd") and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

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
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression") {

        "abccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
  
        "bccd" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "cc"))
        
        "abccd" should (equal ("abcd") or (include regex ("b(c*)d".r withGroup "cc")))
        "bccde" should ((equal ("bcde")) or (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should ((equal ("abcde")) or (include regex ("b(c*)d".r withGroup "cc")))
        "abccde" should (equal ("abcde") or include regex ("b(c*)d".r withGroup "cc"))
  
        "bccd" should (equal ("bcd") or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should ((equal ("bcd")) or (include regex ("b(c*)d".r withGroup "cc")))
        "bccd" should (equal ("bcd") or include regex ("b(c*)d".r withGroup "cc"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression") {

        "abccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
  
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        
        "abccdd" should (equal ("abccd") or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdde" should ((equal ("bccde")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should ((equal ("abccde")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "abccdde" should (equal ("abccde") or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
  
        "bccdd" should (equal ("bccd") or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should ((equal ("bccd")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        "bccdd" should (equal ("bccd") or include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression with not") {
        "fred" should (not (include regex ("bob")) and not (include regex (decimalRegex)))
        "fred" should ((not include regex ("bob")) and (not include regex (decimalRegex)))
        "fred" should (not include regex ("bob") and not include regex (decimalRegex))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-and expression with not") {
        "abccde" should (not (include regex ("b(c*)d".r withGroup "c")) and not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "c"))
        
        "abccde" should (not (equal ("abcde")) and not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not equal ("abcde")) and (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not equal ("abcde") and not include regex ("b(c*)d".r withGroup "c"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-and expression with not") {
        "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        
        "abccdde" should (not (equal ("abccde")) and not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not equal ("abccde")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not equal ("abccde") and not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
      }
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression with not") {
        "fred" should (not (include regex ("fred")) or not (include regex (decimalRegex)))
        "fred" should ((not include regex ("fred")) or (not include regex (decimalRegex)))
        "fred" should (not include regex ("fred") or not include regex (decimalRegex))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroup when used in a logical-or expression with not") {
        "abccde" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "c"))
        
        "abccde" should (not (equal ("abccde")) or not (include regex ("b(c*)d".r withGroup "c")))
        "abccde" should ((not equal ("abccde")) or (not include regex ("b(c*)d".r withGroup "c")))
        "abccde" should (not equal ("abccde") or not include regex ("b(c*)d".r withGroup "c"))
      }
      
      it("should do nothing if the string does not include substring that matched regex specified as a string and withGroups when used in a logical-or expression with not") {
        "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        
        "abccdde" should (not (equal ("abccdde")) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should ((not equal ("abccdde")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        "abccdde" should (not equal ("abccdde") or not include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
      }
  
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string") {
  
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
      
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroup") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should include regex ("b(c*)d".r withGroup "c")
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string does not match substring that matched regex specified as a string and withGroups") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should include regex ("b(c*)(d*)".r withGroups ("cc", "d"))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
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
      
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroup when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should not { include regex ("b(c*)d".r withGroup "cc") }
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should not include regex ("b(c*)d".r withGroup "cc")
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string and withGroups when used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should not { include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) }
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "cc") and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "cc")) and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "cc") and include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "c")) and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") and include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abccde" should (equal ("abccde") and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught7.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          "abccde" should ((equal ("abccde")) and (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught8.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "abccde" should (equal ("abccde") and include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught9.getMessage === "\"abccde\" equaled \"abccde\", but \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught10.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abccde" should ((equal ("abcde")) and (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught11.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") and include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught12.getMessage === "\"abc[c]de\" did not equal \"abc[]de\"")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) and include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccdde") and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught7.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught8 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccdde")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught8.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccdde") and include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught9.getMessage === "\"abccdde\" equaled \"abccdde\", but \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
        val caught10 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccde")) and (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught11.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") and include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught12.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\"")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught1.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccde" should ((include regex ("b(c*)d".r withGroup "c")) or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught2.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccde" should (include regex ("b(c*)d".r withGroup "c") or include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught3.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught4.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccde" should ((equal ("abcde")) or (include regex ("b(c*)d".r withGroup "c")))
        }
        assert(caught5.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccde" should (equal ("abcde") or include regex ("b(c*)d".r withGroup "c"))
        }
        assert(caught6.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", and \"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression") {
  
        val caught1 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught1.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "abccdde" should ((include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught2.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "abccdde" should (include regex ("b(c*)(d*)".r withGroups ("cc", "d")) or include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught3.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught4 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught4.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdde" should ((equal ("abccde")) or (include regex ("b(c*)(d*)".r withGroups ("cc", "d"))))
        }
        assert(caught5.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "abccdde" should (equal ("abccde") or include regex ("b(c*)(d*)".r withGroups ("cc", "d")))
        }
        assert(caught6.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", and \"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "c") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d".r withGroup "c") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"abccd\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d".r withGroup "c")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"bccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d".r withGroup "c") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught6.getMessage === "\"abccde\" included substring that matched regex b(c*)d, but \"cc\" did not match group c, but \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "bccd" should (not equal ("bcd") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught7.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught8 = intercept[TestFailedException] {
          "bccd" should ((not equal ("bcd")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught8.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught9 = intercept[TestFailedException] {
          "bccd" should (not equal ("bcd") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught9.getMessage === "\"bc[c]d\" did not equal \"bc[]d\", but \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abccd" should (not equal ("abcd") and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught10.getMessage === "\"abc[c]d\" did not equal \"abc[]d\", but \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught11 = intercept[TestFailedException] {
          "bccde" should ((not equal ("bcde")) and (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught11.getMessage === "\"bc[c]de\" did not equal \"bc[]de\", but \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught12 = intercept[TestFailedException] {
          "abccde" should (not equal ("abcde") and not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught12.getMessage === "\"abc[c]de\" did not equal \"abc[]de\", but \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-and expression used with not") {

        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught5 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "d"))) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught6 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "d")) and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught6.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*), but \"dd\" did not match group d at index 1, but \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught7 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccd") and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught7.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught8 = intercept[TestFailedException] {
          "bccdd" should ((not equal ("bccd")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught9 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccd") and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught9.getMessage === "\"bccd[d]\" did not equal \"bccd[]\", but \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "abccdd" should (not equal ("abccd") and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"abccd[d]\" did not equal \"abccd[]\", but \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught11 = intercept[TestFailedException] {
          "bccdde" should ((not equal ("bccde")) and (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught11.getMessage === "\"bccd[d]e\" did not equal \"bccd[]e\", but \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))

        val caught12 = intercept[TestFailedException] {
          "abccdde" should (not equal ("abccde") and not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught12.getMessage === "\"abccd[d]e\" did not equal \"abccd[]e\", but \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      }

      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
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
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroup when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "cc") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught1.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccd" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught2.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccd" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught3.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccd" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught4.getMessage === "\"bccd\" included substring that matched regex b(c*)d and group cc, and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccd" should (not include regex ("b(c*)d".r withGroup "cc") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught5.getMessage === "\"abccd\" included substring that matched regex b(c*)d and group cc, and \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccde" should ((not include regex ("b(c*)d".r withGroup "cc")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught6.getMessage === "\"bccde\" included substring that matched regex b(c*)d and group cc, and \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccde" should (not include regex ("b(c*)d".r withGroup "cc") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught7.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccde" should (not (include regex ("b(c*)d".r withGroup "cc")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught8.getMessage === "\"abccde\" included substring that matched regex b(c*)d and group cc, and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "bccd" should (not equal ("bccd") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught9.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "bccd" should ((not equal ("bccd")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught10.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "bccd" should (not equal ("bccd") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught11.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "bccd" should (not (equal ("bccd")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught12.getMessage === "\"bccd\" equaled \"bccd\", and \"bccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught13 = intercept[TestFailedException] {
          "abccd" should (not equal ("abccd") or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught13.getMessage === "\"abccd\" equaled \"abccd\", and \"abccd\" included substring that matched regex b(c*)d and group cc")
        assert(caught13.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught14 = intercept[TestFailedException] {
          "bccde" should ((not equal ("bccde")) or (not include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught14.getMessage === "\"bccde\" equaled \"bccde\", and \"bccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught14.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught15 = intercept[TestFailedException] {
          "abccde" should (not equal ("abccde") or not include regex ("b(c*)d".r withGroup "cc"))
        }
        assert(caught15.getMessage === "\"abccde\" equaled \"abccde\", and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught15.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught16 = intercept[TestFailedException] {
          "abccde" should (not (equal ("abccde")) or not (include regex ("b(c*)d".r withGroup "cc")))
        }
        assert(caught16.getMessage === "\"abccde\" equaled \"abccde\", and \"abccde\" included substring that matched regex b(c*)d and group cc")
        assert(caught16.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
      
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string and withGroups when used in a logical-or expression used with not") {
  
        val caught1 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught1.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught1.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught2 = intercept[TestFailedException] {
          "bccdd" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught2.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught2.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught3 = intercept[TestFailedException] {
          "bccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught3.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught3.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught4 = intercept[TestFailedException] {
          "bccdd" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught4.getMessage === "\"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught4.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught5 = intercept[TestFailedException] {
          "abccdd" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught5.getMessage === "\"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught5.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught6 = intercept[TestFailedException] {
          "bccdde" should ((not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught6.getMessage === "\"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught6.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught7 = intercept[TestFailedException] {
          "abccdde" should (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")) or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught7.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught7.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught8 = intercept[TestFailedException] {
          "abccdde" should (not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught8.getMessage === "\"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd, and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught8.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
        
        val caught9 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccdd") or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught9.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught9.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught10 = intercept[TestFailedException] {
          "bccdd" should ((not equal ("bccdd")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught10.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught10.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught11 = intercept[TestFailedException] {
          "bccdd" should (not equal ("bccdd") or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught11.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught11.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught12 = intercept[TestFailedException] {
          "bccdd" should (not (equal ("bccdd")) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught12.getMessage === "\"bccdd\" equaled \"bccdd\", and \"bccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught12.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught13 = intercept[TestFailedException] {
          "abccdd" should (not equal ("abccdd") or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught13.getMessage === "\"abccdd\" equaled \"abccdd\", and \"abccdd\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught13.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught14 = intercept[TestFailedException] {
          "bccdde" should ((not equal ("bccdde")) or (not include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught14.getMessage === "\"bccdde\" equaled \"bccdde\", and \"bccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught14.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught15 = intercept[TestFailedException] {
          "abccdde" should (not equal ("abccdde") or not include regex ("b(c*)(d*)".r withGroups ("cc", "dd")))
        }
        assert(caught15.getMessage === "\"abccdde\" equaled \"abccdde\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught15.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
        val caught16 = intercept[TestFailedException] {
          "abccdde" should (not (equal ("abccdde")) or not (include regex ("b(c*)(d*)".r withGroups ("cc", "dd"))))
        }
        assert(caught16.getMessage === "\"abccdde\" equaled \"abccdde\", and \"abccdde\" included substring that matched regex b(c*)(d*) and group cc, dd")
        assert(caught16.failedCodeFileName === Some("ShouldIncludeRegexSpec.scala"))
        assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      }
    }
  }
}



/*
      it("should do nothing if the string includes substring that matched regex specified as a string") {
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
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-and expression") {

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
  
      it("should do nothing if the string does not include substring that matched regex specified as a string when used in a logical-or expression") {

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
  
      it("should throw TestFailedException if the string does matches substring that matched regex specified as a string when used with not") {
  
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
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-and expression used with not") {

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
  
      it("should throw TestFailedException if the string includes substring that matched regex specified as a string when used in a logical-or expression used with not") {
  
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
