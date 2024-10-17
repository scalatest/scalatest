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

class ShouldFullyMatchSpec extends AnyFunSpec with ReturnsNormallyThrowsAssertion {

/*
s should include substring t
s should include regex t
s should startWith substring t
s should startWith regex t
s should endWith substring t
s should endWith regex t
s should fullyMatch regex t
*/

  describe("The fullyMatch regex syntax") {

    val decimal = """(-)?(\d+)(\.\d*)?"""
    val decimalRegex = """(-)?(\d+)(\.\d*)?""".r

    describe("(when the regex is specified by a string)") {

      it("should do nothing if the string fully matches the regular expression specified as a string") {
        "1.7" should fullyMatch regex ("1.7")
        "1.7" should fullyMatch regex (decimal)
        "-1.8" should fullyMatch regex (decimal)
        "8" should fullyMatch regex (decimal)
        "1." should fullyMatch regex (decimal)
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used with not") {
  
        "eight" should not { fullyMatch regex (decimal) }
        "1.eight" should not { fullyMatch regex (decimal) }
        "one.8" should not { fullyMatch regex (decimal) }
  
        "eight" should not fullyMatch regex (decimal)
        "1.eight" should not fullyMatch regex (decimal)
        "one.8" should not fullyMatch regex (decimal)
        "1.8-" should not fullyMatch regex (decimal)
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression") {
        "1.7" should (fullyMatch regex (decimal) and (fullyMatch regex (decimal)))
        "1.7" should ((fullyMatch regex (decimal)) and (fullyMatch regex (decimal)))
        "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression") {
        "1.7" should (fullyMatch regex ("hello") or (fullyMatch regex (decimal)))
        "1.7" should ((fullyMatch regex ("hello")) or (fullyMatch regex (decimal)))
        "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimal))
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression with not") {
        "fred" should (not (fullyMatch regex ("bob")) and not (fullyMatch regex (decimal)))
        "fred" should ((not fullyMatch regex ("bob")) and (not fullyMatch regex (decimal)))
        "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimal))
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression with not") {
        "fred" should (not (fullyMatch regex ("fred")) or not (fullyMatch regex (decimal)))
        "fred" should ((not fullyMatch regex ("fred")) or (not fullyMatch regex (decimal)))
        "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimal))
      }
  
      it("should throw TestFailedException if the string does not match the regular expression specified as a string") {
  
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
  
      it("should throw TestFailedException if the string does matches the regular expression specified as a string when used with not") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression used with not") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression used with not") {
  
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

    describe("(when the regex is specifed by a string and with group)") {
      
      describe("(when used with should)") {
      
        it("should do nothing if the string fully matches the regular expression and with group as specified") {
          "abbc" should fullyMatch regex ("a(b*)c" withGroup "bb") 
          "abbcc" should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) 
          
          "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bb") and (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bb")) and (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bb") and fullyMatch regex ("a(b*)c" withGroup "bb"))
          
          "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
          
          "abbc" should (equal ("abbc") and (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should ((equal ("abbc")) and (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should (equal ("abbc") and fullyMatch regex ("a(b*)c" withGroup "bb"))
          
          "abbcc" should (equal ("abbcc") and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should ((equal ("abbcc")) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
          
          "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") or (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bbb")) or (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") or fullyMatch regex ("a(b*)c" withGroup "bb"))
          
          "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bbb", "cc")) or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bbb", "cc"))) or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bbb", "cc")) or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
          
          "abbc" should (equal ("abbc") or (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should ((equal ("abbc")) or (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbc" should (equal ("abbc") or fullyMatch regex ("a(b*)c" withGroup "bb"))
          
          "abbcc" should (equal ("abbc") or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should ((equal ("abbc")) or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          "abbcc" should (equal ("abbc") or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
        }
      
        it("should throw TestFailedException if the string fully matches the regular expression but does not match specified group") {
          val caught1 = intercept[TestFailedException] {
            "abbbc" should fullyMatch regex ("a(b*)c" withGroup "bb")
          }
          assert(caught1.message === Some("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"))
          assert(caught1.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
          val caught2 = intercept[TestFailedException] {
            "abbccc" should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
          }
          assert(caught2.message === Some("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"))
          assert(caught2.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught3 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bb") and (fullyMatch regex ("a(b*)c" withGroup "bbb")))
          }
          assert(caught3.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught3.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught4 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bb")) and (fullyMatch regex ("a(b*)c" withGroup "bbb")))
          }
          assert(caught4.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught4.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught5 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bb") and fullyMatch regex ("a(b*)c" withGroup "bbb"))
          }
          assert(caught5.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught5.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught6 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") and fullyMatch regex ("a(b*)c" withGroup "bbbb"))
          }
          assert(caught6.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught6.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught7 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bbb")) and (fullyMatch regex ("a(b*)c" withGroup "bbb")))
          }
          assert(caught7.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught7.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught8 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") and fullyMatch regex ("a(b*)c" withGroup "bbbb"))
          }
          assert(caught8.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught8.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught9 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
          }
          assert(caught9.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught9.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught10 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
          }
          assert(caught10.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught10.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught11 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
          }
          assert(caught11.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught11.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught12 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc")))
          }
          assert(caught12.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught12.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught13 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          }
          assert(caught13.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught13.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught14 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc")))
          }
          assert(caught14.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught14.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught15 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") or (fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          }
          assert(caught15.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught15.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught16 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bbb")) or (fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          }
          assert(caught16.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught16.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught17 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") or fullyMatch regex ("a(b*)c" withGroup "bbbb"))
          }
          assert(caught17.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught17.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught18 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          }
          assert(caught18.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught18.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught19 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) or (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          }
          assert(caught19.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught19.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught20 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc")))
          }
          assert(caught20.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught20.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught21 = intercept[TestFailedException] {
            "abbc" should (equal ("abbc") and (fullyMatch regex ("a(b*)c" withGroup "bbb")))
          }
          assert(caught21.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught21.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught22 = intercept[TestFailedException] {
            "abbc" should ((equal ("abbc")) and (fullyMatch regex ("a(b*)c" withGroup "bbb")))
          }
          assert(caught22.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught22.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught23 = intercept[TestFailedException] {
            "abbc" should (equal ("abbc") and fullyMatch regex ("a(b*)c" withGroup "bbb"))
          }
          assert(caught23.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught23.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught24 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") and (equal ("abbc")))
          }
          assert(caught24.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught24.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught25 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c" withGroup "bbb")) and (equal ("abbc")))
          }
          assert(caught25.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught25.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught26 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c" withGroup "bbb") and equal ("abbc"))
          }
          assert(caught26.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught26.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught27 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbcc") and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
          }
          assert(caught27.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught27.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught28 = intercept[TestFailedException] {
            "abbcc" should ((equal ("abbcc")) and (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))))
          }
          assert(caught28.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught28.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught29 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
          }
          assert(caught29.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught29.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught30 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (equal ("abbcc")))
          }
          assert(caught30.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught30.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught31 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (equal ("abbcc")))
          }
          assert(caught31.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught31.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught32 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and equal ("abbcc"))
          }
          assert(caught32.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught32.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
        }
      }
      
      describe("(when used with should not)") {
        it("should do nothing if the string does not fully match the regular expression and with group as specified") {
          "abbbc" should not { fullyMatch regex ("a(b*)c" withGroup "bb") } 
          "abbbc" should not fullyMatch regex ("a(b*)c" withGroup "bb") 
        
          "abbccc" should not { fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) }
          "abbccc" should not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
          
          "abbbc" should (not (fullyMatch regex ("a(b*)c" withGroup "bb")) and not (fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          "abbbc" should ((not fullyMatch regex ("a(b*)c" withGroup "bb")) and (not fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          "abbbc" should (not fullyMatch regex ("a(b*)c" withGroup "bb") and not fullyMatch regex ("a(b*)c" withGroup "bbbb"))
          
          "abbccc" should (not (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and not (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          "abbccc" should ((not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))) and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          "abbccc" should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc")))
          
          "abbbc" should (not (fullyMatch regex ("a(b*)c" withGroup "bbb")) or not (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbbc" should ((not fullyMatch regex ("a(b*)c" withGroup "bbb")) or (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbbc" should (not fullyMatch regex ("a(b*)c" withGroup "bbb") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
          
          "abbbc" should (not (equal ("abbcc")) and not (fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          "abbbc" should ((not equal "abbcc") and (not fullyMatch regex ("a(b*)c" withGroup "bbbb")))
          "abbbc" should (not equal "abbcc" and not fullyMatch regex ("a(b*)c" withGroup "bbbb"))
          
          "abbccc" should (not (equal ("abbcc")) and not (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          "abbccc" should ((not equal ("abbcc")) and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc"))))
          "abbccc" should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cccc")))
          
          "abbbc" should (not (equal ("abbbc")) or not (fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbbc" should ((not equal ("abbbc")) or (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          "abbbc" should (not equal ("abbbc") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
        }
      
        it("should throw TestFailedException if the string fully matches the regular expression and with group as specified") {
          val caught1 = intercept[TestFailedException] {
            "abbc" should not { fullyMatch regex ("a(b*)c" withGroup "bb") }
          }
          assert(caught1.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
          assert(caught1.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught2 = intercept[TestFailedException] {
            "abbc" should not fullyMatch regex ("a(b*)c" withGroup "bb") 
          }
          assert(caught2.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
          assert(caught2.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught3 = intercept[TestFailedException] {
            "abbcc" should not { fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) }
          }
          assert(caught3.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
          assert(caught3.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught4 = intercept[TestFailedException] {
            "abbcc" should not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
          }
          assert(caught4.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
          assert(caught4.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught5 = intercept[TestFailedException] {
            "abbc" should (not fullyMatch regex ("a(b*)c" withGroup "bbb") and (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          }
          assert(caught5.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught5.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught6 = intercept[TestFailedException] {
            "abbc" should ((not fullyMatch regex ("a(b*)c" withGroup "bbb")) and (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          }
          assert(caught6.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught6.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught7 = intercept[TestFailedException] {
            "abbc" should (not fullyMatch regex ("a(b*)c" withGroup "bbb") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
          }
          assert(caught7.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught7.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught8 = intercept[TestFailedException] {
            "abbcc" should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          }
          assert(caught8.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught8.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught9 = intercept[TestFailedException] {
            "abbcc" should ((not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc"))) and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          }
          assert(caught9.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught9.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught10 = intercept[TestFailedException] {
            "abbcc" should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
          }
          assert(caught10.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught10.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught11 = intercept[TestFailedException] {
            "abbc" should (not equal ("abbcc") and (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          }
          assert(caught11.getMessage === "\"abbc[]\" did not equal \"abbc[c]\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught11.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught12 = intercept[TestFailedException] {
            "abbc" should ((not equal ("abbcc")) and (not fullyMatch regex ("a(b*)c" withGroup "bb")))
          }
          assert(caught12.getMessage === "\"abbc[]\" did not equal \"abbc[c]\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught12.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught13 = intercept[TestFailedException] {
            "abbc" should (not equal ("abbcc") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
          }
          assert(caught13.getMessage === "\"abbc[]\" did not equal \"abbc[c]\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught13.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught14 = intercept[TestFailedException] {
            "abbcc" should (not equal ("abbccc") and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          }
          assert(caught14.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught14.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught15 = intercept[TestFailedException] {
            "abbcc" should ((not equal "abbccc") and (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))))
          }
          assert(caught15.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught15.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught16 = intercept[TestFailedException] {
            "abbcc" should (not equal "abbccc" and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
          }
          assert(caught16.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught16.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
        }
      }
    }
    
    describe("(when the regex is specified by an actual Regex)") {

      it("should do nothing if the string fully matches the regular expression specified as a string") {
        "1.7" should fullyMatch regex ("1.7")
        "1.7" should fullyMatch regex (decimalRegex)
        "-1.8" should fullyMatch regex (decimalRegex)
        "8" should fullyMatch regex (decimalRegex)
        "1." should fullyMatch regex (decimalRegex)
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used with not") {
  
        "eight" should not { fullyMatch regex (decimalRegex) }
        "1.eight" should not { fullyMatch regex (decimalRegex) }
        "one.8" should not { fullyMatch regex (decimalRegex) }
  
        "eight" should not fullyMatch regex (decimalRegex)
        "1.eight" should not fullyMatch regex (decimalRegex)
        "one.8" should not fullyMatch regex (decimalRegex)
        "1.8-" should not fullyMatch regex (decimalRegex)
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression") {
        "1.7" should (fullyMatch regex (decimalRegex) and (fullyMatch regex (decimalRegex)))
        "1.7" should ((fullyMatch regex (decimalRegex)) and (fullyMatch regex (decimalRegex)))
        "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
      }

      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression") {
        "1.7" should (fullyMatch regex ("hello") or (fullyMatch regex (decimalRegex)))
        "1.7" should ((fullyMatch regex ("hello")) or (fullyMatch regex (decimalRegex)))
        "1.7" should (fullyMatch regex ("hello") or fullyMatch regex (decimalRegex))
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-and expression with not") {
        "fred" should (not (fullyMatch regex ("bob")) and not (fullyMatch regex (decimalRegex)))
        "fred" should ((not fullyMatch regex ("bob")) and (not fullyMatch regex (decimalRegex)))
        "fred" should (not fullyMatch regex ("bob") and not fullyMatch regex (decimalRegex))
      }
  
      it("should do nothing if the string does not fully match the regular expression specified as a string when used in a logical-or expression with not") {
        "fred" should (not (fullyMatch regex ("fred")) or not (fullyMatch regex (decimalRegex)))
        "fred" should ((not fullyMatch regex ("fred")) or (not fullyMatch regex (decimalRegex)))
        "fred" should (not fullyMatch regex ("fred") or not fullyMatch regex (decimalRegex))
      }
  
      it("should throw TestFailedException if the string does not match the regular expression specified as a string") {
  
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
  
      it("should throw TestFailedException if the string does matches the regular expression specified as a string when used with not") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-and expression used with not") {
  
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
  
      it("should throw TestFailedException if the string fully matches the regular expression specified as a string when used in a logical-or expression used with not") {
  
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
    
    describe("(when the regex is specifed by a actual Regex and with group)") {
      
      describe("(when used with should)") {
      
        it("should do nothing if the string fully matches the regular expression and with group as specified") {
          "abbc" should fullyMatch regex ("a(b*)c".r withGroup "bb") 
          "abbcc" should fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) 
          
          "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bb") and (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should ((fullyMatch regex ("a(b*)c".r withGroup "bb")) and (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bb") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
          
          "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should ((fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
          
          "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") or (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should ((fullyMatch regex ("a(b*)c".r withGroup "bbb")) or (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
          
          "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bbb", "cc")) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should ((fullyMatch regex ("a(b*)(c*)".r withGroups ("bbb", "cc"))) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bbb", "cc")) or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
          
          "abbc" should (equal ("abbc") and (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should ((equal ("abbc")) and (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should (equal ("abbc") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
          
          "abbcc" should (equal ("abbcc") and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should ((equal ("abbcc")) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
          
          "abbc" should (equal ("abbbc") or (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should ((equal ("abbbc")) or (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbc" should (equal ("abbbc") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
          
          "abbcc" should (equal ("abbbcc") or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should ((equal ("abbbcc")) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          "abbcc" should (equal ("abbbcc") or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
        }
      
        it("should throw TestFailedException if the string fully matches the regular expression but does not match specified group") {
          val caught1 = intercept[TestFailedException] {
            "abbbc" should fullyMatch regex ("a(b*)c".r withGroup "bb")
          }
          assert(caught1.message === Some("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"))
          assert(caught1.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
        
          val caught2 = intercept[TestFailedException] {
            "abbccc" should fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
          }
          assert(caught2.message === Some("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"))
          assert(caught2.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught3 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bb") and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught3.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught3.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught4 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c".r withGroup "bb")) and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught4.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught4.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught5 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bb") and fullyMatch regex ("a(b*)c".r withGroup "bbb"))
          }
          assert(caught5.getMessage === "\"abbc\" fully matched the regular expression a(b*)c and group bb, but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught5.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught6 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") and fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught6.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught6.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught7 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c".r withGroup "bbb")) and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught7.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught7.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught8 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") and fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught8.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught8.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught9 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))))
          }
          assert(caught9.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught9.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught10 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))))
          }
          assert(caught10.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught10.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught11 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
          }
          assert(caught11.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught11.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught12 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught12.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught12.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught13 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught13.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught13.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught14 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught14.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught14.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught15 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") or (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          }
          assert(caught15.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught15.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught16 = intercept[TestFailedException] {
            "abbc" should ((fullyMatch regex ("a(b*)c".r withGroup "bbb")) or (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          }
          assert(caught16.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught16.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught17 = intercept[TestFailedException] {
            "abbc" should (fullyMatch regex ("a(b*)c".r withGroup "bbb") or fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught17.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught17.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught18 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught18.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught18.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught19 = intercept[TestFailedException] {
            "abbcc" should ((fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught19.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught19.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught20 = intercept[TestFailedException] {
            "abbcc" should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught20.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught20.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught21 = intercept[TestFailedException] {
            "abbc" should (equal ("abbc") and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught21.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught21.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught22 = intercept[TestFailedException] {
            "abbc" should ((equal ("abbc")) and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught22.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught22.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught23 = intercept[TestFailedException] {
            "abbc" should (equal ("abbc") and fullyMatch regex ("a(b*)c".r withGroup "bbb"))
          }
          assert(caught23.getMessage === "\"abbc\" equaled \"abbc\", but \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb")
          assert(caught23.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught24 = intercept[TestFailedException] {
            "abbc" should (equal ("abbbc") and fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught24.getMessage === "\"abb[]c\" did not equal \"abb[b]c\"")
          assert(caught24.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught25 = intercept[TestFailedException] {
            "abbc" should ((equal ("abbbc")) and (fullyMatch regex ("a(b*)c".r withGroup "bbb")))
          }
          assert(caught25.getMessage === "\"abb[]c\" did not equal \"abb[b]c\"")
          assert(caught25.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught26 = intercept[TestFailedException] {
            "abbc" should (equal ("abbbc") and fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught26.getMessage === "\"abb[]c\" did not equal \"abb[b]c\"")
          assert(caught26.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught27 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbcc") and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))))
          }
          assert(caught27.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught27.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught28 = intercept[TestFailedException] {
            "abbcc" should ((equal ("abbcc")) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))))
          }
          assert(caught28.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught28.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught29 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
          }
          assert(caught29.getMessage === "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1")
          assert(caught29.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          // Check to make sure the error message "short circuits" (i.e., just reports the left side's failure)
          val caught30 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbccc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught30.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\"")
          assert(caught30.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught31 = intercept[TestFailedException] {
            "abbcc" should ((equal ("abbccc")) and (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught31.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\"")
          assert(caught31.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught32 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbccc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught32.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\"")
          assert(caught32.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught33 = intercept[TestFailedException] {
            "abbc" should (equal ("abbbc") or (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          }
          assert(caught33.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught33.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught33.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught34 = intercept[TestFailedException] {
            "abbc" should ((equal ("abbbc")) or (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          }
          assert(caught34.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught34.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught34.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught35 = intercept[TestFailedException] {
            "abbc" should (equal ("abbbc") or fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          }
          assert(caught35.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", and \"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbbb")
          assert(caught35.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught35.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught36 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbccc") or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught36.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught36.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught36.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught37 = intercept[TestFailedException] {
            "abbcc" should ((equal ("abbccc")) or (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          }
          assert(caught37.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught37.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught37.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught38 = intercept[TestFailedException] {
            "abbcc" should (equal ("abbccc") or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          }
          assert(caught38.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group cccc at index 1")
          assert(caught38.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught38.failedCodeLineNumber === Some(thisLineNumber - 4))
        }
      }
      
      describe("(when used with should not)") {
        it("should do nothing if the string does not fully match the regular expression and with group as specified") {
          "abbbc" should not { fullyMatch regex ("a(b*)c".r withGroup "bb") } 
          "abbbc" should not fullyMatch regex ("a(b*)c".r withGroup "bb") 
        
          "abbccc" should not { fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) }
          "abbccc" should not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
          
          "abbbc" should (not (fullyMatch regex ("a(b*)c".r withGroup "bb")) and not (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          "abbbc" should ((not fullyMatch regex ("a(b*)c".r withGroup "bb")) and (not fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          "abbbc" should (not fullyMatch regex ("a(b*)c".r withGroup "bb") and not fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          
          "abbccc" should (not (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))) and not (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          "abbccc" should ((not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))) and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          "abbccc" should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          
          "abbbc" should (not (fullyMatch regex ("a(b*)c".r withGroup "bbb")) or not (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbbc" should ((not fullyMatch regex ("a(b*)c".r withGroup "bbb")) or (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbbc" should (not fullyMatch regex ("a(b*)c".r withGroup "bbb") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
          
          "abbbc" should (not (equal ("abbc")) and not (fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          "abbbc" should ((not equal ("abbc")) and (not fullyMatch regex ("a(b*)c".r withGroup "bbbb")))
          "abbbc" should (not equal ("abbc") and not fullyMatch regex ("a(b*)c".r withGroup "bbbb"))
          
          "abbccc" should (not (equal ("abbcc")) and not (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          "abbccc" should ((not equal ("abbcc")) and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc"))))
          "abbccc" should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cccc")))
          
          "abbbc" should (not (equal ("abbbc")) or not (fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbbc" should ((not equal ("abbbc")) or (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          "abbbc" should (not equal ("abbbc") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
        }
      
        it("should throw TestFailedException if the string fully matches the regular expression and with group as specified") {
          val caught1 = intercept[TestFailedException] {
            "abbc" should not { fullyMatch regex ("a(b*)c".r withGroup "bb") }
          }
          assert(caught1.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
          assert(caught1.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught2 = intercept[TestFailedException] {
            "abbc" should not fullyMatch regex ("a(b*)c".r withGroup "bb") 
          }
          assert(caught2.message === Some("\"abbc\" fully matched the regular expression a(b*)c and group bb"))
          assert(caught2.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught3 = intercept[TestFailedException] {
            "abbcc" should not { fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) }
          }
          assert(caught3.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
          assert(caught3.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught4 = intercept[TestFailedException] {
            "abbcc" should not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
          }
          assert(caught4.message === Some("\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc"))
          assert(caught4.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught5 = intercept[TestFailedException] {
            "abbc" should (not fullyMatch regex ("a(b*)c".r withGroup "bbb") and (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          }
          assert(caught5.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught5.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught6 = intercept[TestFailedException] {
            "abbc" should ((not fullyMatch regex ("a(b*)c".r withGroup "bbb")) and (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          }
          assert(caught6.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught6.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught7 = intercept[TestFailedException] {
            "abbc" should (not fullyMatch regex ("a(b*)c".r withGroup "bbb") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
          }
          assert(caught7.getMessage === "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught7.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught8 = intercept[TestFailedException] {
            "abbcc" should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          }
          assert(caught8.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught8.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught9 = intercept[TestFailedException] {
            "abbcc" should ((not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc"))) and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          }
          assert(caught9.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught9.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught10 = intercept[TestFailedException] {
            "abbcc" should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
          }
          assert(caught10.getMessage === "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught10.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught11 = intercept[TestFailedException] {
            "abbc" should (not equal ("abbbc") and (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          }
          assert(caught11.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught11.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught12 = intercept[TestFailedException] {
            "abbc" should ((not equal ("abbbc")) and (not fullyMatch regex ("a(b*)c".r withGroup "bb")))
          }
          assert(caught12.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught12.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught13 = intercept[TestFailedException] {
            "abbc" should (not equal ("abbbc") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
          }
          assert(caught13.getMessage === "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" fully matched the regular expression a(b*)c and group bb")
          assert(caught13.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
          
          val caught14 = intercept[TestFailedException] {
            "abbcc" should (not equal ("abbccc") and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          }
          assert(caught14.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught14.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught15 = intercept[TestFailedException] {
            "abbcc" should ((not equal ("abbccc")) and (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))))
          }
          assert(caught15.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught15.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
  
          val caught16 = intercept[TestFailedException] {
            "abbcc" should (not equal ("abbccc") and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
          }
          assert(caught16.getMessage === "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc")
          assert(caught16.failedCodeFileName === Some("ShouldFullyMatchSpec.scala"))
          assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
        }
      }
    }
    
  }
}