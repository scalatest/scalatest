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

import SharedHelpers._
import FailureMessages._
import exceptions.TestFailedException
import org.scalactic.Prettifier
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers

class InspectorShorthandsRegexWithGroupsSpec extends AnyFunSpec with Matchers {

  private val prettifier = Prettifier.default

  def errorMessage(index: Int, message: String, lineNumber: Int, left: Any): String = 
    "'all' inspection failed, because: \n" +
    "  at index " + index + ", " + message + " (InspectorShorthandsRegexWithGroupsSpec.scala:" + lineNumber + ") \n" +
    "in " + decorateToStringValue(prettifier, left)
  
  describe("Inspector shorthands") {
    
    it("should work with fullyMatch regex withGroup and withGroups") {
      
      all(List("abbc")) should fullyMatch regex ("a(b*)c" withGroup "bb")
      all(List("abbcc")) should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbc")) should fullyMatch regex ("a(b*)c".r withGroup "bb")
      all(List("abbcc")) should fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not fullyMatch regex ("a(b*)c" withGroup "bb")
      all(List("abbbc")) should not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbc")) should not fullyMatch regex ("a(b*)c".r withGroup "bb")
      all(List("abbbc")) should not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbcc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c"))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c"))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should not fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should not fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbcc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with fullyMatch regex withGroup and withGroups when used with logical-and expression") {
      
      all(List("abbc")) should (fullyMatch regex ("a(b*)c" withGroup "bb") and fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbc")) should (fullyMatch regex ("a(b*)c".r withGroup "bb") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c" withGroup "bb") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c".r withGroup "bb") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") and fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbc")) should (equal ("abbc") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbcc")) should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbc") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbbc")) should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbbc")) should (not equal ("abbc") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbbc")) should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      // shouldNot does not support logical expression yet
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (fullyMatch regex ("a(b*)c" withGroup "bb") and fullyMatch regex ("a(b*)c" withGroup "bbb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbcc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (fullyMatch regex ("a(b*)c".r withGroup "bb") and fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not fullyMatch regex ("a(b*)c" withGroup "bb") and not fullyMatch regex ("a(b*)c" withGroup "bbb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not fullyMatch regex ("a(b*)c".r withGroup "bb") and not fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbcc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (fullyMatch regex ("a(b*)c" withGroup "bbb") and fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c and group bbb, but \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (fullyMatch regex ("a(b*)c".r withGroup "bbb") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c and group bbb, but \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not fullyMatch regex ("a(b*)c" withGroup "bbb") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbc")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not fullyMatch regex ("a(b*)c".r withGroup "bbb") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbcc")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) should (equal ("abbc") and fullyMatch regex ("a(b*)c" withGroup "bbb"))
      }
      assert(caught17.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\"", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbcc")
      val caught18 = intercept[TestFailedException] {
        all(list18) should (equal ("abbc") and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbc[c]\" did not equal \"abbc[]\"", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("abbbc")
      val caught19 = intercept[TestFailedException] {
        all(list19) should (equal ("abbc") and fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      }
      assert(caught19.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\"", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("abbcc")
      val caught20 = intercept[TestFailedException] {
        all(list20) should (equal ("abbc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught20.message === Some(errorMessage(0, "\"abbc[c]\" did not equal \"abbc[]\"", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) should (not equal ("abbc") and not fullyMatch regex ("a(b*)c" withGroup "bbb"))
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\"", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbcc")
      val caught22 = intercept[TestFailedException] {
        all(list22) should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\"", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("abbc")
      val caught23 = intercept[TestFailedException] {
        all(list23) should (not equal ("abbc") and not fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      }
      assert(caught23.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\"", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("abbcc")
      val caught24 = intercept[TestFailedException] {
        all(list24) should (not equal ("abbcc") and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      }
      assert(caught24.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\"", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list25 = List("abbbc")
      val caught25 = intercept[TestFailedException] {
        all(list25) should (equal ("abbbc") and fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught25.message === Some(errorMessage(0, "\"abbbc\" equaled \"abbbc\", but \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list25)))
      assert(caught25.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list26 = List("abbcc")
      val caught26 = intercept[TestFailedException] {
        all(list26) should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")))
      }
      assert(caught26.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list26)))
      assert(caught26.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list27 = List("abbbc")
      val caught27 = intercept[TestFailedException] {
        all(list27) should (equal ("abbbc") and fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught27.message === Some(errorMessage(0, "\"abbbc\" equaled \"abbbc\", but \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list27)))
      assert(caught27.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list28 = List("abbcc")
      val caught28 = intercept[TestFailedException] {
        all(list28) should (equal ("abbcc") and fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      }
      assert(caught28.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", but \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list28)))
      assert(caught28.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list29 = List("abbc")
      val caught29 = intercept[TestFailedException] {
        all(list29) should (not equal ("abbbc") and not fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught29.message === Some(errorMessage(0, "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list29)))
      assert(caught29.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list30 = List("abbcc")
      val caught30 = intercept[TestFailedException] {
        all(list30) should (not equal ("abbccc") and not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught30.message === Some(errorMessage(0, "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list30)))
      assert(caught30.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list31 = List("abbc")
      val caught31 = intercept[TestFailedException] {
        all(list31) should (not equal ("abbbc") and not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught31.message === Some(errorMessage(0, "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list31)))
      assert(caught31.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list32 = List("abbcc")
      val caught32 = intercept[TestFailedException] {
        all(list32) should (not equal ("abbccc") and not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught32.message === Some(errorMessage(0, "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list32)))
      assert(caught32.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      /*val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group \"bb\"", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group \"bb\"", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))*/
    }
    
    it("should work with fullyMatch regex withGroup and withGroups when used with logical-or expression") {
      
      all(List("abbc")) should (fullyMatch regex ("a(b*)c" withGroup "bb") or fullyMatch regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbc")) should (fullyMatch regex ("a(b*)c".r withGroup "bb") or fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      
      all(List("abbc")) should (fullyMatch regex ("a(b*)c" withGroup "bbb") or fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbc")) should (fullyMatch regex ("a(b*)c".r withGroup "bbb") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbcc")) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") or fullyMatch regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (equal ("abbcc") or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbc")) should (equal ("abbc") or fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbcc")) should (equal ("abbcc") or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      
      all(List("abbc")) should (equal ("abbbc") or fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbccc") or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbc")) should (equal ("abbbc") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbcc")) should (equal ("abbccc") or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c" withGroup "bb") or not fullyMatch regex ("a(b*)c" withGroup "bbb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c".r withGroup "bb") or not fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c" withGroup "bbb") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)c".r withGroup "bbb") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbbc")) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbc") or not fullyMatch regex ("a(b*)c" withGroup "bbb"))
      all(List("abbbc")) should (not equal ("abbcc") or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbbc")) should (not equal ("abbc") or not fullyMatch regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbbc")) should (not equal ("abbcc") or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not equal ("abbbc") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
      all(List("abbbc")) should (not equal ("abbccc") or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbbc")) should (not equal ("abbbc") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      all(List("abbbc")) should (not equal ("abbccc") or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      // shouldNot does not support logical expression yet
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      //all(List("abbbc")) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (fullyMatch regex ("a(b*)c" withGroup "bb") or fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbcc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")) or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (fullyMatch regex ("a(b*)c".r withGroup "bb") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")) or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1, and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not fullyMatch regex ("a(b*)c" withGroup "bb") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb, and \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, and \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not fullyMatch regex ("a(b*)c".r withGroup "bb") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group bb, and \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbcc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc, and \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (equal ("abbc") or fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\", and \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (equal ("abbc") or fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "c")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbc[c]\" did not equal \"abbc[]\", and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list2)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (equal ("abbc") or fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\", and \"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (equal ("abbc") or fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbc[c]\" did not equal \"abbc[]\", and \"abbcc\" fully matched the regular expression a(b*)(c*), but \"cc\" did not match group c at index 1", thisLineNumber - 2, list4)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not equal ("abbc") or not fullyMatch regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", and \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not equal ("abbcc") or not fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", and \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbc")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not equal ("abbc") or not fullyMatch regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", and \"abbc\" fully matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbcc")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not equal ("abbcc") or not fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", and \"abbcc\" fully matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      /*val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) shouldNot fullyMatch regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group \"bb\"", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) shouldNot fullyMatch regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) shouldNot fullyMatch regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbc\" fully matched the regular expression a(b*)c and group \"bb\"", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) shouldNot fullyMatch regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbcc\" fully matched the regular expression a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))*/
    }
    
    it("should work with startWith regex withGroup and withGroups") {
      
      all(List("abbc")) should startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcc")) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbcdef")) should startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccdef")) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbc")) should startWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbcc")) should startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbcdef")) should startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccdef")) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbcdef")) should not startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not startWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) should not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbbcdef")) should not startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbcdef")) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot startWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) shouldNot startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbbcdef")) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should startWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbcdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should not startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should not startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbccdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should not startWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should not startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught17.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list9)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbcc")
      val caught18 = intercept[TestFailedException] {
        all(list18) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list10)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("abbcdef")
      val caught19 = intercept[TestFailedException] {
        all(list19) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught19.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list11)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("abbccdef")
      val caught20 = intercept[TestFailedException] {
        all(list20) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught20.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list12)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) shouldNot startWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbcc")
      val caught22 = intercept[TestFailedException] {
        all(list22) shouldNot startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("abbcdef")
      val caught23 = intercept[TestFailedException] {
        all(list23) shouldNot startWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught23.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("abbccdef")
      val caught24 = intercept[TestFailedException] {
        all(list24) shouldNot startWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught24.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with startWith regex withGroup and withGroups when used with logical-and expression") {
      
      all(List("abbc")) should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbcdef")) should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbccdef")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not startWith regex ("a(b*)c".r withGroup "bb") and not startWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("abbbcdef")) should (not startWith regex ("a(b*)c" withGroup "bb") and not startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcccdef")) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") and startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbcc") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbcdef")) should (equal ("abbcdef") and startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbccdef")) should (equal ("abbccdef") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbc") and not startWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not equal ("abbcc") and not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("abbbcdef")) should (not equal ("abbcdef") and not startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcccdef")) should (not equal ("abbccdef") and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (startWith regex ("a(b*)c" withGroup "bb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not startWith regex ("a(b*)c" withGroup "bb") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not startWith regex ("a(b*)c" withGroup "bb") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbccdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (startWith regex ("a(b*)c" withGroup "bbb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbbc\" started with a substring that matched the regular expression a(b*)c and group bbb, but \"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "c")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, c, but \"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (startWith regex ("a(b*)c" withGroup "bbb") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbbcdef\" started with a substring that matched the regular expression a(b*)c and group bbb, but \"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "c")) and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, c, but \"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not startWith regex ("a(b*)c" withGroup "bbb") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not startWith regex ("a(b*)c" withGroup "bbb") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) should (equal ("abbc") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught17.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\"", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbc")
      val caught18 = intercept[TestFailedException] {
        all(list18) should (equal ("abbcc") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbc[]\" did not equal \"abbc[c]\"", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("abbbcdef")
      val caught19 = intercept[TestFailedException] {
        all(list19) should (equal ("abbcdef") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught19.message === Some(errorMessage(0, "\"abb[b]cdef\" did not equal \"abb[]cdef\"", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("abbcdef")
      val caught20 = intercept[TestFailedException] {
        all(list20) should (equal ("abbccdef") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught20.message === Some(errorMessage(0, "\"abbc[]def\" did not equal \"abbc[c]def\"", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) should (not equal ("abbc") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\"", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbcc")
      val caught22 = intercept[TestFailedException] {
        all(list22) should (not equal ("abbcc") and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\"", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("abbcdef")
      val caught23 = intercept[TestFailedException] {
        all(list23) should (not equal ("abbcdef") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught23.message === Some(errorMessage(0, "\"abbcdef\" equaled \"abbcdef\"", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("abbccdef")
      val caught24 = intercept[TestFailedException] {
        all(list24) should (not equal ("abbccdef") and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught24.message === Some(errorMessage(0, "\"abbccdef\" equaled \"abbccdef\"", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list25 = List("abbbc")
      val caught25 = intercept[TestFailedException] {
        all(list25) should (equal ("abbbc") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught25.message === Some(errorMessage(0, "\"abbbc\" equaled \"abbbc\", but \"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list25)))
      assert(caught25.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list26 = List("abbc")
      val caught26 = intercept[TestFailedException] {
        all(list26) should (equal ("abbc") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught26.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", but \"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list26)))
      assert(caught26.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list27 = List("abbbcdef")
      val caught27 = intercept[TestFailedException] {
        all(list27) should (equal ("abbbcdef") and startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught27.message === Some(errorMessage(0, "\"abbbcdef\" equaled \"abbbcdef\", but \"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list27)))
      assert(caught27.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list28 = List("abbcdef")
      val caught28 = intercept[TestFailedException] {
        all(list28) should (equal ("abbcdef") and startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught28.message === Some(errorMessage(0, "\"abbcdef\" equaled \"abbcdef\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list28)))
      assert(caught28.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list29 = List("abbc")
      val caught29 = intercept[TestFailedException] {
        all(list29) should (not equal ("abbbc") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught29.message === Some(errorMessage(0, "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list29)))
      assert(caught29.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list30 = List("abbcc")
      val caught30 = intercept[TestFailedException] {
        all(list30) should (not equal ("abbccc") and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught30.message === Some(errorMessage(0, "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list30)))
      assert(caught30.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list31 = List("abbcdef")
      val caught31 = intercept[TestFailedException] {
        all(list31) should (not equal ("abbbcdef") and not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught31.message === Some(errorMessage(0, "\"abb[]cdef\" did not equal \"abb[b]cdef\", but \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list31)))
      assert(caught31.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list32 = List("abbccdef")
      val caught32 = intercept[TestFailedException] {
        all(list32) should (not equal ("abbcccdef") and not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught32.message === Some(errorMessage(0, "\"abbcc[]def\" did not equal \"abbcc[c]def\", but \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list32)))
      assert(caught32.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with startWith regex withGroup and withGroups when used with logical-or expression") {
      
      all(List("abbc")) should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbcdef")) should (startWith regex ("a(b*)c" withGroup "bbb") or startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbccdef")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not startWith regex ("a(b*)c".r withGroup "bbb") or not startWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("abbbcdef")) should (not startWith regex ("a(b*)c" withGroup "bbb") or not startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcccdef")) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (startWith regex ("a(b*)c" withGroup "bb") or startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbcdef")) should (startWith regex ("a(b*)c" withGroup "bb") or startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbccdef")) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not startWith regex ("a(b*)c".r withGroup "bb") or not startWith regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbccc")) should (not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      all(List("abbbcdef")) should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcccdef")) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbc")) should (equal ("abbbc") or startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbccc") or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("abbcdef")) should (equal ("abbbcdef") or startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbccdef")) should (equal ("abbcccdef") or startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbbc") or not startWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not equal ("abbccc") or not startWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("abbbcdef")) should (not equal ("abbbcdef") or not startWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcccdef")) should (not equal ("abbcccdef") or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") or startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (equal ("abbcc") or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("abbcdef")) should (equal ("abbcdef") or startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbccdef")) should (equal ("abbccdef") or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not equal ("abbc") or not startWith regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbccc")) should (not equal ("abbcc") or not startWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      all(List("abbbcdef")) should (not equal ("abbcdef") or not startWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcccdef")) should (not equal ("abbccdef") or not startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (startWith regex ("a(b*)c" withGroup "bb") or startWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1, and \"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (startWith regex ("a(b*)c" withGroup "bb") or startWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1, and \"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb, and \"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc, and \"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not startWith regex ("a(b*)c" withGroup "bb") or not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb, and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbccdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc, and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (equal ("abbc") or startWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\", and \"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (equal ("abbcc") or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbc[]\" did not equal \"abbc[c]\", and \"abbc\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (equal ("abbcdef") or startWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"abb[b]cdef\" did not equal \"abb[]cdef\", and \"abbbcdef\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbcdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (equal ("abbccdef") or startWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbc[]def\" did not equal \"abbc[c]def\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not equal ("abbc") or not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", and \"abbc\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not equal ("abbcc") or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", and \"abbcc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not equal ("abbcdef") or not startWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbcdef\" equaled \"abbcdef\", and \"abbcdef\" started with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not equal ("abbccdef") or not startWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbccdef\" equaled \"abbccdef\", and \"abbccdef\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
    }

    it("should work with endWith regex withGroup and withGroups") {
      
      all(List("abbc")) should endWith regex ("a(b*)c" withGroup "bb")
      all(List("abbcc")) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbc")) should endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbcc")) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbc")) should endWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbcc")) should endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbc")) should endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbcc")) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not endWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbc")) should not endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not endWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) should not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbbc")) should not endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbc")) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot endWith regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) shouldNot endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbbc")) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("123abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some(errorMessage(0, "\"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("123abbc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should endWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("123abbbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught7.message === Some(errorMessage(0, "\"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("123abbc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should not endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("123abbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) should not endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("123abbcc")
      val caught12 = intercept[TestFailedException] {
        all(list12) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should not endWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("123abbc")
      val caught15 = intercept[TestFailedException] {
        all(list15) should not endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught15.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("123abbcc")
      val caught16 = intercept[TestFailedException] {
        all(list16) should not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught16.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught17.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbcc")
      val caught18 = intercept[TestFailedException] {
        all(list18) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("123abbc")
      val caught19 = intercept[TestFailedException] {
        all(list19) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught19.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("123abbcc")
      val caught20 = intercept[TestFailedException] {
        all(list20) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught20.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) shouldNot endWith regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbcc")
      val caught22 = intercept[TestFailedException] {
        all(list22) shouldNot endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("123abbc")
      val caught23 = intercept[TestFailedException] {
        all(list23) shouldNot endWith regex ("a(b*)c" withGroup "bb")
      }
      assert(caught23.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("123abbcc")
      val caught24 = intercept[TestFailedException] {
        all(list24) shouldNot endWith regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught24.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with endWith regex withGroup and withGroups when used with logical-and expression") {
      
      all(List("abbc")) should (endWith regex ("a(b*)c" withGroup "bb") and endWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbc")) should (endWith regex ("a(b*)c" withGroup "bb") and endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not endWith regex ("a(b*)c".r withGroup "bb") and not endWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("123abbbc")) should (not endWith regex ("a(b*)c" withGroup "bb") and not endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccc")) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") and endWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbcc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbc")) should (equal ("123abbc") and endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcc")) should (equal ("123abbcc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbc") and not endWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not equal ("abbcc") and not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("123abbbc")) should (not equal ("123abbc" withGroup "bb") and not endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccc")) should (not equal ("123abbcc" withGroups ("bb", "cc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (endWith regex ("a(b*)c" withGroup "bb") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("123abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (endWith regex ("a(b*)c" withGroup "bb") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("123abbc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not endWith regex ("a(b*)c" withGroup "bb") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("123abbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not endWith regex ("a(b*)c" withGroup "bb") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("123abbcc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (endWith regex ("a(b*)c" withGroup "bbb") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbbc\" ended with a substring that matched the regular expression a(b*)c and group bbb, but \"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "c")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, c, but \"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("123abbbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (endWith regex ("a(b*)c" withGroup "bbb") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"123abbbc\" ended with a substring that matched the regular expression a(b*)c and group bbb, but \"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("123abbc")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "c")) and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, c, but \"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not endWith regex ("a(b*)c" withGroup "bbb") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("123abbc")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not endWith regex ("a(b*)c" withGroup "bbb") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bbb, but \"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("123abbcc")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*), but \"cc\" did not match group ccc at index 1, but \"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) should (equal ("abbc") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught17.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\"", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbc")
      val caught18 = intercept[TestFailedException] {
        all(list18) should (equal ("abbcc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbc[]\" did not equal \"abbc[c]\"", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("123abbbc")
      val caught19 = intercept[TestFailedException] {
        all(list19) should (equal ("123abbc") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught19.message === Some(errorMessage(0, "\"123abb[b]c\" did not equal \"123abb[]c\"", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("123abbc")
      val caught20 = intercept[TestFailedException] {
        all(list20) should (equal ("123abbcc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught20.message === Some(errorMessage(0, "\"123abbc[]\" did not equal \"123abbc[c]\"", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) should (not equal ("abbc") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\"", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbcc")
      val caught22 = intercept[TestFailedException] {
        all(list22) should (not equal ("abbcc") and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\"", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("123abbc")
      val caught23 = intercept[TestFailedException] {
        all(list23) should (not equal ("123abbc") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught23.message === Some(errorMessage(0, "\"123abbc\" equaled \"123abbc\"", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("123abbcc")
      val caught24 = intercept[TestFailedException] {
        all(list24) should (not equal ("123abbcc") and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught24.message === Some(errorMessage(0, "\"123abbcc\" equaled \"123abbcc\"", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list25 = List("abbbc")
      val caught25 = intercept[TestFailedException] {
        all(list25) should (equal ("abbbc") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught25.message === Some(errorMessage(0, "\"abbbc\" equaled \"abbbc\", but \"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list25)))
      assert(caught25.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list26 = List("abbc")
      val caught26 = intercept[TestFailedException] {
        all(list26) should (equal ("abbc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught26.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", but \"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list26)))
      assert(caught26.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list27 = List("123abbbc")
      val caught27 = intercept[TestFailedException] {
        all(list27) should (equal ("123abbbc") and endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught27.message === Some(errorMessage(0, "\"123abbbc\" equaled \"123abbbc\", but \"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list27)))
      assert(caught27.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list28 = List("123abbc")
      val caught28 = intercept[TestFailedException] {
        all(list28) should (equal ("123abbc") and endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught28.message === Some(errorMessage(0, "\"123abbc\" equaled \"123abbc\", but \"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list28)))
      assert(caught28.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list29 = List("abbc")
      val caught29 = intercept[TestFailedException] {
        all(list29) should (not equal ("abbbc") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught29.message === Some(errorMessage(0, "\"abb[]c\" did not equal \"abb[b]c\", but \"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list29)))
      assert(caught29.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list30 = List("abbcc")
      val caught30 = intercept[TestFailedException] {
        all(list30) should (not equal ("abbccc") and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught30.message === Some(errorMessage(0, "\"abbcc[]\" did not equal \"abbcc[c]\", but \"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list30)))
      assert(caught30.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list31 = List("123abbc")
      val caught31 = intercept[TestFailedException] {
        all(list31) should (not equal ("123abbbc") and not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught31.message === Some(errorMessage(0, "\"123abb[]c\" did not equal \"123abb[b]c\", but \"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list31)))
      assert(caught31.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list32 = List("123abbcc")
      val caught32 = intercept[TestFailedException] {
        all(list32) should (not equal ("123abbccc") and not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught32.message === Some(errorMessage(0, "\"123abbcc[]\" did not equal \"123abbcc[c]\", but \"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list32)))
      assert(caught32.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with endWith regex withGroup and withGroups when used with logical-or expression") {
      
      all(List("abbc")) should (endWith regex ("a(b*)c" withGroup "bbb") or endWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbc")) should (endWith regex ("a(b*)c" withGroup "bbb") or endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not endWith regex ("a(b*)c".r withGroup "bbb") or not endWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not endWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")) or not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("123abbbc")) should (not endWith regex ("a(b*)c" withGroup "bbb") or not endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccc")) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")) or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (endWith regex ("a(b*)c" withGroup "bb") or endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("123abbc")) should (endWith regex ("a(b*)c" withGroup "bb") or endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("123abbcc")) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not endWith regex ("a(b*)c".r withGroup "bb") or not endWith regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbccc")) should (not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not endWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      all(List("123abbbc")) should (not endWith regex ("a(b*)c" withGroup "bb") or not endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("123abbccc")) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbc")) should (equal ("abbbc") or endWith regex ("a(b*)c" withGroup "bb"))
      all(List("abbcc")) should (equal ("abbccc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbc")) should (equal ("123abbbc") or endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcc")) should (equal ("123abbccc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbbc")) should (not equal ("abbbc") or not endWith regex ("a(b*)c".r withGroup "bb"))
      all(List("abbccc")) should (not equal ("abbccc") or not endWith regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      all(List("123abbbc")) should (not equal ("123abbbc") or not endWith regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccc")) should (not equal ("123abbccc") or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      
      all(List("abbc")) should (equal ("abbc") or endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("abbcc")) should (equal ("abbcc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      all(List("123abbc")) should (equal ("123abbc") or endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("123abbcc")) should (equal ("123abbcc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      all(List("abbbc")) should (not equal ("abbc") or not endWith regex ("a(b*)c".r withGroup "bbb"))
      all(List("abbccc")) should (not equal ("abbcc") or not endWith regex ("a(b*)(c*)".r withGroups ("bb", "ccc")))
      all(List("123abbbc")) should (not equal ("123abbc") or not endWith regex ("a(b*)c" withGroup "bbb"))
      all(List("123abbccc")) should (not equal ("123abbcc") or not endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (endWith regex ("a(b*)c" withGroup "bb") or endWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1, and \"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("123abbbc")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (endWith regex ("a(b*)c" withGroup "bb") or endWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb, and \"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("123abbc")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group cc at index 1, and \"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not endWith regex ("a(b*)c" withGroup "bb") or not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb, and \"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbcc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc, and \"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("123abbc")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not endWith regex ("a(b*)c" withGroup "bb") or not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb, and \"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("123abbcc")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc, and \"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (equal ("abbc") or endWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"abb[b]c\" did not equal \"abb[]c\", and \"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (equal ("abbcc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbc[]\" did not equal \"abbc[c]\", and \"abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("123abbbc")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (equal ("123abbc") or endWith regex ("a(b*)c" withGroup "b"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"123abb[b]c\" did not equal \"123abb[]c\", and \"123abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group b", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("123abbc")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (equal ("123abbcc") or endWith regex ("a(b*)(c*)" withGroups ("bb", "ccc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"123abbc[]\" did not equal \"123abbc[c]\", and \"123abbc\" ended with a substring that matched the regular expression a(b*)(c*), but \"c\" did not match group ccc at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not equal ("abbc") or not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" equaled \"abbc\", and \"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not equal ("abbcc") or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" equaled \"abbcc\", and \"abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("123abbc")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not equal ("123abbc") or not endWith regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"123abbc\" equaled \"123abbc\", and \"123abbc\" ended with a substring that matched the regular expression a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("123abbcc")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not equal ("123abbcc") or not endWith regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"123abbcc\" equaled \"123abbcc\", and \"123abbcc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
    
    it("should work with include regex withGroup and withGroups") {
      
      all(List("abbc")) should include regex ("a(b*)c" withGroup "bb")
      all(List("abbcc")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbcdef")) should include regex ("a(b*)c" withGroup "bb")
      all(List("abbccdef")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbc")) should include regex ("a(b*)c".r withGroup "bb")
      all(List("abbcc")) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbcdef")) should include regex ("a(b*)c" withGroup "bb")
      all(List("abbccdef")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbcdef")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not include regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbbcdef")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("abbbcdef")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot include regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("abbbcdef")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("abbcccdef")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbc")) should include regex ("a(b*)c" withGroup "bb")
      all(List("abbcc")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbc")) should include regex ("a(b*)c" withGroup "bb")
      all(List("123abbcc")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbc")) should include regex ("a(b*)c".r withGroup "bb")
      all(List("abbcc")) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbc")) should include regex ("a(b*)c" withGroup "bb")
      all(List("123abbcc")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbc")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) should not include regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbbc")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("abbccc")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbc")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("abbbc")) shouldNot include regex ("a(b*)c".r withGroup "bb")
      all(List("abbccc")) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      all(List("123abbbc")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("123abbccc")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      
      all(List("123abbcdef")) should include regex ("a(b*)c" withGroup "bb")
      all(List("123abbccdef")) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbcdef")) should include regex ("a(b*)c".r withGroup "bb")
      all(List("123abbccdef")) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      all(List("123abbbcdef")) should not include regex ("a(b*)c" withGroup "bb")
      all(List("123abbcccdef")) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbcdef")) should not include regex ("a(b*)c".r withGroup "bb")
      all(List("123abbcccdef")) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      all(List("123abbbcdef")) shouldNot include regex ("a(b*)c" withGroup "bb")
      all(List("123abbcccdef")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      all(List("123abbbcdef")) shouldNot include regex ("a(b*)c".r withGroup "bb")
      all(List("123abbcccdef")) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val list1 = List("abbbc")
      val caught1 = intercept[TestFailedException] {
        all(list1) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught1.message === Some(errorMessage(0, "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("abbc")
      val caught2 = intercept[TestFailedException] {
        all(list2) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught2.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught3.message === Some(errorMessage(0, "\"abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught4.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("abbbc")
      val caught5 = intercept[TestFailedException] {
        all(list5) should include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught5.message === Some(errorMessage(0, "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("abbc")
      val caught6 = intercept[TestFailedException] {
        all(list6) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught6.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("abbbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught7.message === Some(errorMessage(0, "\"abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("abbcdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught8.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("abbc")
      val caught9 = intercept[TestFailedException] {
        all(list9) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught9.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("abbcc")
      val caught10 = intercept[TestFailedException] {
        all(list10) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught10.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("abbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught11.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("abbccdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught12.message === Some(errorMessage(0, "\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("abbc")
      val caught13 = intercept[TestFailedException] {
        all(list13) should not include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught13.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("abbcc")
      val caught14 = intercept[TestFailedException] {
        all(list14) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught14.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught15.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught16.message === Some(errorMessage(0, "\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("abbbc")
      val caught17 = intercept[TestFailedException] {
        all(list17) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught17.message === Some(errorMessage(0, "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("abbc")
      val caught18 = intercept[TestFailedException] {
        all(list18) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught18.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("123abbbc")
      val caught19 = intercept[TestFailedException] {
        all(list19) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught19.message === Some(errorMessage(0, "\"123abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("123abbc")
      val caught20 = intercept[TestFailedException] {
        all(list20) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught20.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("abbbc")
      val caught21 = intercept[TestFailedException] {
        all(list21) should include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught21.message === Some(errorMessage(0, "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("abbc")
      val caught22 = intercept[TestFailedException] {
        all(list22) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught22.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("123abbbc")
      val caught23 = intercept[TestFailedException] {
        all(list23) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught23.message === Some(errorMessage(0, "\"123abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("123abbc")
      val caught24 = intercept[TestFailedException] {
        all(list24) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught24.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list25 = List("abbc")
      val caught25 = intercept[TestFailedException] {
        all(list25) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught25.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list25)))
      assert(caught25.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list26 = List("abbcc")
      val caught26 = intercept[TestFailedException] {
        all(list26) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught26.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list26)))
      assert(caught26.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list27 = List("123abbc")
      val caught27 = intercept[TestFailedException] {
        all(list27) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught27.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list27)))
      assert(caught27.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list28 = List("123abbcc")
      val caught28 = intercept[TestFailedException] {
        all(list28) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught28.message === Some(errorMessage(0, "\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list28)))
      assert(caught28.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list29 = List("abbc")
      val caught29 = intercept[TestFailedException] {
        all(list29) should not include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught29.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list29)))
      assert(caught29.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list30 = List("abbcc")
      val caught30 = intercept[TestFailedException] {
        all(list30) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught30.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list30)))
      assert(caught30.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list31 = List("123abbc")
      val caught31 = intercept[TestFailedException] {
        all(list31) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught31.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list31)))
      assert(caught31.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list32 = List("123abbcc")
      val caught32 = intercept[TestFailedException] {
        all(list32) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught32.message === Some(errorMessage(0, "\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list32)))
      assert(caught32.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list33 = List("123abbbcdef")
      val caught33 = intercept[TestFailedException] {
        all(list33) should include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught33.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list33)))
      assert(caught33.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught33.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list34 = List("123abbcdef")
      val caught34 = intercept[TestFailedException] {
        all(list34) should include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught34.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list34)))
      assert(caught34.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught34.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list35 = List("123abbbcdef")
      val caught35 = intercept[TestFailedException] {
        all(list35) should include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught35.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list35)))
      assert(caught35.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught35.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list36 = List("123abbcdef")
      val caught36 = intercept[TestFailedException] {
        all(list36) should include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught36.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list36)))
      assert(caught36.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught36.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list37 = List("123abbcdef")
      val caught37 = intercept[TestFailedException] {
        all(list37) should not include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught37.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list37)))
      assert(caught37.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught37.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list38 = List("123abbccdef")
      val caught38 = intercept[TestFailedException] {
        all(list38) should not include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught38.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list38)))
      assert(caught38.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught38.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list39 = List("123abbcdef")
      val caught39 = intercept[TestFailedException] {
        all(list39) should not include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught39.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list39)))
      assert(caught39.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught39.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list40 = List("123abbccdef")
      val caught40 = intercept[TestFailedException] {
        all(list40) should not include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught40.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list40)))
      assert(caught40.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught40.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list41 = List("abbc")
      val caught41 = intercept[TestFailedException] {
        all(list41) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught41.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list41)))
      assert(caught41.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught41.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list42 = List("abbcc")
      val caught42 = intercept[TestFailedException] {
        all(list42) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught42.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list42)))
      assert(caught42.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught42.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list43 = List("abbcdef")
      val caught43 = intercept[TestFailedException] {
        all(list43) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught43.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list43)))
      assert(caught43.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught43.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list44 = List("abbccdef")
      val caught44 = intercept[TestFailedException] {
        all(list44) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught44.message === Some(errorMessage(0, "\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list44)))
      assert(caught44.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught44.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list45 = List("abbc")
      val caught45 = intercept[TestFailedException] {
        all(list45) shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught45.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list45)))
      assert(caught45.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught45.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list46 = List("abbcc")
      val caught46 = intercept[TestFailedException] {
        all(list46) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught46.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list46)))
      assert(caught46.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught46.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list47 = List("abbcdef")
      val caught47 = intercept[TestFailedException] {
        all(list47) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught47.message === Some(errorMessage(0, "\"abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list47)))
      assert(caught47.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught47.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list48 = List("abbccdef")
      val caught48 = intercept[TestFailedException] {
        all(list48) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught48.message === Some(errorMessage(0, "\"abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list48)))
      assert(caught48.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught48.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list49 = List("abbc")
      val caught49 = intercept[TestFailedException] {
        all(list49) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught49.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list49)))
      assert(caught49.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught49.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list50 = List("abbcc")
      val caught50 = intercept[TestFailedException] {
        all(list50) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught50.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list50)))
      assert(caught50.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught50.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list51 = List("123abbc")
      val caught51 = intercept[TestFailedException] {
        all(list51) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught51.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list51)))
      assert(caught51.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught51.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list52 = List("123abbcc")
      val caught52 = intercept[TestFailedException] {
        all(list52) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught52.message === Some(errorMessage(0, "\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list52)))
      assert(caught52.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught52.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list53 = List("abbc")
      val caught53 = intercept[TestFailedException] {
        all(list53) shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught53.message === Some(errorMessage(0, "\"abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list53)))
      assert(caught53.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught53.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list54 = List("abbcc")
      val caught54 = intercept[TestFailedException] {
        all(list54) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught54.message === Some(errorMessage(0, "\"abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list54)))
      assert(caught54.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught54.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list55 = List("123abbc")
      val caught55 = intercept[TestFailedException] {
        all(list55) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught55.message === Some(errorMessage(0, "\"123abbc\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list55)))
      assert(caught55.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught55.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list56 = List("123abbcc")
      val caught56 = intercept[TestFailedException] {
        all(list56) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught56.message === Some(errorMessage(0, "\"123abbcc\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list56)))
      assert(caught56.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught56.failedCodeLineNumber === Some(thisLineNumber - 4))

      val list57 = List("123abbcdef")
      val caught57 = intercept[TestFailedException] {
        all(list57) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught57.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list57)))
      assert(caught57.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught57.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list58 = List("123abbccdef")
      val caught58 = intercept[TestFailedException] {
        all(list58) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught58.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list58)))
      assert(caught58.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught58.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list59 = List("123abbcdef")
      val caught59 = intercept[TestFailedException] {
        all(list59) shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught59.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list59)))
      assert(caught59.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught59.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list60 = List("123abbccdef")
      val caught60 = intercept[TestFailedException] {
        all(list60) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught60.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list60)))
      assert(caught60.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught60.failedCodeLineNumber === Some(thisLineNumber - 4))
      
    }
    
    it("should work with include regex withGroup and withGroups when used with logical-and expression") {
      
      all(List("123abbcdef")) should (include regex ("a(b*)c" withGroup "bb") and include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)" withGroups ("bb", "cc")) and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (include regex ("a(b*)c".r withGroup "bb") and include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not include regex ("a(b*)c" withGroup "bb") and not include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbbcdef")) should (not include regex ("a(b*)c".r withGroup "bb") and not include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbcdef")) should (equal ("123abbcdef") and include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbccdef") and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (equal ("123abbcdef") and include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbccdef") and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not equal ("123abbcdef") and not include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcccdef")) should (not equal ("123abbccdef") and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbbcdef")) should (not equal ("123abbcdef") and not include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbcccdef")) should (not equal ("123abbccdef") and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      // shouldNot does not support logical expression yet.
      //all(List("123abbbcdef")) shouldNot include regex ("a(b*)c" withGroup "bb")
      //all(List("123abbcccdef")) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      //all(List("123abbbcdef")) shouldNot include regex ("a(b*)c".r withGroup "bb")
      //all(List("123abbcccdef")) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      
      val list1 = List("123abbbcdef")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (include regex ("a(b*)c" withGroup "bb") and include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("123abbcdef")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (include regex ("a(b*)(c*)" withGroups ("bb", "cc")) and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("123abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (include regex ("a(b*)c".r withGroup "bb") and include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("123abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("123abbcdef")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not include regex ("a(b*)c" withGroup "bb") and not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("123abbccdef")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not include regex ("a(b*)(c*)" withGroups ("bb", "cc")) and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("123abbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not include regex ("a(b*)c".r withGroup "bb") and not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("123abbccdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("123abbbcdef")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (include regex ("a(b*)c" withGroup "bbb") and include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c and group bbb, but \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("123abbcdef")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (include regex ("a(b*)(c*)" withGroups ("bb", "c")) and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*) and group bb, c, but \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("123abbbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (include regex ("a(b*)c".r withGroup "bbb") and include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c and group bbb, but \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("123abbcdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (include regex ("a(b*)(c*)".r withGroups ("bb", "c")) and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*) and group bb, c, but \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("123abbcdef")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not include regex ("a(b*)c" withGroup "b") and not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c, but \"bb\" did not match group b, but \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("123abbccdef")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not include regex ("a(b*)(c*)" withGroups ("bb", "c")) and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*), but \"cc\" did not match group c at index 1, but \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("123abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not include regex ("a(b*)c".r withGroup "b") and not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c, but \"bb\" did not match group b, but \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("123abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "c")) and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*), but \"cc\" did not match group c at index 1, but \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list17 = List("123abbbcdef")
      val caught17 = intercept[TestFailedException] {
        all(list17) should (equal ("123abbcdef") and include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught17.message === Some(errorMessage(0, "\"123abb[b]cdef\" did not equal \"123abb[]cdef\"", thisLineNumber - 2, list17)))
      assert(caught17.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught17.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list18 = List("123abbcdef")
      val caught18 = intercept[TestFailedException] {
        all(list18) should (equal ("123abbccdef") and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught18.message === Some(errorMessage(0, "\"123abbc[]def\" did not equal \"123abbc[c]def\"", thisLineNumber - 2, list18)))
      assert(caught18.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught18.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list19 = List("123abbbcdef")
      val caught19 = intercept[TestFailedException] {
        all(list19) should (equal ("123abbcdef") and include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught19.message === Some(errorMessage(0, "\"123abb[b]cdef\" did not equal \"123abb[]cdef\"", thisLineNumber - 2, list19)))
      assert(caught19.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught19.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list20 = List("123abbcdef")
      val caught20 = intercept[TestFailedException] {
        all(list20) should (equal ("123abbccdef") and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught20.message === Some(errorMessage(0, "\"123abbc[]def\" did not equal \"123abbc[c]def\"", thisLineNumber - 2, list20)))
      assert(caught20.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught20.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list21 = List("123abbcdef")
      val caught21 = intercept[TestFailedException] {
        all(list21) should (not equal ("123abbcdef") and not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught21.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\"", thisLineNumber - 2, list21)))
      assert(caught21.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught21.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list22 = List("123abbccdef")
      val caught22 = intercept[TestFailedException] {
        all(list22) should (not equal ("123abbccdef") and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught22.message === Some(errorMessage(0, "\"123abbccdef\" equaled \"123abbccdef\"", thisLineNumber - 2, list22)))
      assert(caught22.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught22.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list23 = List("123abbcdef")
      val caught23 = intercept[TestFailedException] {
        all(list23) should (not equal ("123abbcdef") and not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught23.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\"", thisLineNumber - 2, list23)))
      assert(caught23.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught23.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list24 = List("123abbccdef")
      val caught24 = intercept[TestFailedException] {
        all(list24) should (not equal ("123abbccdef") and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught24.message === Some(errorMessage(0, "\"123abbccdef\" equaled \"123abbccdef\"", thisLineNumber - 2, list24)))
      assert(caught24.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught24.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list25 = List("123abbbcdef")
      val caught25 = intercept[TestFailedException] {
        all(list25) should (equal ("123abbbcdef") and include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught25.message === Some(errorMessage(0, "\"123abbbcdef\" equaled \"123abbbcdef\", but \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list25)))
      assert(caught25.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught25.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list26 = List("123abbcdef")
      val caught26 = intercept[TestFailedException] {
        all(list26) should (equal ("123abbcdef") and include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught26.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\", but \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list26)))
      assert(caught26.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught26.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list27 = List("123abbbcdef")
      val caught27 = intercept[TestFailedException] {
        all(list27) should (equal ("123abbbcdef") and include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught27.message === Some(errorMessage(0, "\"123abbbcdef\" equaled \"123abbbcdef\", but \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list27)))
      assert(caught27.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught27.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list28 = List("123abbcdef")
      val caught28 = intercept[TestFailedException] {
        all(list28) should (equal ("123abbcdef") and include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught28.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\", but \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list28)))
      assert(caught28.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught28.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list29 = List("123abbcdef")
      val caught29 = intercept[TestFailedException] {
        all(list29) should (not equal ("123abcdef") and not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught29.message === Some(errorMessage(0, "\"123ab[b]cdef\" did not equal \"123ab[]cdef\", but \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list29)))
      assert(caught29.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught29.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list30 = List("123abbccdef")
      val caught30 = intercept[TestFailedException] {
        all(list30) should (not equal ("123abbcdef") and not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught30.message === Some(errorMessage(0, "\"123abbc[c]def\" did not equal \"123abbc[]def\", but \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list30)))
      assert(caught30.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught30.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list31 = List("123abbcdef")
      val caught31 = intercept[TestFailedException] {
        all(list31) should (not equal ("123abcdef") and not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught31.message === Some(errorMessage(0, "\"123ab[b]cdef\" did not equal \"123ab[]cdef\", but \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list31)))
      assert(caught31.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught31.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list32 = List("123abbccdef")
      val caught32 = intercept[TestFailedException] {
        all(list32) should (not equal ("123abbcdef") and not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught32.message === Some(errorMessage(0, "\"123abbc[c]def\" did not equal \"123abbc[]def\", but \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list32)))
      assert(caught32.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught32.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      
      /*
      val list57 = List("123abbcdef")
      val caught57 = intercept[TestFailedException] {
        all(list57) shouldNot include regex ("a(b*)c" withGroup "bb")
      }
      assert(caught57.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group \"bb\"", thisLineNumber - 2, list57)))
      assert(caught57.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught57.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list58 = List("123abbccdef")
      val caught58 = intercept[TestFailedException] {
        all(list58) shouldNot include regex ("a(b*)(c*)" withGroups ("bb", "cc"))
      }
      assert(caught58.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list58)))
      assert(caught58.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught58.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list59 = List("123abbcdef")
      val caught59 = intercept[TestFailedException] {
        all(list59) shouldNot include regex ("a(b*)c".r withGroup "bb")
      }
      assert(caught59.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group \"bb\"", thisLineNumber - 2, list59)))
      assert(caught59.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught59.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list60 = List("123abbccdef")
      val caught60 = intercept[TestFailedException] {
        all(list60) shouldNot include regex ("a(b*)(c*)".r withGroups ("bb", "cc"))
      }
      assert(caught60.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group \"bb\", \"cc\"", thisLineNumber - 2, list60)))
      assert(caught60.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught60.failedCodeLineNumber === Some(thisLineNumber - 4))*/
      
    }
    
    it("should work with include regex withGroup and withGroups when used with logical-or expression") {
      
      all(List("123abbcdef")) should (include regex ("a(b*)c" withGroup "b") or include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)" withGroups ("bb", "c")) or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (include regex ("a(b*)c".r withGroup "b") or include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)".r withGroups ("bb", "c")) or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not include regex ("a(b*)c" withGroup "b") or not include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)" withGroups ("bb", "c")) or not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbbcdef")) should (not include regex ("a(b*)c".r withGroup "b") or not include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "c")) or not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbcdef")) should (include regex ("a(b*)c" withGroup "bb") or include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)" withGroups ("bb", "cc")) or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (include regex ("a(b*)c".r withGroup "bb") or include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not include regex ("a(b*)c" withGroup "bb") or not include regex ("a(b*)c" withGroup "b"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not include regex ("a(b*)(c*)" withGroups ("bb", "c")))
      all(List("123abbbcdef")) should (not include regex ("a(b*)c".r withGroup "bb") or not include regex ("a(b*)c".r withGroup "b"))
      all(List("123abbcccdef")) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not include regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      
      all(List("123abbcdef")) should (equal ("123abcdef") or include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbcdef") or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (equal ("123abcdef") or include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbcdef") or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not equal ("123abcdef") or not include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbcccdef")) should (not equal ("123abbcdef") or not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbbcdef")) should (not equal ("123abcdef") or not include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbcccdef")) should (not equal ("123abbcdef") or not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbcdef")) should (equal ("123abbcdef") or include regex ("a(b*)c" withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbccdef") or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      all(List("123abbcdef")) should (equal ("123abbcdef") or include regex ("a(b*)c".r withGroup "bb"))
      all(List("123abbccdef")) should (equal ("123abbccdef") or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      
      all(List("123abbbcdef")) should (not equal ("123abbcdef") or not include regex ("a(b*)c" withGroup "b"))
      all(List("123abbcccdef")) should (not equal ("123abbccdef") or not include regex ("a(b*)(c*)" withGroups ("bb", "c")))
      all(List("123abbbcdef")) should (not equal ("123abbcdef") or not include regex ("a(b*)c".r withGroup "b"))
      all(List("123abbcccdef")) should (not equal ("123abbccdef") or not include regex ("a(b*)(c*)".r withGroups ("bb", "c")))
      
      val list1 = List("123abbbcdef")
      val caught1 = intercept[TestFailedException] {
        all(list1) should (include regex ("a(b*)c" withGroup "bb") or include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught1.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb, and \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list1)))
      assert(caught1.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught1.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list2 = List("123abbcdef")
      val caught2 = intercept[TestFailedException] {
        all(list2) should (include regex ("a(b*)(c*)" withGroups ("bb", "cc")) or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught2.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1, and \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list2)))
      assert(caught2.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught2.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list3 = List("123abbbcdef")
      val caught3 = intercept[TestFailedException] {
        all(list3) should (include regex ("a(b*)c".r withGroup "bb") or include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught3.message === Some(errorMessage(0, "\"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb, and \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list3)))
      assert(caught3.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught3.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list4 = List("123abbcdef")
      val caught4 = intercept[TestFailedException] {
        all(list4) should (include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught4.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1, and \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list4)))
      assert(caught4.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught4.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list5 = List("123abbcdef")
      val caught5 = intercept[TestFailedException] {
        all(list5) should (not include regex ("a(b*)c" withGroup "bb") or not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught5.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb, and \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list5)))
      assert(caught5.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught5.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list6 = List("123abbccdef")
      val caught6 = intercept[TestFailedException] {
        all(list6) should (not include regex ("a(b*)(c*)" withGroups ("bb", "cc")) or not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught6.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc, and \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list6)))
      assert(caught6.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught6.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list7 = List("123abbcdef")
      val caught7 = intercept[TestFailedException] {
        all(list7) should (not include regex ("a(b*)c".r withGroup "bb") or not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught7.message === Some(errorMessage(0, "\"123abbcdef\" included substring that matched regex a(b*)c and group bb, and \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list7)))
      assert(caught7.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught7.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list8 = List("123abbccdef")
      val caught8 = intercept[TestFailedException] {
        all(list8) should (not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")) or not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught8.message === Some(errorMessage(0, "\"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc, and \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list8)))
      assert(caught8.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught8.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list9 = List("123abbbcdef")
      val caught9 = intercept[TestFailedException] {
        all(list9) should (equal ("123abbcdef") or include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught9.message === Some(errorMessage(0, "\"123abb[b]cdef\" did not equal \"123abb[]cdef\", and \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list9)))
      assert(caught9.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught9.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list10 = List("123abbcdef")
      val caught10 = intercept[TestFailedException] {
        all(list10) should (equal ("123abbccdef") or include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught10.message === Some(errorMessage(0, "\"123abbc[]def\" did not equal \"123abbc[c]def\", and \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list10)))
      assert(caught10.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught10.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list11 = List("123abbbcdef")
      val caught11 = intercept[TestFailedException] {
        all(list11) should (equal ("123abbcdef") or include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught11.message === Some(errorMessage(0, "\"123abb[b]cdef\" did not equal \"123abb[]cdef\", and \"123abbbcdef\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb", thisLineNumber - 2, list11)))
      assert(caught11.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught11.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list12 = List("123abbcdef")
      val caught12 = intercept[TestFailedException] {
        all(list12) should (equal ("123abbccdef") or include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught12.message === Some(errorMessage(0, "\"123abbc[]def\" did not equal \"123abbc[c]def\", and \"123abbcdef\" included substring that matched regex a(b*)(c*), but \"c\" did not match group cc at index 1", thisLineNumber - 2, list12)))
      assert(caught12.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught12.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list13 = List("123abbcdef")
      val caught13 = intercept[TestFailedException] {
        all(list13) should (not equal ("123abbcdef") or not include regex ("a(b*)c" withGroup "bb"))
      }
      assert(caught13.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\", and \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list13)))
      assert(caught13.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught13.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list14 = List("123abbccdef")
      val caught14 = intercept[TestFailedException] {
        all(list14) should (not equal ("123abbccdef") or not include regex ("a(b*)(c*)" withGroups ("bb", "cc")))
      }
      assert(caught14.message === Some(errorMessage(0, "\"123abbccdef\" equaled \"123abbccdef\", and \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list14)))
      assert(caught14.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught14.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list15 = List("123abbcdef")
      val caught15 = intercept[TestFailedException] {
        all(list15) should (not equal ("123abbcdef") or not include regex ("a(b*)c".r withGroup "bb"))
      }
      assert(caught15.message === Some(errorMessage(0, "\"123abbcdef\" equaled \"123abbcdef\", and \"123abbcdef\" included substring that matched regex a(b*)c and group bb", thisLineNumber - 2, list15)))
      assert(caught15.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught15.failedCodeLineNumber === Some(thisLineNumber - 4))
      
      val list16 = List("123abbccdef")
      val caught16 = intercept[TestFailedException] {
        all(list16) should (not equal ("123abbccdef") or not include regex ("a(b*)(c*)".r withGroups ("bb", "cc")))
      }
      assert(caught16.message === Some(errorMessage(0, "\"123abbccdef\" equaled \"123abbccdef\", and \"123abbccdef\" included substring that matched regex a(b*)(c*) and group bb, cc", thisLineNumber - 2, list16)))
      assert(caught16.failedCodeFileName === Some("InspectorShorthandsRegexWithGroupsSpec.scala"))
      assert(caught16.failedCodeLineNumber === Some(thisLineNumber - 4))
    }
  }
  
}
