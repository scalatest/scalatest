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
package org.scalatest.words

import org.scalatest._
import SharedHelpers.createTempDirectory
import Matchers._
import matchers.{BePropertyMatcher, 
                 BePropertyMatchResult, 
                 AMatcher, 
                 AnMatcher, 
                 BeMatcher, 
                 MatchResult}
import matchers.{NegatedFailureMessage, 
                 MidSentenceFailureMessage, 
                 MidSentenceNegatedFailureMessage}
// SKIP-SCALATESTJS-START
import java.io.File
// SKIP-SCALATESTJS-END
import FailureMessages.decorateToStringValue
import org.scalatest.exceptions.NotAllowedException
import org.scalactic.Prettifier
import org.scalactic.source.SourceInfo

class NotWordSpec extends FunSpec with FileMocks {
  
  describe("NotWord ") {
    
    it("should have pretty toString") {
      not.toString should be ("not")
    }
    
    describe("apply(Matcher) method returns Matcher") {
      
      val mt = not (be < 3)
      
      it("should have pretty toString") {
        mt.toString should be ("not (be < 3)")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "0 was less than 3"
        mr.negatedFailureMessage shouldBe "0 was not less than 3"
        mr.midSentenceFailureMessage shouldBe "0 was less than 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was not less than 3"
        mr.rawFailureMessage shouldBe "{0} was less than {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not less than {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was less than {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not less than {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "0 was not less than 3"
        nmr.negatedFailureMessage shouldBe "0 was less than 3"
        nmr.midSentenceFailureMessage shouldBe "0 was not less than 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was less than 3"
        nmr.rawFailureMessage shouldBe "{0} was not less than {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was less than {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not less than {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was less than {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("apply(MatcherFactory1) method returns MatcherFactory1") {
      
      val mtf = not (equal (3))
      val mt = mtf.matcher[Int]
      
      it("should have pretty toString") {
        mt.toString should be ("not (equal (3))")
      }
      
      val mr = mt(3)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "3 equaled 3"
        mr.negatedFailureMessage shouldBe "3 did not equal 3"
        mr.midSentenceFailureMessage shouldBe "3 equaled 3"
        mr.midSentenceNegatedFailureMessage shouldBe "3 did not equal 3"
        mr.rawFailureMessage shouldBe "{0} equaled {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not equal {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} equaled {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal {1}"
        mr.failureMessageArgs shouldBe Vector(3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(3, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "3 did not equal 3"
        nmr.negatedFailureMessage shouldBe "3 equaled 3"
        nmr.midSentenceFailureMessage shouldBe "3 did not equal 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "3 equaled 3"
        nmr.rawFailureMessage shouldBe "{0} did not equal {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} equaled {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not equal {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} equaled {1}"
        nmr.failureMessageArgs shouldBe Vector(3, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(3, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3)

      }
    }
    
    describe("apply(MatcherFactory2) method returns MatcherFactory2") {
      
      val mtf1 = equal (3)
      val mt1 = mtf1.matcher[Int]
      val mtf2 = not equal ("3")
      val mt2 = mtf2.matcher[Int]
      val matcherFactory2 = mtf1 and mtf2
      val mtf = not (matcherFactory2)
      val mt = mtf.matcher[Int]
      
      it("should have pretty toString") {
        mt.toString should be ("not (" + matcherFactory2 + ")")
      }
      
      val mr = mt(3)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "3 equaled 3, and 3 did not equal \"3\""
        mr.negatedFailureMessage shouldBe "3 equaled 3, but 3 equaled \"3\""
        mr.midSentenceFailureMessage shouldBe "3 equaled 3, and 3 did not equal \"3\""
        mr.midSentenceNegatedFailureMessage shouldBe "3 equaled 3, but 3 equaled \"3\""
        mr.rawFailureMessage shouldBe "{0}, and {1}"
        mr.rawNegatedFailureMessage shouldBe "{0}, but {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0}, and {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0}, but {1}"
        mr.failureMessageArgs shouldBe Vector(NegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))
        mr.negatedFailureMessageArgs shouldBe Vector(NegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))
        mr.midSentenceFailureMessageArgs shouldBe Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "3 equaled 3, but 3 equaled \"3\""
        nmr.negatedFailureMessage shouldBe "3 equaled 3, and 3 did not equal \"3\""
        nmr.midSentenceFailureMessage shouldBe "3 equaled 3, but 3 equaled \"3\""
        nmr.midSentenceNegatedFailureMessage shouldBe "3 equaled 3, and 3 did not equal \"3\""
        nmr.rawFailureMessage shouldBe "{0}, but {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0}, and {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0}, but {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0}, and {1}"
        nmr.failureMessageArgs shouldBe Vector(NegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))
        nmr.negatedFailureMessageArgs shouldBe Vector(NegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))

      }
    }
    
    describe("apply(BeMatcher) method returns Matcher") {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = not (odd)
      
      it("should have pretty toString") {
        mt.toString should be ("not (" + odd + ")")
      }
      
      val mr = mt(1)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "1 was odd"
        mr.negatedFailureMessage shouldBe "1 was even"
        mr.midSentenceFailureMessage shouldBe "1 was odd"
        mr.midSentenceNegatedFailureMessage shouldBe "1 was even"
        mr.rawFailureMessage shouldBe "1 was odd"
        mr.rawNegatedFailureMessage shouldBe "1 was even"
        mr.rawMidSentenceFailureMessage shouldBe "1 was odd"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "1 was even"
        mr.failureMessageArgs shouldBe Vector(1)
        mr.negatedFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "1 was even"
        nmr.negatedFailureMessage shouldBe "1 was odd"
        nmr.midSentenceFailureMessage shouldBe "1 was even"
        nmr.midSentenceNegatedFailureMessage shouldBe "1 was odd"
        nmr.rawFailureMessage shouldBe "1 was even"
        nmr.rawNegatedFailureMessage shouldBe "1 was odd"
        nmr.rawMidSentenceFailureMessage shouldBe "1 was even"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "1 was odd"
        nmr.failureMessageArgs shouldBe Vector(1)
        nmr.negatedFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
    }
    
    describe("equal(Any) method returns MatcherFactory1") {
      
      val mtf = not equal (3)
      val mt = mtf.matcher[Int]
      
      it("should have pretty toString") {
        mt.toString should be ("not (equal (3))")
      }
      
      val mr = mt(3)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "3 equaled 3"
        mr.negatedFailureMessage shouldBe "3 did not equal 3"
        mr.midSentenceFailureMessage shouldBe "3 equaled 3"
        mr.midSentenceNegatedFailureMessage shouldBe "3 did not equal 3"
        mr.rawFailureMessage shouldBe "{0} equaled {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not equal {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} equaled {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal {1}"
        mr.failureMessageArgs shouldBe Vector(3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(3, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "3 did not equal 3"
        nmr.negatedFailureMessage shouldBe "3 equaled 3"
        nmr.midSentenceFailureMessage shouldBe "3 did not equal 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "3 equaled 3"
        nmr.rawFailureMessage shouldBe "{0} did not equal {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} equaled {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not equal {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} equaled {1}"
        nmr.failureMessageArgs shouldBe Vector(3, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(3, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3)

      }
    }
    
    describe("equal(Spread) method returns MatcherFactory1") {
      
      val mt = not equal (3 +- 1)
      
      it("should have pretty toString") {
        mt.toString should be ("not equal 3 +- 1")
      }
      
      val mr = mt(3)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "3 equaled 3 plus or minus 1"
        mr.negatedFailureMessage shouldBe "3 did not equal 3 plus or minus 1"
        mr.midSentenceFailureMessage shouldBe "3 equaled 3 plus or minus 1"
        mr.midSentenceNegatedFailureMessage shouldBe "3 did not equal 3 plus or minus 1"
        mr.rawFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        mr.failureMessageArgs shouldBe Vector(3, 3, 1)
        mr.negatedFailureMessageArgs shouldBe Vector(3, 3, 1)
        mr.midSentenceFailureMessageArgs shouldBe Vector(3, 3, 1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3, 1)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "3 did not equal 3 plus or minus 1"
        nmr.negatedFailureMessage shouldBe "3 equaled 3 plus or minus 1"
        nmr.midSentenceFailureMessage shouldBe "3 did not equal 3 plus or minus 1"
        nmr.midSentenceNegatedFailureMessage shouldBe "3 equaled 3 plus or minus 1"
        nmr.rawFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        nmr.failureMessageArgs shouldBe Vector(3, 3, 1)
        nmr.negatedFailureMessageArgs shouldBe Vector(3, 3, 1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(3, 3, 1)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(3, 3, 1)

      }
    }

    // SKIP-SCALATESTJS-START
    describe("val exists of type MatcherFactory1") {
      
      val tempDir = createTempDirectory()
      val lhs = File.createTempFile("delete", "me", tempDir)
      
      val existVal = not.exist
      val mt = existVal.matcher[File]
      
      it("should have pretty toString") {
        mt.toString should be ("not exist")
      }
      
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " exists"
        mr.negatedFailureMessage shouldBe lhs + " does not exist"
        mr.midSentenceFailureMessage shouldBe lhs + " exists"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " does not exist"
        mr.rawFailureMessage shouldBe "{0} exists"
        mr.rawNegatedFailureMessage shouldBe "{0} does not exist"
        mr.rawMidSentenceFailureMessage shouldBe "{0} exists"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} does not exist"
        mr.failureMessageArgs shouldBe Vector(lhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " does not exist"
        nmr.negatedFailureMessage shouldBe lhs + " exists"
        nmr.midSentenceFailureMessage shouldBe lhs + " does not exist"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " exists"
        nmr.rawFailureMessage shouldBe "{0} does not exist"
        nmr.rawNegatedFailureMessage shouldBe "{0} exists"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} does not exist"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} exists"
        nmr.failureMessageArgs shouldBe Vector(lhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs)

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("equal(null) method returns MatcherFactory1") {
      
      val mt = not equal (null)
      
      it("should have pretty toString") {
        mt.toString should be ("not equal null")
      }
      
      val mr = mt("Bob")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "The reference equaled null"
        mr.negatedFailureMessage shouldBe "\"Bob\" did not equal null"
        mr.midSentenceFailureMessage shouldBe "the reference equaled null"
        mr.midSentenceNegatedFailureMessage shouldBe "\"Bob\" did not equal null"
        mr.rawFailureMessage shouldBe "The reference equaled null"
        mr.rawNegatedFailureMessage shouldBe "{0} did not equal null"
        mr.rawMidSentenceFailureMessage shouldBe "the reference equaled null"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal null"
        mr.failureMessageArgs shouldBe Vector.empty
        mr.negatedFailureMessageArgs shouldBe Vector("Bob")
        mr.midSentenceFailureMessageArgs shouldBe Vector.empty
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("Bob")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "\"Bob\" did not equal null"
        nmr.negatedFailureMessage shouldBe "The reference equaled null"
        nmr.midSentenceFailureMessage shouldBe "\"Bob\" did not equal null"
        nmr.midSentenceNegatedFailureMessage shouldBe "the reference equaled null"
        nmr.rawFailureMessage shouldBe "{0} did not equal null"
        nmr.rawNegatedFailureMessage shouldBe "The reference equaled null"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not equal null"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "the reference equaled null"
        nmr.failureMessageArgs shouldBe Vector("Bob")
        nmr.negatedFailureMessageArgs shouldBe Vector.empty
        nmr.midSentenceFailureMessageArgs shouldBe Vector("Bob")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector.empty

      }
    }
    
    describe("be(BeMatcher) method returns Matcher") {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = not be (odd)
      
      it("should have pretty toString") {
        mt.toString should be ("not be " + odd)
      }
      
      val mr = mt(1)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "1 was odd"
        mr.negatedFailureMessage shouldBe "1 was even"
        mr.midSentenceFailureMessage shouldBe "1 was odd"
        mr.midSentenceNegatedFailureMessage shouldBe "1 was even"
        mr.rawFailureMessage shouldBe "1 was odd"
        mr.rawNegatedFailureMessage shouldBe "1 was even"
        mr.rawMidSentenceFailureMessage shouldBe "1 was odd"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "1 was even"
        mr.failureMessageArgs shouldBe Vector(1)
        mr.negatedFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceFailureMessageArgs shouldBe Vector(1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "1 was even"
        nmr.negatedFailureMessage shouldBe "1 was odd"
        nmr.midSentenceFailureMessage shouldBe "1 was even"
        nmr.midSentenceNegatedFailureMessage shouldBe "1 was odd"
        nmr.rawFailureMessage shouldBe "1 was even"
        nmr.rawNegatedFailureMessage shouldBe "1 was odd"
        nmr.rawMidSentenceFailureMessage shouldBe "1 was even"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "1 was odd"
        nmr.failureMessageArgs shouldBe Vector(1)
        nmr.negatedFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(1)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(1)

      }
    }
    
    describe("be(null) method returns MatcherFactory1") {
      
      val mt = not be (null)
      
      it("should have pretty toString") {
        mt.toString should be ("not be null")
      }
      
      val mr = mt("Bob")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "The reference was null"
        mr.negatedFailureMessage shouldBe "\"Bob\" was not null"
        mr.midSentenceFailureMessage shouldBe "the reference was null"
        mr.midSentenceNegatedFailureMessage shouldBe "\"Bob\" was not null"
        mr.rawFailureMessage shouldBe "The reference was null"
        mr.rawNegatedFailureMessage shouldBe "{0} was not null"
        mr.rawMidSentenceFailureMessage shouldBe "the reference was null"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not null"
        mr.failureMessageArgs shouldBe Vector.empty
        mr.negatedFailureMessageArgs shouldBe Vector("Bob")
        mr.midSentenceFailureMessageArgs shouldBe Vector.empty
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("Bob")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "\"Bob\" was not null"
        nmr.negatedFailureMessage shouldBe "The reference was null"
        nmr.midSentenceFailureMessage shouldBe "\"Bob\" was not null"
        nmr.midSentenceNegatedFailureMessage shouldBe "the reference was null"
        nmr.rawFailureMessage shouldBe "{0} was not null"
        nmr.rawNegatedFailureMessage shouldBe "The reference was null"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not null"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "the reference was null"
        nmr.failureMessageArgs shouldBe Vector("Bob")
        nmr.negatedFailureMessageArgs shouldBe Vector.empty
        nmr.midSentenceFailureMessageArgs shouldBe Vector("Bob")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector.empty

      }
    }
    
    describe("be(ResultOfLessThanComparison) method returns Matcher") {
      
      val mt = not be < (3)
      
      it("should have pretty toString") {
        mt.toString should be ("not be < (3)")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "0 was less than 3"
        mr.negatedFailureMessage shouldBe "0 was not less than 3"
        mr.midSentenceFailureMessage shouldBe "0 was less than 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was not less than 3"
        mr.rawFailureMessage shouldBe "{0} was less than {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not less than {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was less than {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not less than {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "0 was not less than 3"
        nmr.negatedFailureMessage shouldBe "0 was less than 3"
        nmr.midSentenceFailureMessage shouldBe "0 was not less than 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was less than 3"
        nmr.rawFailureMessage shouldBe "{0} was not less than {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was less than {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not less than {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was less than {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("be(ResultOfGreaterThanComparison) method returns Matcher") {
      
      val mt = not be > (3)
      
      it("should have pretty toString") {
        mt.toString should be ("not be > (3)")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "0 was greater than 3"
        mr.negatedFailureMessage shouldBe "0 was not greater than 3"
        mr.midSentenceFailureMessage shouldBe "0 was greater than 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was not greater than 3"
        mr.rawFailureMessage shouldBe "{0} was greater than {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not greater than {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was greater than {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not greater than {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "0 was not greater than 3"
        nmr.negatedFailureMessage shouldBe "0 was greater than 3"
        nmr.midSentenceFailureMessage shouldBe "0 was not greater than 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was greater than 3"
        nmr.rawFailureMessage shouldBe "{0} was not greater than {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was greater than {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not greater than {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was greater than {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)
        
      }
    }
    
    describe("be(ResultOfLessThanOrEqualToComparison) method returns Matcher") {
      
      val mt = not be <= (3)
      
      it("should have pretty toString") {
        mt.toString should be ("not be <= (3)")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "0 was less than or equal to 3"
        mr.negatedFailureMessage shouldBe "0 was not less than or equal to 3"
        mr.midSentenceFailureMessage shouldBe "0 was less than or equal to 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was not less than or equal to 3"
        mr.rawFailureMessage shouldBe "{0} was less than or equal to {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not less than or equal to {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was less than or equal to {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not less than or equal to {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "0 was not less than or equal to 3"
        nmr.negatedFailureMessage shouldBe "0 was less than or equal to 3"
        nmr.midSentenceFailureMessage shouldBe "0 was not less than or equal to 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was less than or equal to 3"
        nmr.rawFailureMessage shouldBe "{0} was not less than or equal to {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was less than or equal to {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not less than or equal to {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was less than or equal to {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("be(ResultOfGreaterThanOrEqualToComparison) method returns Matcher") {
      
      val mt = not be >= (3)
      
      it("should have pretty toString") {
        mt.toString should be ("not be >= (3)")
      }
      
      val mr = mt(0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "0 was greater than or equal to 3"
        mr.negatedFailureMessage shouldBe "0 was not greater than or equal to 3"
        mr.midSentenceFailureMessage shouldBe "0 was greater than or equal to 3"
        mr.midSentenceNegatedFailureMessage shouldBe "0 was not greater than or equal to 3"
        mr.rawFailureMessage shouldBe "{0} was greater than or equal to {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was greater than or equal to {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        mr.failureMessageArgs shouldBe Vector(0, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "0 was not greater than or equal to 3"
        nmr.negatedFailureMessage shouldBe "0 was greater than or equal to 3"
        nmr.midSentenceFailureMessage shouldBe "0 was not greater than or equal to 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "0 was greater than or equal to 3"
        nmr.rawFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was greater than or equal to {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not greater than or equal to {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was greater than or equal to {1}"
        nmr.failureMessageArgs shouldBe Vector(0, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(0, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(0, 3)

      }
    }
    
    describe("be(TripleEqualsInvocation) method fails") {
      intercept[NotAllowedException] { val mt = be === "cheese" }
    }

    // SKIP-SCALATESTJS-START
    describe("be(Symbol) method returns Matcher") {
      val mt = not be ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be 'file")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe fileMock + " was file"
        mr.negatedFailureMessage shouldBe fileMock + " was not file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not file"
        mr.rawFailureMessage shouldBe "{0} was {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe fileMock + " was not file"
        nmr.negatedFailureMessage shouldBe fileMock + " was file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was not file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was file"
        nmr.rawFailureMessage shouldBe "{0} was not {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("be(BePropertyMatcher) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be " + file)
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was file"
        mr.negatedFailureMessage shouldBe myFile + " was not file"
        mr.midSentenceFailureMessage shouldBe myFile + " was file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not file"
        mr.rawFailureMessage shouldBe "{0} was {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not file"
        nmr.negatedFailureMessage shouldBe myFile + " was file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was file"
        nmr.rawFailureMessage shouldBe "{0} was not {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }

    // SKIP-SCALATESTJS-START
    describe("apply(ResultOfAWordToSymbolApplication) method returns Matcher") {
      val mt = not be a ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be a ('file)")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe fileMock + " was a file"
        mr.negatedFailureMessage shouldBe fileMock + " was not a file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was a file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not a file"
        mr.rawFailureMessage shouldBe "{0} was a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe fileMock + " was not a file"
        nmr.negatedFailureMessage shouldBe fileMock + " was a file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was not a file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was a file"
        nmr.rawFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("be(ResultOfAWordToBePropertyMatcherApplication) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be a " + file)
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was a file"
        mr.negatedFailureMessage shouldBe myFile + " was not a file"
        mr.midSentenceFailureMessage shouldBe myFile + " was a file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not a file"
        mr.rawFailureMessage shouldBe "{0} was a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not a file"
        nmr.negatedFailureMessage shouldBe myFile + " was a file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not a file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was a file"
        nmr.rawFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("be(ResultOfAWordToAMatcherApplication) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not be a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be a (AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean))")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was a file"
        mr.negatedFailureMessage shouldBe myFile + " was not a file"
        mr.midSentenceFailureMessage shouldBe myFile + " was a file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not a file"
        mr.rawFailureMessage shouldBe "{0} was a {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was a {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not a {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not a file"
        nmr.negatedFailureMessage shouldBe myFile + " was a file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not a file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was a file"
        nmr.rawFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was a {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }

    // SKIP-SCALATESTJS-START
    describe("be(ResultOfAnWordToSymbolApplication) method returns Matcher") {
      val mt = not be an ('file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be an ('file)")
      }
      
      val mr = mt(fileMock)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe fileMock + " was an file"
        mr.negatedFailureMessage shouldBe fileMock + " was not an file"
        mr.midSentenceFailureMessage shouldBe fileMock + " was an file"
        mr.midSentenceNegatedFailureMessage shouldBe fileMock + " was not an file"
        mr.rawFailureMessage shouldBe "{0} was an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe fileMock + " was not an file"
        nmr.negatedFailureMessage shouldBe fileMock + " was an file"
        nmr.midSentenceFailureMessage shouldBe fileMock + " was not an file"
        nmr.midSentenceNegatedFailureMessage shouldBe fileMock + " was an file"
        nmr.rawFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.failureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fileMock, UnquotedString("file"))

      }
    }
    // SKIP-SCALATESTJS-END
    
    describe("be(ResultOfAnWordToBePropertyMatcherApplication) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be an (" + file + ")")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was an file"
        mr.negatedFailureMessage shouldBe myFile + " was not an file"
        mr.midSentenceFailureMessage shouldBe myFile + " was an file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an file"
        mr.rawFailureMessage shouldBe "{0} was an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not an file"
        nmr.negatedFailureMessage shouldBe myFile + " was an file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not an file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was an file"
        nmr.rawFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("be(ResultOfAnWordToAnMatcherApplication) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not be an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not be an (AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean))")
      }
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was an file"
        mr.negatedFailureMessage shouldBe myFile + " was not an file"
        mr.midSentenceFailureMessage shouldBe myFile + " was an file"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an file"
        mr.rawFailureMessage shouldBe "{0} was an {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was an {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an {1}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not an file"
        nmr.negatedFailureMessage shouldBe myFile + " was an file"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not an file"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was an file"
        nmr.rawFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString("file"))

      }
    }
    
    describe("be(ResultOfTheSameInstanceAsApplication) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val myFileLeft = MyFile("left", true, false)
      val myFileRight = MyFile("right", true, false)
      
      val mt = not be theSameInstanceAs (myFileRight)
      
      it("should have pretty toString") {
        mt.toString should be ("not be theSameInstanceAs (" + myFileRight + ")")
      }
      
      val mr = mt(myFileLeft)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        mr.negatedFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        mr.midSentenceFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        mr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        mr.rawFailureMessage shouldBe "{0} was the same instance as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not the same instance as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was the same instance as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not the same instance as {1}"
        mr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        nmr.negatedFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        nmr.midSentenceFailureMessage shouldBe myFileLeft + " was not the same instance as " + myFileRight
        nmr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was the same instance as " + myFileRight
        nmr.rawFailureMessage shouldBe "{0} was not the same instance as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was the same instance as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not the same instance as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was the same instance as {1}"
        nmr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

      }
    }
    
    describe("be(Spread) method returns Matcher") {
      val spread = 7.1 +- 0.2
      val mt = not be (spread)
      
      it("should have pretty toString") {
        mt.toString should be ("not be 7.1 +- 0.2")
      }
      
      val mr = mt(7.0)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        mr.negatedFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        mr.midSentenceFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        mr.midSentenceNegatedFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        mr.rawFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        mr.failureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.negatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.midSentenceFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        nmr.negatedFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        nmr.midSentenceFailureMessage shouldBe 7.0 + " was not 7.1 plus or minus 0.2"
        nmr.midSentenceNegatedFailureMessage shouldBe 7.0 + " was 7.1 plus or minus 0.2"
        nmr.rawFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not {1} plus or minus {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was {1} plus or minus {2}"
        nmr.failureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.negatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(7.0, 7.1, 0.2)

      }
    }
    
    describe("be(ResultOfDefinedAt) method returns Matcher") {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val mt = not be definedAt (8)
      
      it("should have pretty toString") {
        mt.toString should be ("not be definedAt (8)")
      }
      
      val mr = mt(fraction)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe fraction + " was defined at 8"
        mr.negatedFailureMessage shouldBe fraction + " was not defined at 8"
        mr.midSentenceFailureMessage shouldBe fraction + " was defined at 8"
        mr.midSentenceNegatedFailureMessage shouldBe fraction + " was not defined at 8"
        mr.rawFailureMessage shouldBe "{0} was defined at {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was defined at {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not defined at {1}"
        mr.failureMessageArgs shouldBe Vector(fraction, 8)
        mr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe fraction + " was not defined at 8"
        nmr.negatedFailureMessage shouldBe fraction + " was defined at 8"
        nmr.midSentenceFailureMessage shouldBe fraction + " was not defined at 8"
        nmr.midSentenceNegatedFailureMessage shouldBe fraction + " was defined at 8"
        nmr.rawFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was defined at {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not defined at {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was defined at {1}"
        nmr.failureMessageArgs shouldBe Vector(fraction, 8)
        nmr.negatedFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(fraction, 8)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(fraction, 8)

      }
    }
    
    describe("be(Any) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      val myFileRight = MyFile("test right", true, false)
      
      val mt = not be (myFileRight)
      
      it("should have pretty toString") {
        mt.toString should be ("not be " + myFileRight)
      }
      
      describe("when left is not null") {
      
        val myFileLeft = MyFile("test left", true, false)
        val mr = mt(myFileLeft)
      
        it("should have correct MatcherResult") {
          mr.matches shouldBe true
          mr.failureMessage shouldBe myFileLeft + " was equal to " + myFileRight
          mr.negatedFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
          mr.midSentenceFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
          mr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
          mr.rawFailureMessage shouldBe "{0} was equal to {1}"
          mr.rawNegatedFailureMessage shouldBe "{0} was not equal to {1}"
          mr.rawMidSentenceFailureMessage shouldBe "{0} was equal to {1}"
          mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not equal to {1}"
          mr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          mr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          mr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe false
          nmr.failureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
          nmr.negatedFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
          nmr.midSentenceFailureMessage shouldBe myFileLeft + " was not equal to " + myFileRight
          nmr.midSentenceNegatedFailureMessage shouldBe myFileLeft + " was equal to " + myFileRight
          nmr.rawFailureMessage shouldBe "{0} was not equal to {1}"
          nmr.rawNegatedFailureMessage shouldBe "{0} was equal to {1}"
          nmr.rawMidSentenceFailureMessage shouldBe "{0} was not equal to {1}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was equal to {1}"
          nmr.failureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          nmr.negatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          nmr.midSentenceFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileLeft, myFileRight)

        }
      }
      
      describe("when left is null") {
      
        val myFileLeft: MyFile = null
        val mr = mt(myFileLeft)
      
        it("should have correct MatcherResult") {
          mr.matches shouldBe true
          mr.failureMessage shouldBe "The reference was null"
          mr.negatedFailureMessage shouldBe myFileRight + " was not null"
          mr.midSentenceFailureMessage shouldBe "the reference was null"
          mr.midSentenceNegatedFailureMessage shouldBe myFileRight + " was not null"
          mr.rawFailureMessage shouldBe "The reference was null"
          mr.rawNegatedFailureMessage shouldBe "{0} was not null"
          mr.rawMidSentenceFailureMessage shouldBe "the reference was null"
          mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not null"
          mr.failureMessageArgs shouldBe Vector.empty
          mr.negatedFailureMessageArgs shouldBe Vector(myFileRight)
          mr.midSentenceFailureMessageArgs shouldBe Vector.empty
          mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFileRight)

        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe false
          nmr.failureMessage shouldBe myFileRight + " was not null"
          nmr.negatedFailureMessage shouldBe "The reference was null"
          nmr.midSentenceFailureMessage shouldBe myFileRight + " was not null"
          nmr.midSentenceNegatedFailureMessage shouldBe "the reference was null"
          nmr.rawFailureMessage shouldBe "{0} was not null"
          nmr.rawNegatedFailureMessage shouldBe "The reference was null"
          nmr.rawMidSentenceFailureMessage shouldBe "{0} was not null"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "the reference was null"
          nmr.failureMessageArgs shouldBe Vector(myFileRight)
          nmr.negatedFailureMessageArgs shouldBe Vector.empty
          nmr.midSentenceFailureMessageArgs shouldBe Vector(myFileRight)
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector.empty

        }
      }
    }
    
    describe("be(SortedWord) method returns MatcherFactory") {
      
      val mtf = not be (sorted)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not (be (sorted))")
        mt.toString should be ("not (be (sorted))")
      }
      
      val leftList = List(1, 2, 3)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe leftList + " was sorted"
        mr.negatedFailureMessage shouldBe leftList + " was not sorted"
        mr.midSentenceFailureMessage shouldBe leftList + " was sorted"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " was not sorted"
        mr.rawFailureMessage shouldBe "{0} was sorted"
        mr.rawNegatedFailureMessage shouldBe "{0} was not sorted"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was sorted"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not sorted"
        mr.failureMessageArgs shouldBe Vector(leftList)
        mr.negatedFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe leftList + " was not sorted"
        nmr.negatedFailureMessage shouldBe leftList + " was sorted"
        nmr.midSentenceFailureMessage shouldBe leftList + " was not sorted"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " was sorted"
        nmr.rawFailureMessage shouldBe "{0} was not sorted"
        nmr.rawNegatedFailureMessage shouldBe "{0} was sorted"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not sorted"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was sorted"
        nmr.failureMessageArgs shouldBe Vector(leftList)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
    }
    
    describe("be(ReadableWord) method returns MatcherFactory") {
      
      class MyFile {
        def isReadable: Boolean = true
      }
      
      val mtf = not be (readable)
      val mt = mtf.matcher[MyFile]
      
      it("should have pretty toString") {
        mtf.toString should be ("not (be (readable))")
        mt.toString should be ("not (be (readable))")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was readable"
        mr.negatedFailureMessage shouldBe myFile + " was not readable"
        mr.midSentenceFailureMessage shouldBe myFile + " was readable"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not readable"
        mr.rawFailureMessage shouldBe "{0} was readable"
        mr.rawNegatedFailureMessage shouldBe "{0} was not readable"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was readable"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not readable"
        mr.failureMessageArgs shouldBe Vector(myFile)
        mr.negatedFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not readable"
        nmr.negatedFailureMessage shouldBe myFile + " was readable"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not readable"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was readable"
        nmr.rawFailureMessage shouldBe "{0} was not readable"
        nmr.rawNegatedFailureMessage shouldBe "{0} was readable"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not readable"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was readable"
        nmr.failureMessageArgs shouldBe Vector(myFile)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
    }
    
    describe("be(WritableWord) method returns MatcherFactory") {
      
      class MyFile {
        def isWritable: Boolean = true
      }
      
      val mtf = not be (writable)
      val mt = mtf.matcher[MyFile]
      
      it("should have pretty toString") {
        mtf.toString should be ("not (be (writable))")
        mt.toString should be ("not (be (writable))")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was writable"
        mr.negatedFailureMessage shouldBe myFile + " was not writable"
        mr.midSentenceFailureMessage shouldBe myFile + " was writable"
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not writable"
        mr.rawFailureMessage shouldBe "{0} was writable"
        mr.rawNegatedFailureMessage shouldBe "{0} was not writable"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was writable"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not writable"
        mr.failureMessageArgs shouldBe Vector(myFile)
        mr.negatedFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not writable"
        nmr.negatedFailureMessage shouldBe myFile + " was writable"
        nmr.midSentenceFailureMessage shouldBe myFile + " was not writable"
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was writable"
        nmr.rawFailureMessage shouldBe "{0} was not writable"
        nmr.rawNegatedFailureMessage shouldBe "{0} was writable"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not writable"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was writable"
        nmr.failureMessageArgs shouldBe Vector(myFile)
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile)

      }
    }
    
    describe("be(EmptyWord) method returns MatcherFactory") {
      
      val mtf = not be (empty)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not (be (empty))")
        mt.toString should be ("not (be (empty))")
      }
      
      val leftList = List.empty[Int]
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe leftList + " was empty"
        mr.negatedFailureMessage shouldBe leftList + " was not empty"
        mr.midSentenceFailureMessage shouldBe leftList + " was empty"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " was not empty"
        mr.rawFailureMessage shouldBe "{0} was empty"
        mr.rawNegatedFailureMessage shouldBe "{0} was not empty"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was empty"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not empty"
        mr.failureMessageArgs shouldBe Vector(leftList)
        mr.negatedFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe leftList + " was not empty"
        nmr.negatedFailureMessage shouldBe leftList + " was empty"
        nmr.midSentenceFailureMessage shouldBe leftList + " was not empty"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " was empty"
        nmr.rawFailureMessage shouldBe "{0} was not empty"
        nmr.rawNegatedFailureMessage shouldBe "{0} was empty"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not empty"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was empty"
        nmr.failureMessageArgs shouldBe Vector(leftList)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList)

      }
    }
    
    describe("be(DefinedWord) method returns MatcherFactory") {
      
      val mtf = not be (defined)
      val mt = mtf.matcher[Option[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not (be (defined))")
        mt.toString should be ("not (be (defined))")
      }
      
      val leftOption = Some(1)
      val mr = mt(leftOption)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe leftOption + " was defined"
        mr.negatedFailureMessage shouldBe leftOption + " was not defined"
        mr.midSentenceFailureMessage shouldBe leftOption + " was defined"
        mr.midSentenceNegatedFailureMessage shouldBe leftOption + " was not defined"
        mr.rawFailureMessage shouldBe "{0} was defined"
        mr.rawNegatedFailureMessage shouldBe "{0} was not defined"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was defined"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not defined"
        mr.failureMessageArgs shouldBe Vector(leftOption)
        mr.negatedFailureMessageArgs shouldBe Vector(leftOption)
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftOption)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftOption)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe leftOption + " was not defined"
        nmr.negatedFailureMessage shouldBe leftOption + " was defined"
        nmr.midSentenceFailureMessage shouldBe leftOption + " was not defined"
        nmr.midSentenceNegatedFailureMessage shouldBe leftOption + " was defined"
        nmr.rawFailureMessage shouldBe "{0} was not defined"
        nmr.rawNegatedFailureMessage shouldBe "{0} was defined"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not defined"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was defined"
        nmr.failureMessageArgs shouldBe Vector(leftOption)
        nmr.negatedFailureMessageArgs shouldBe Vector(leftOption)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftOption)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftOption)

      }
    }
    
    describe("be(ResultOfATypeInvocation) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAType = new ResultOfATypeInvocation(clazz, Prettifier.default, SourceInfo.sourceInfo)
      
      val mt = not be (resultOfAType)
      
      it("should have pretty toString") {
        mt.toString should be ("not be a [" + clazz.getName + "]")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.negatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.midSentenceFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.rawFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.negatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.midSentenceFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.rawFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))

      }
    }
    
    describe("be(ResultOfAnTypeInvocation) method returns Matcher") {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAnType = new ResultOfAnTypeInvocation(clazz, Prettifier.default, SourceInfo.sourceInfo)
      
      val mt = not be (resultOfAnType)
      
      it("should have pretty toString") {
        mt.toString should be ("not be an [" + clazz.getName + "]")
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.negatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.midSentenceFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        mr.midSentenceNegatedFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        mr.rawFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} was an instance of {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        mr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        mr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.negatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.midSentenceFailureMessage shouldBe myFile + " was not an instance of " + clazz.getName + ", but an instance of " + myFile.getClass.getName
        nmr.midSentenceNegatedFailureMessage shouldBe myFile + " was an instance of " + clazz.getName
        nmr.rawFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} was not an instance of {1}, but an instance of {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} was an instance of {1}"
        nmr.failureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.negatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName), UnquotedString(myFile.getClass.getName))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(myFile, UnquotedString(clazz.getName))

      }
    }
    
    describe("fullyMatch(ResultOfRegexWordApplication) method returns Matcher") {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not fullyMatch regex (decimal)
      
      it("should have pretty toString") {
        mt.toString should be ("not fullyMatch regex (\"" + decimal + "\")")
      }
      
      val mr = mt("2.7")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"2.7\" fully matched the regular expression " + decimal
        mr.negatedFailureMessage shouldBe "\"2.7\" did not fully match the regular expression " + decimal
        mr.midSentenceFailureMessage shouldBe "\"2.7\" fully matched the regular expression " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"2.7\" did not fully match the regular expression " + decimal
        mr.rawFailureMessage shouldBe "{0} fully matched the regular expression {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not fully match the regular expression {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} fully matched the regular expression {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not fully match the regular expression {1}"
        mr.failureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"2.7\" did not fully match the regular expression " + decimal
        nmr.negatedFailureMessage shouldBe "\"2.7\" fully matched the regular expression " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"2.7\" did not fully match the regular expression " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"2.7\" fully matched the regular expression " + decimal
        nmr.rawFailureMessage shouldBe "{0} did not fully match the regular expression {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} fully matched the regular expression {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not fully match the regular expression {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} fully matched the regular expression {1}"
        nmr.failureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7", UnquotedString(decimal))

      }
      
    }
    
    describe("include(ResultOfRegexWordApplication) method returns Matcher") {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not include regex (decimal)
      
      it("should have pretty toString") {
        mt.toString should be ("not include regex (\"" + decimal + "\")")
      }
      
      val mr = mt("b2.7")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.negatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.rawFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.negatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.rawFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
    }
    
    describe("include(String) method returns Matcher") {
      
      val decimal = "2.7"
      val mt = not include (decimal)
      
      it("should have pretty toString") {
        mt.toString should be ("not include \"2.7\"")
      }
      
      val mr = mt("b2.7")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"b2.7\" included substring \"2.7\""
        mr.negatedFailureMessage shouldBe "\"b2.7\" did not include substring \"2.7\""
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" included substring \"2.7\""
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not include substring \"2.7\""
        mr.rawFailureMessage shouldBe "{0} included substring {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not include substring {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} included substring {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"b2.7\" did not include substring \"2.7\""
        nmr.negatedFailureMessage shouldBe "\"b2.7\" included substring \"2.7\""
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" did not include substring \"2.7\""
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" included substring \"2.7\""
        nmr.rawFailureMessage shouldBe "{0} did not include substring {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} included substring {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not include substring {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")

      }
    }
    
    describe("startWith(ResultOfRegexWordApplication) method returns Matcher") {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not startWith regex (decimal)
      
      it("should have pretty toString") {
        mt.toString should be ("not startWith regex (\"" + decimal + "\")")
      }
      
      val mr = mt("2.7b")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"2.7b\" started with a substring that matched the regular expression " + decimal
        mr.negatedFailureMessage shouldBe "\"2.7b\" did not start with a substring that matched the regular expression " + decimal
        mr.midSentenceFailureMessage shouldBe "\"2.7b\" started with a substring that matched the regular expression " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"2.7b\" did not start with a substring that matched the regular expression " + decimal
        mr.rawFailureMessage shouldBe "{0} started with a substring that matched the regular expression {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not start with a substring that matched the regular expression {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} started with a substring that matched the regular expression {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not start with a substring that matched the regular expression {1}"
        mr.failureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"2.7b\" did not start with a substring that matched the regular expression " + decimal
        nmr.negatedFailureMessage shouldBe "\"2.7b\" started with a substring that matched the regular expression " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"2.7b\" did not start with a substring that matched the regular expression " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"2.7b\" started with a substring that matched the regular expression " + decimal
        nmr.rawFailureMessage shouldBe "{0} did not start with a substring that matched the regular expression {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} started with a substring that matched the regular expression {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not start with a substring that matched the regular expression {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} started with a substring that matched the regular expression {1}"
        nmr.failureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7b", UnquotedString(decimal))

      }
    }
    
    describe("startWith(String) method returns Matcher") {
      
      val mt = not startWith ("2.7")
      
      it("should have pretty toString") {
        mt.toString should be ("not startWith \"2.7\"")
      }
      
      val mr = mt("2.7b")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"2.7b\" started with substring \"2.7\""
        mr.negatedFailureMessage shouldBe "\"2.7b\" did not start with substring \"2.7\""
        mr.midSentenceFailureMessage shouldBe "\"2.7b\" started with substring \"2.7\""
        mr.midSentenceNegatedFailureMessage shouldBe "\"2.7b\" did not start with substring \"2.7\""
        mr.rawFailureMessage shouldBe "{0} started with substring {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not start with substring {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} started with substring {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not start with substring {1}"
        mr.failureMessageArgs shouldBe Vector("2.7b", "2.7")
        mr.negatedFailureMessageArgs shouldBe Vector("2.7b", "2.7")
        mr.midSentenceFailureMessageArgs shouldBe Vector("2.7b", "2.7")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7b", "2.7")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"2.7b\" did not start with substring \"2.7\""
        nmr.negatedFailureMessage shouldBe "\"2.7b\" started with substring \"2.7\""
        nmr.midSentenceFailureMessage shouldBe "\"2.7b\" did not start with substring \"2.7\""
        nmr.midSentenceNegatedFailureMessage shouldBe "\"2.7b\" started with substring \"2.7\""
        nmr.rawFailureMessage shouldBe "{0} did not start with substring {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} started with substring {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not start with substring {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} started with substring {1}"
        nmr.failureMessageArgs shouldBe Vector("2.7b", "2.7")
        nmr.negatedFailureMessageArgs shouldBe Vector("2.7b", "2.7")
        nmr.midSentenceFailureMessageArgs shouldBe Vector("2.7b", "2.7")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("2.7b", "2.7")

      }
    }
    
    describe("endWith(ResultOfRegexWordApplication) method returns Matcher") {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not endWith regex (decimal)
      
      it("should have pretty toString") {
        mt.toString should be ("not endWith regex (\"" + decimal + "\")")
      }
      
      val mr = mt("b2.7")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"b2.7\" ended with a substring that matched the regular expression " + decimal
        mr.negatedFailureMessage shouldBe "\"b2.7\" did not end with a substring that matched the regular expression " + decimal
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" ended with a substring that matched the regular expression " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not end with a substring that matched the regular expression " + decimal
        mr.rawFailureMessage shouldBe "{0} ended with a substring that matched the regular expression {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not end with a substring that matched the regular expression {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} ended with a substring that matched the regular expression {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not end with a substring that matched the regular expression {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"b2.7\" did not end with a substring that matched the regular expression " + decimal
        nmr.negatedFailureMessage shouldBe "\"b2.7\" ended with a substring that matched the regular expression " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" did not end with a substring that matched the regular expression " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" ended with a substring that matched the regular expression " + decimal
        nmr.rawFailureMessage shouldBe "{0} did not end with a substring that matched the regular expression {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} ended with a substring that matched the regular expression {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not end with a substring that matched the regular expression {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} ended with a substring that matched the regular expression {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
    }
    
    describe("endWith(String) method returns Matcher") {
      
      val mt = not endWith ("2.7")
      
      it("should have pretty toString") {
        mt.toString should be ("not endWith \"2.7\"")
      }
      
      val mr = mt("b2.7")
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"b2.7\" ended with substring \"2.7\""
        mr.negatedFailureMessage shouldBe "\"b2.7\" did not end with substring \"2.7\""
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" ended with substring \"2.7\""
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not end with substring \"2.7\""
        mr.rawFailureMessage shouldBe "{0} ended with substring {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not end with substring {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} ended with substring {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not end with substring {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"b2.7\" did not end with substring \"2.7\""
        nmr.negatedFailureMessage shouldBe "\"b2.7\" ended with substring \"2.7\""
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" did not end with substring \"2.7\""
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" ended with substring \"2.7\""
        nmr.rawFailureMessage shouldBe "{0} did not end with substring {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} ended with substring {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not end with substring {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} ended with substring {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", "2.7")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", "2.7")

      }
    }
    
    describe("contain(T) method returns MatcherFactory1") {
      
      val mtf = not contain (2)
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain 2")
        mt.toString should be ("not contain 2")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "Array(1, 2, 3) contained element 2"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        mr.rawFailureMessage shouldBe "{0} contained element {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain element {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained element {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain element {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 2)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 2)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 2)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 2)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) did not contain element 2"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) contained element 2"
        nmr.rawFailureMessage shouldBe "{0} did not contain element {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained element {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain element {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained element {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 2)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 2)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 2)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 2)

      }
    }
    
    describe("contain(ResultOfOneOfApplication) method returns MatcherFactory1") {
      
      val mtf = not contain oneOf (2, 8)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain oneOf (2, 8)")
        mt.toString should be ("not contain oneOf (2, 8)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        mr.negatedFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        mr.midSentenceFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        mr.midSentenceNegatedFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        mr.rawFailureMessage shouldBe Resources.rawContainedOneOfElements
        mr.rawNegatedFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        mr.rawMidSentenceFailureMessage shouldBe Resources.rawContainedOneOfElements
        mr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.negatedFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.midSentenceFailureMessage shouldBe FailureMessages.didNotContainOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.midSentenceNegatedFailureMessage shouldBe FailureMessages.containedOneOfElements(lhs, UnquotedString("2, 8"))
        nmr.rawFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        nmr.rawNegatedFailureMessage shouldBe Resources.rawContainedOneOfElements
        nmr.rawMidSentenceFailureMessage shouldBe Resources.rawDidNotContainOneOfElements
        nmr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawContainedOneOfElements
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("2, 8"))

      }
    }
    
    describe("contain(ResultOfAtLeastOneOfApplication) method returns MatcherFactory1") {
      
      val mtf = not contain atLeastOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain atLeastOneOf (1, 2)")
        mt.toString should be ("not contain atLeastOneOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained at least one of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} contained at least one of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained at least one of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain at least one of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained at least one of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained at least one of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain at least one of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained at least one of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfNoneOfApplication) method returns MatcherFactory1") {
      
      val mtf = not contain noneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain noneOf (1, 2)")
        mt.toString should be ("not contain noneOf (1, 2)")
      }
      
      val lhs = List(7, 8, 9)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        mr.rawFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        mr.rawNegatedFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        mr.rawMidSentenceFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        mr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessage shouldBe FailureMessages.containedAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessage shouldBe FailureMessages.didNotContainAtLeastOneOf(lhs, UnquotedString("1, 2"))
        nmr.rawFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        nmr.rawNegatedFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        nmr.rawMidSentenceFailureMessage shouldBe Resources.rawContainedAtLeastOneOf
        nmr.rawMidSentenceNegatedFailureMessage shouldBe Resources.rawDidNotContainAtLeastOneOf
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfTheSameElementsAsApplication) method returns MatcherFactory1") {
      
      val rhs = List(1, 2, 3)
      val mtf = not contain theSameElementsAs (rhs)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain theSameElementsAs (List(1, 2, 3))")
        mt.toString should be ("not contain theSameElementsAs (List(1, 2, 3))")
      }
      
      val lhs = List(3, 2, 1)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained the same elements as " + rhs
        mr.negatedFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        mr.midSentenceFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        mr.rawFailureMessage shouldBe "{0} contained the same elements as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained the same elements as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, rhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        nmr.negatedFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain the same elements as " + rhs
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained the same elements as " + rhs
        nmr.rawFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained the same elements as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain the same elements as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained the same elements as {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
    }
    
    describe("contain(ResultOfTheSameElementsInOrderAsApplication) method returns MatcherFactory1") {
      
      val rhs = List(1, 2, 3)
      val mtf = not contain theSameElementsInOrderAs (rhs)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain theSameElementsInOrderAs (List(1, 2, 3))")
        mt.toString should be ("not contain theSameElementsInOrderAs (List(1, 2, 3))")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        mr.negatedFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        mr.midSentenceFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        mr.rawFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, rhs)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        nmr.negatedFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain the same elements in the same (iterated) order as " + rhs
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained the same elements in the same (iterated) order as " + rhs
        nmr.rawFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain the same elements in the same (iterated) order as {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained the same elements in the same (iterated) order as {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, rhs)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, rhs)

      }
    }
    
    describe("contain(ResultOfOnlyApplication) method returns MatcherFactory1") {
      
      val mtf = not contain only (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain only (1, 2)")
        mt.toString should be ("not contain only (1, 2)")
      }
      
      val lhs = List(2, 1)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained only (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " contained only (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        mr.rawFailureMessage shouldBe "{0} contained only ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain only ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained only ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain only ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain only (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " contained only (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain only (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained only (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} did not contain only ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained only ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain only ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained only ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfInOrderOnlyApplication) method returns MatcherFactory1") {
      
      val mtf = not contain inOrderOnly (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain inOrderOnly (1, 2)")
        mt.toString should be ("not contain inOrderOnly (1, 2)")
      }
      
      val lhs = List(1, 2)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained only (1, 2) in order"
        mr.negatedFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        mr.midSentenceFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        mr.rawFailureMessage shouldBe "{0} contained only ({1}) in order"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained only ({1}) in order"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        nmr.negatedFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain only (1, 2) in order"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained only (1, 2) in order"
        nmr.rawFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained only ({1}) in order"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain only ({1}) in order"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained only ({1}) in order"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfAllOfApplication) method returns MatcherFactory1") {
      
      val mtf = not contain allOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain allOf (1, 2)")
        mt.toString should be ("not contain allOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained all of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " contained all of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} contained all of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain all of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained all of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain all of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain all of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " contained all of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain all of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained all of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} did not contain all of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained all of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain all of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained all of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfInOrderApplication) method returns MatcherFactory1") {
      
      val mtf = not contain inOrder (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain inOrder (1, 2)")
        mt.toString should be ("not contain inOrder (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained all of (1, 2) in order"
        mr.negatedFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        mr.midSentenceFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        mr.rawFailureMessage shouldBe "{0} contained all of ({1}) in order"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained all of ({1}) in order"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        nmr.negatedFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain all of (1, 2) in order"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained all of (1, 2) in order"
        nmr.rawFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained all of ({1}) in order"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain all of ({1}) in order"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained all of ({1}) in order"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfAtMostOneOfApplication) method returns MatcherFactory1") {
      
      val mtf = not contain atMostOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("not contain atMostOneOf (1, 2)")
        mt.toString should be ("not contain atMostOneOf (1, 2)")
      }
      
      val lhs = List(1, 6, 8)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe lhs + " contained at most one of (1, 2)"
        mr.negatedFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        mr.midSentenceFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        mr.rawFailureMessage shouldBe "{0} contained at most one of ({1})"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained at most one of ({1})"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        mr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        nmr.negatedFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        nmr.midSentenceFailureMessage shouldBe lhs + " did not contain at most one of (1, 2)"
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " contained at most one of (1, 2)"
        nmr.rawFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained at most one of ({1})"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain at most one of ({1})"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained at most one of ({1})"
        nmr.failureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, UnquotedString("1, 2"))

      }
    }
    
    describe("contain(ResultOfKeyWordApplication) method returns MatcherFactory1") {
      
      val mtf = not contain key ("2")
      val mt = mtf.matcher[Map[String, String]]
      
      it("should have pretty toString") {
        mt.toString should be ("not contain key (\"2\")")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        mr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        mr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        mr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        mr.rawFailureMessage shouldBe "{0} contained key {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain key {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained key {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain key {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, "2")
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, "2")
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "2")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "2")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        nmr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        nmr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain key \"2\""
        nmr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained key \"2\""
        nmr.rawFailureMessage shouldBe "{0} did not contain key {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained key {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain key {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained key {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, "2")
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, "2")
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "2")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "2")

      }
    }
    
    describe("contain(ResultOfValueWordApplication) method returns MatcherFactory1") {
      
      val mtf = not contain value ("two")
      val mt = mtf.matcher[Map[String, String]]
      
      it("should have pretty toString") {
        mt.toString should be ("not contain value (\"two\")")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        mr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        mr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        mr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        mr.rawFailureMessage shouldBe "{0} contained value {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain value {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained value {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain value {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, "two")
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, "two")
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "two")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "two")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        nmr.negatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        nmr.midSentenceFailureMessage shouldBe decorateToStringValue(lhs) + " did not contain value \"two\""
        nmr.midSentenceNegatedFailureMessage shouldBe decorateToStringValue(lhs) + " contained value \"two\""
        nmr.rawFailureMessage shouldBe "{0} did not contain value {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained value {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain value {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained value {1}"
        nmr.failureMessageArgs shouldBe Vector(lhs, "two")
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, "two")
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "two")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "two")

      }
    }
    
    describe("contain(ResultOfAWordToAMatcherApplication) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not contain a (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not contain a (AMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean))")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        mr.negatedFailureMessage shouldBe leftList + " did not contain a file"
        mr.midSentenceFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " did not contain a file"
        mr.rawFailureMessage shouldBe "{0} contained a {1}: {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain a {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained a {1}: {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain a {1}"
        mr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        mr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe leftList + " did not contain a file"
        nmr.negatedFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        nmr.midSentenceFailureMessage shouldBe leftList + " did not contain a file"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " contained a file: " + myFile + " was a file"
        nmr.rawFailureMessage shouldBe "{0} did not contain a {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained a {1}: {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain a {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained a {1}: {2}"
        nmr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))

      }
    }
    
    describe("contain(ResultOfAnWordToAnMatcherApplication) method returns Matcher") {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not contain an (file)
      
      it("should have pretty toString") {
        mt.toString should be ("not contain an (AnMatcher[" + classOf[MyFile].getName + "](\"file\", " + classOf[MyFile].getName + " => Boolean))")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe false
        mr.failureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        mr.negatedFailureMessage shouldBe leftList + " did not contain an file"
        mr.midSentenceFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        mr.midSentenceNegatedFailureMessage shouldBe leftList + " did not contain an file"
        mr.rawFailureMessage shouldBe "{0} contained an {1}: {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} did not contain an {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} contained an {1}: {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not contain an {1}"
        mr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        mr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        mr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe leftList + " did not contain an file"
        nmr.negatedFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        nmr.midSentenceFailureMessage shouldBe leftList + " did not contain an file"
        nmr.midSentenceNegatedFailureMessage shouldBe leftList + " contained an file: " + myFile + " was an file"
        nmr.rawFailureMessage shouldBe "{0} did not contain an {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} contained an {1}: {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} did not contain an {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} contained an {1}: {2}"
        nmr.failureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.negatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))
        nmr.midSentenceFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))

      }
    }
    
  }
  
}
