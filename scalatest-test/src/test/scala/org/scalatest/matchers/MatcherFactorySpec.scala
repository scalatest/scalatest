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
package org.scalatest.matchers

import org.scalatest._
import Matchers._
import matchers.{MatchResult, 
                 FailureMessage, 
                 NegatedFailureMessage, 
                 MidSentenceNegatedFailureMessage, 
                 MidSentenceFailureMessage}
import org.scalactic.Prettifier

class MatcherFactorySpec extends FunSpec {
  
  describe("MatcherFactory1 ") {
    
    describe("AndNotWord ") {
      
      describe("equal(Null) method returns Matcher") {
        
        val aNullRef: String = null
        val mtf = not equal ("hi") and not equal (null)
        val mt = mtf.matcher[String]
      
        it("should have pretty toString") {
          mtf.toString should be ("(not (equal (\"hi\"))) and (not equal null)")
        }
        
        val mr = mt("Bob")
        
        val leftResult = 
          MatchResult(
            true, 
            "{0} equaled {1}", 
            "{0} did not equal {1}",
            Vector("Bob", "hi"), 
            Vector("[Bob]", "[hi]"),
            Prettifier.default
          )
          
        val rightResult = 
          MatchResult(
            true, 
            "The reference equaled null", 
            "{0} did not equal null",
            "the reference equaled null", 
            "{0} did not equal null", 
            Vector.empty, 
            Vector("Bob"),
            Prettifier.default
          )
          
        it("should have correct MatcherResult") {
          mr.matches shouldBe (true)
          mr.failureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null")
          mr.negatedFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null")
          mr.midSentenceFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null")
          mr.midSentenceNegatedFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null")
          mr.rawFailureMessage shouldBe ("{0}, but {1}")
          mr.rawNegatedFailureMessage shouldBe ("{0}, and {1}")
          mr.rawMidSentenceFailureMessage shouldBe ("{0}, but {1}")
          mr.rawMidSentenceNegatedFailureMessage shouldBe ("{0}, and {1}")
          mr.failureMessageArgs shouldBe (Vector(NegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          mr.negatedFailureMessageArgs shouldBe (Vector(NegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          mr.midSentenceFailureMessageArgs shouldBe (Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe (false)
          nmr.failureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null")
          nmr.negatedFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null")
          nmr.midSentenceFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null")
          nmr.midSentenceNegatedFailureMessage shouldBe ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null")
          nmr.rawFailureMessage shouldBe ("{0}, and {1}")
          nmr.rawNegatedFailureMessage shouldBe ("{0}, but {1}")
          nmr.rawMidSentenceFailureMessage shouldBe ("{0}, and {1}")
          nmr.rawMidSentenceNegatedFailureMessage shouldBe ("{0}, but {1}")
          nmr.failureMessageArgs shouldBe (Vector(NegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          nmr.negatedFailureMessageArgs shouldBe (Vector(NegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          nmr.midSentenceFailureMessageArgs shouldBe (Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          nmr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
        }
        
      }
      
    }
    
    describe("OrNotWord ") {
      
      describe("equal(Null) method returns Matcher") {
        
        val aNullRef: String = null
        val mtf = not equal ("Bob") or not equal (null)
        val mt = mtf.matcher[String]
      
        it("should have pretty toString") {
          mt.toString should be ("(not (equal (\"Bob\"))) or (not equal null)")
        }
        
        val mr = mt("Bob")
        
        val leftResult = 
          MatchResult(
            false, 
            "{0} equaled {1}", 
            "{0} did not equal {1}",
            Vector("Bob", "Bob"), 
            Vector("Bob", "Bob"),
            Prettifier.default
          )
          
        val rightResult = 
          MatchResult(
            true, 
            "The reference equaled null", 
            "{0} did not equal null",
            "the reference equaled null", 
            "{0} did not equal null", 
            Vector.empty, 
            Vector("Bob"),
            Prettifier.default
          )
        
        it("should have correct MatcherResult") {
          mr.matches shouldBe (true)
          mr.failureMessage shouldBe ("\"Bob\" equaled \"Bob\", and the reference equaled null")
          mr.negatedFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null")
          mr.midSentenceFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and the reference equaled null")
          mr.midSentenceNegatedFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null")
          mr.rawFailureMessage shouldBe ("{0}, and {1}")
          mr.rawNegatedFailureMessage shouldBe ("{0}, and {1}")
          mr.rawMidSentenceFailureMessage shouldBe ("{0}, and {1}")
          mr.rawMidSentenceNegatedFailureMessage shouldBe ("{0}, and {1}")
          mr.failureMessageArgs shouldBe (Vector(FailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          mr.negatedFailureMessageArgs shouldBe (Vector(FailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          mr.midSentenceFailureMessageArgs shouldBe (Vector(MidSentenceFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          mr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(MidSentenceFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe (false)
          nmr.failureMessage shouldBe ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null")
          nmr.negatedFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and the reference equaled null")
          nmr.midSentenceFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null")
          nmr.midSentenceNegatedFailureMessage shouldBe ("\"Bob\" equaled \"Bob\", and the reference equaled null")
          nmr.rawFailureMessage shouldBe ("{0}, and {1}")
          nmr.rawNegatedFailureMessage shouldBe ("{0}, and {1}")
          nmr.rawMidSentenceFailureMessage shouldBe ("{0}, and {1}")
          nmr.rawMidSentenceNegatedFailureMessage shouldBe ("{0}, and {1}")
          nmr.failureMessageArgs shouldBe (Vector(FailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          nmr.negatedFailureMessageArgs shouldBe (Vector(FailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
          nmr.midSentenceFailureMessageArgs shouldBe (Vector(MidSentenceFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))
          nmr.midSentenceNegatedFailureMessageArgs shouldBe (Vector(MidSentenceFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))
        }
        
      }
      
    }
    
    describe("and(MatcherFactory1) method returns MatcherFactory1") {
      
      val mtf1 = equal ("Bob")
      val mtf = mtf1 and (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      it("should have pretty toString") {
        mtf.toString should be ("(equal (\"Bob\")) and (equal (\"Alice\"))")
      }
    }
    
    describe("or(MatcherFactory1) method returns MatcherFactory1") {
      
      val mtf1 = equal ("Bob")
      val mtf = mtf1 or (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      it("should have pretty toString") {
        mtf.toString should be ("(equal (\"Bob\")) or (equal (\"Alice\"))")
      }
    }
    
  }
  
}