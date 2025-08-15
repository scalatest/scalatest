/*
 * Copyright 2001-2025 Artima, Inc.
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
import matchers.{MatchResult, 
                 FailureMessage, 
                 NegatedFailureMessage, 
                 MidSentenceNegatedFailureMessage, 
                 MidSentenceFailureMessage}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers._

class MatcherSpec extends AnyFunSpec {
  
  describe("Matcher ") {
    
    describe("AndNotWord ") {
      
      describe("equal(Null) method returns Matcher") {
        
        val aNullRef: String = null
        val mt = not be ("hi") and not equal (null)
      
        it("should have pretty toString") {
          mt.toString should be ("(not be \"hi\") and (not equal null)")
        }
        
        val mr = mt("Bob")
        
        val leftResult = 
          MatchResult(
            true, 
            "{0} was equal to {1}", 
            "{0} was not equal to {1}",
            Vector("Bob", "hi"), 
            Vector("[Bob]", "[hi]")
          )
          
        val rightResult = 
          MatchResult(
            true, 
            "The reference equaled null", 
            "{0} did not equal null",
            "the reference equaled null", 
            "{0} did not equal null", 
            Vector.empty, 
            Vector("Bob")
          )
          
        it("should have correct MatcherResult") {
          mr.matches shouldBe (true)
          mr.failureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null")
          mr.negatedFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null")
          mr.midSentenceFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null")
          mr.midSentenceNegatedFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null")
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
          nmr.failureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null")
          nmr.negatedFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null")
          nmr.midSentenceFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null")
          nmr.midSentenceNegatedFailureMessage shouldBe ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null")
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
        val mt = not be ("Bob") or not equal (null)
      
        it("should have pretty toString") {
          mt.toString should be ("(not be \"Bob\") or (not equal null)")
        }
        
        val mr = mt("Bob")
        
        val leftResult = 
          MatchResult(
            false, 
            "{0} was equal to {1}", 
            "{0} was not equal to {1}",
            Vector("Bob", "Bob"), 
            Vector("Bob", "Bob")
          )
          
        val rightResult = 
          MatchResult(
            true, 
            "The reference equaled null", 
            "{0} did not equal null",
            "the reference equaled null", 
            "{0} did not equal null", 
            Vector.empty, 
            Vector("Bob")
          )
        
        it("should have correct MatcherResult") {
          mr.matches shouldBe (true)
          mr.failureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and the reference equaled null")
          mr.negatedFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null")
          mr.midSentenceFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and the reference equaled null")
          mr.midSentenceNegatedFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null")
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
          nmr.failureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null")
          nmr.negatedFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and the reference equaled null")
          nmr.midSentenceFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null")
          nmr.midSentenceNegatedFailureMessage shouldBe ("\"Bob\" was equal to \"Bob\", and the reference equaled null")
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
      
      val mt1 = be ("Bob")
      val mtf = mt1 and (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      it("should have pretty toString") {
        mtf.toString should be ("(be (\"Bob\")) and (equal (\"Alice\"))")
      }
    }
    
    describe("or(MatcherFactory1) method returns MatcherFactory1") {
      
      val mt1 = be ("Bob")
      val mtf = mt1 or (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      it("should have pretty toString") {
        mtf.toString should be ("(be (\"Bob\")) or (equal (\"Alice\"))")
      }
    }
    
    /*describe("compose(U => T) returns Matcher") {
      
      val mt1 = be > 18
      def fun(input: String): Int = 88
      val mt = mt1 compose fun
      
      it("should have pretty toString") {
        mt.toString should be ("(be > 18) compose (java.lang.String => Int)")
      }
      
    }*/
    
    describe("apply(T => MatchResult) method returns Matcher") {
      
      val mt = Matcher.apply((test: String) => MatchResult(true, "test", "test"))
      
      it("should have pretty toString") {
        mt.toString should be ("Matcher[java.lang.String](java.lang.String => MatchResult)")
      }
      
    }
    
  }
  
}