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
import Matchers._
import matchers.{MatchResult, 
                 FailureMessage, 
                 NegatedFailureMessage, 
                 MidSentenceNegatedFailureMessage, 
                 MidSentenceFailureMessage}

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
          mr should have (
            'matches (true),
            'failureMessage ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null"),
            'negatedFailureMessage ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null"),
            'midSentenceFailureMessage ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null"),
            'midSentenceNegatedFailureMessage ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null"),
            'rawFailureMessage ("{0}, but {1}"),
            'rawNegatedFailureMessage ("{0}, and {1}"),
            'rawMidSentenceFailureMessage ("{0}, but {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0}, and {1}"),
            'failureMessageArgs(Vector(NegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'negatedFailureMessageArgs(Vector(NegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'midSentenceFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'midSentenceNegatedFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))    
          )
        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr should have (
            'matches (false),
            'failureMessage ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null"),
            'negatedFailureMessage ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null"),
            'midSentenceFailureMessage ("\"[Bob]\" did not equal \"[hi]\", and \"Bob\" did not equal null"),
            'midSentenceNegatedFailureMessage ("\"[Bob]\" did not equal \"[hi]\", but the reference equaled null"),
            'rawFailureMessage ("{0}, and {1}"),
            'rawNegatedFailureMessage ("{0}, but {1}"),
            'rawMidSentenceFailureMessage ("{0}, and {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0}, but {1}"),
            'failureMessageArgs(Vector(NegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'negatedFailureMessageArgs(Vector(NegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'midSentenceFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'midSentenceNegatedFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))    
          )
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
          mr should have (
            'matches (true),
            'failureMessage ("\"Bob\" equaled \"Bob\", and the reference equaled null"),
            'negatedFailureMessage ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null"),
            'midSentenceFailureMessage ("\"Bob\" equaled \"Bob\", and the reference equaled null"),
            'midSentenceNegatedFailureMessage ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null"),
            'rawFailureMessage ("{0}, and {1}"),
            'rawNegatedFailureMessage ("{0}, and {1}"),
            'rawMidSentenceFailureMessage ("{0}, and {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0}, and {1}"),
            'failureMessageArgs(Vector(FailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'negatedFailureMessageArgs(Vector(FailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'midSentenceFailureMessageArgs(Vector(MidSentenceFailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'midSentenceNegatedFailureMessageArgs(Vector(MidSentenceFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult)))    
          )
        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr should have (
            'matches (false),
            'failureMessage ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null"),
            'negatedFailureMessage ("\"Bob\" equaled \"Bob\", and the reference equaled null"),
            'midSentenceFailureMessage ("\"Bob\" equaled \"Bob\", and \"Bob\" did not equal null"),
            'midSentenceNegatedFailureMessage ("\"Bob\" equaled \"Bob\", and the reference equaled null"),
            'rawFailureMessage ("{0}, and {1}"),
            'rawNegatedFailureMessage ("{0}, and {1}"),
            'rawMidSentenceFailureMessage ("{0}, and {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0}, and {1}"),
            'failureMessageArgs(Vector(FailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'negatedFailureMessageArgs(Vector(FailureMessage(leftResult), MidSentenceFailureMessage(rightResult))),
            'midSentenceFailureMessageArgs(Vector(MidSentenceFailureMessage(leftResult), MidSentenceNegatedFailureMessage(rightResult))),
            'midSentenceNegatedFailureMessageArgs(Vector(MidSentenceFailureMessage(leftResult), MidSentenceFailureMessage(rightResult)))    
          )
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