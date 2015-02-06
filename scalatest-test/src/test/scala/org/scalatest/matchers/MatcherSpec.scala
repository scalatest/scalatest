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

class MatcherSpec extends Spec {
  
  object `Matcher ` {
    
    object `AndNotWord ` {
      
      object `equal(Null) method returns Matcher` {
        
        val aNullRef: String = null
        val mt = not be ("hi") and not equal (null)
      
        def `should have pretty toString` {
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
          
        def `should have correct MatcherResult` {
          mr should have (
            'matches (true),
            'failureMessage ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null"),
            'negatedFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null"),
            'midSentenceFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null"),
            'midSentenceNegatedFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null"),
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
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null"),
            'negatedFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null"),
            'midSentenceFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", and \"Bob\" did not equal null"),
            'midSentenceNegatedFailureMessage ("\"[Bob]\" was not equal to \"[hi]\", but the reference equaled null"),
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
    
    object `OrNotWord ` {
      
      object `equal(Null) method returns Matcher` {
        
        val aNullRef: String = null
        val mt = not be ("Bob") or not equal (null)
      
        def `should have pretty toString` {
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
        
        def `should have correct MatcherResult` {
          mr should have (
            'matches (true),
            'failureMessage ("\"Bob\" was equal to \"Bob\", and the reference equaled null"),
            'negatedFailureMessage ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null"),
            'midSentenceFailureMessage ("\"Bob\" was equal to \"Bob\", and the reference equaled null"),
            'midSentenceNegatedFailureMessage ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null"),
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
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null"),
            'negatedFailureMessage ("\"Bob\" was equal to \"Bob\", and the reference equaled null"),
            'midSentenceFailureMessage ("\"Bob\" was equal to \"Bob\", and \"Bob\" did not equal null"),
            'midSentenceNegatedFailureMessage ("\"Bob\" was equal to \"Bob\", and the reference equaled null"),
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
    
    object `and(MatcherFactory1) method returns MatcherFactory1` {
      
      val mt1 = be ("Bob")
      val mtf = mt1 and (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      def `should have pretty toString` {
        mtf.toString should be ("(be (\"Bob\")) and (equal (\"Alice\"))")
      }
    }
    
    object `or(MatcherFactory1) method returns MatcherFactory1` {
      
      val mt1 = be ("Bob")
      val mtf = mt1 or (equal ("Alice"))
      val mt = mtf.matcher[String]
      
      def `should have pretty toString` {
        mtf.toString should be ("(be (\"Bob\")) or (equal (\"Alice\"))")
      }
    }
    
    /*object `compose(U => T) returns Matcher` {
      
      val mt1 = be > 18
      def fun(input: String): Int = 88
      val mt = mt1 compose fun
      
      def `should have pretty toString` {
        mt.toString should be ("(be > 18) compose (java.lang.String => Int)")
      }
      
    }*/
    
    object `apply(T => MatchResult) method returns Matcher` {
      
      val mt = Matcher.apply((test: String) => MatchResult(true, "test", "test"))
      
      def `should have pretty toString` {
        mt.toString should be ("Matcher[java.lang.String](java.lang.String => MatchResult)")
      }
      
    }
    
  }
  
}