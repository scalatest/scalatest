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
    
  }
  
}