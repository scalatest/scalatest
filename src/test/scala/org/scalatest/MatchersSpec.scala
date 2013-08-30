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

import org.scalatest._
import Matchers._

class MatchersSpec extends Spec {
  
  object `Matchers ` {
    
    object `equal(Spread) method returns Matcher` {
      
      val mt = equal (8 +- 1)
      
      def `should have pretty toString` {
        mt.toString should be ("equal (8 +- 1)")
      }
      
      val mr = mt(9)
      
      def `should have correct MatchResult` {
        mr should have (
          'matches (true),
          'failureMessage ("9 did not equal 8 plus or minus 1"),
          'negatedFailureMessage ("9 equaled 8 plus or minus 1"),
          'midSentenceFailureMessage ("9 did not equal 8 plus or minus 1"),
          'midSentenceNegatedFailureMessage ("9 equaled 8 plus or minus 1"),
          'rawFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'failureMessageArgs(Vector(9, 8, 1)),
          'negatedFailureMessageArgs(Vector(9, 8, 1)),
          'midSentenceFailureMessageArgs(Vector(9, 8, 1)),
          'midSentenceNegatedFailureMessageArgs(Vector(9, 8, 1))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatchResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("9 equaled 8 plus or minus 1"),
          'negatedFailureMessage ("9 did not equal 8 plus or minus 1"),
          'midSentenceFailureMessage ("9 equaled 8 plus or minus 1"),
          'midSentenceNegatedFailureMessage ("9 did not equal 8 plus or minus 1"),
          'rawFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'failureMessageArgs(Vector(9, 8, 1)),
          'negatedFailureMessageArgs(Vector(9, 8, 1)),
          'midSentenceFailureMessageArgs(Vector(9, 8, 1)),
          'midSentenceNegatedFailureMessageArgs(Vector(9, 8, 1))    
        )
      }
      
    }
    
    object `equal(Null) method returns Matcher` {
      
      val mt = equal (null)
      
      def `should have pretty toString` {
        mt.toString should be ("equal (null)")
      }
      
      val mr = mt(null)
      
      def `should have correct MatchResult` {
        mr should have (
          'matches (true),
          'failureMessage ("null did not equal null"),
          'negatedFailureMessage ("The reference equaled null"),
          'midSentenceFailureMessage ("null did not equal null"),
          'midSentenceNegatedFailureMessage ("the reference equaled null"),
          'rawFailureMessage ("{0} did not equal null"),
          'rawNegatedFailureMessage ("The reference equaled null"),
          'rawMidSentenceFailureMessage ("{0} did not equal null"),
          'rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
          'failureMessageArgs(Vector(null)),
          'negatedFailureMessageArgs(Vector.empty),
          'midSentenceFailureMessageArgs(Vector(null)),
          'midSentenceNegatedFailureMessageArgs(Vector.empty)    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatchResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("The reference equaled null"),
          'negatedFailureMessage ("null did not equal null"),
          'midSentenceFailureMessage ("the reference equaled null"),
          'midSentenceNegatedFailureMessage ("null did not equal null"),
          'rawFailureMessage ("The reference equaled null"),
          'rawNegatedFailureMessage ("{0} did not equal null"),
          'rawMidSentenceFailureMessage ("the reference equaled null"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
          'failureMessageArgs(Vector.empty),
          'negatedFailureMessageArgs(Vector(null)),
          'midSentenceFailureMessageArgs(Vector.empty),
          'midSentenceNegatedFailureMessageArgs(Vector(null))    
        )
      }
      
    }
    
    object `HavePropertyMatcherGenerator ` {
      
      object `apply(Any) returns HavePropertyMatcher` {
        
        val generator = new HavePropertyMatcherGenerator('name)
        val havePropMatcher = generator("test")
        
        def `should have pretty toString` {
          havePropMatcher.toString should be ("HavePropertyMatcher[AnyRef, Any](expectedValue = \"test\")")
        }
        
      }
      
    }
    
    object `RegexWord ` {
      
      def `should have pretty toString` {
        regex.toString should be ("regex")
      }
      
    }
    
  }
  
}