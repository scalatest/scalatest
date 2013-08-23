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

class MatcherWordsSpec extends Spec with MatcherWords {
  
  object `MatcherWords ` {
    
    object `equal(Any) method returns MatcherFactory1` {
      
      val mtf = equal ("tommy")
      val mt = mtf.matcher[String]
      
      def `should have pretty toString` {
        mtf.toString should be ("equal \"tommy\"")
        mt.toString should be ("equal \"tommy\"")
      }
      
      val mr = mt("tomy")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'negatedFailureMessage ("\"tomy\" equaled \"tommy\""),
          'midSentenceFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'midSentenceNegatedFailureMessage ("\"tomy\" equaled \"tommy\""),
          'rawFailureMessage ("{0} did not equal {1}"),
          'rawNegatedFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
          'failureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'negatedFailureMessageArgs(Vector("tomy", "tommy")),
          'midSentenceFailureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'midSentenceNegatedFailureMessageArgs(Vector("tomy", "tommy"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"tomy\" equaled \"tommy\""),
          'negatedFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'midSentenceFailureMessage ("\"tomy\" equaled \"tommy\""),
          'midSentenceNegatedFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'rawFailureMessage ("{0} equaled {1}"),
          'rawNegatedFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1}"),
          'failureMessageArgs(Vector("tomy", "tommy")),
          'negatedFailureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'midSentenceFailureMessageArgs(Vector("tomy", "tommy")),
          'midSentenceNegatedFailureMessageArgs(Vector("tom[]y", "tom[m]y"))    
        )
      }
      
    }
    
    object `legacyEqual(Any) method returns Matcher` {
      
      val mt = legacyEqual ("tommy")
      
      def `should have pretty toString` {
        mt.toString should be ("legacyEqual \"tommy\"")
      }
      
      val mr = mt("tomy")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'negatedFailureMessage ("\"tomy\" equaled \"tommy\""),
          'midSentenceFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'midSentenceNegatedFailureMessage ("\"tomy\" equaled \"tommy\""),
          'rawFailureMessage ("{0} did not equal {1}"),
          'rawNegatedFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
          'failureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'negatedFailureMessageArgs(Vector("tomy", "tommy")),
          'midSentenceFailureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'midSentenceNegatedFailureMessageArgs(Vector("tomy", "tommy"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"tomy\" equaled \"tommy\""),
          'negatedFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'midSentenceFailureMessage ("\"tomy\" equaled \"tommy\""),
          'midSentenceNegatedFailureMessage ("\"tom[]y\" did not equal \"tom[m]y\""),
          'rawFailureMessage ("{0} equaled {1}"),
          'rawNegatedFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1}"),
          'failureMessageArgs(Vector("tomy", "tommy")),
          'negatedFailureMessageArgs(Vector("tom[]y", "tom[m]y")),
          'midSentenceFailureMessageArgs(Vector("tomy", "tommy")),
          'midSentenceNegatedFailureMessageArgs(Vector("tom[]y", "tom[m]y"))    
        )
      }
      
    }
    
  }
  
}