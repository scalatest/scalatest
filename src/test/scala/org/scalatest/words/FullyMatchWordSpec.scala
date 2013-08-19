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

class FullyMatchWordSpec extends FreeSpec with Matchers {
  
  "FullyMatchWord " - {
    
    "regex(String) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = fullyMatch regex decimal
      
      "should have pretty toString" in {
        mt.toString should be ("fullyMatch regex " + decimal)
      }
      
      val mr = mt("2.7")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (true),
          'failureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (false),
          'failureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'rawFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
          'failureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal)))    
        )
      }
      
    }
    
    "regex(a(b*)c withGroup bb) method returns Matcher" - {
      
      val bb = "bb"
      
      val mt = fullyMatch regex ("""a(b*)c""" withGroup bb)
      
      "should have pretty toString" in {
        mt.toString should be ("fullyMatch regex a(b*)c withGroup " + bb)
      }
      
      val mr1 = mt("abbc")
      
      "when apply with \"abbc\"" - {
      
        "should have correct MatcherResult" in {
          mr1 should have (
            'matches (true),
            'failureMessage ("\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'negatedFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'failureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb")))    
          )
        }
      
        val nmr = mr1.negated
      
        "should have correct negated MatcherResult" in {
          nmr should have (
            'matches (false),
            'failureMessage ("\"abbc\" fully matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" fully matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'rawFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'failureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb")))    
          )
        }
        
      }
      
      val mr2 = mt("abbbc")
        
      "when apply with \"abbbc\"" - {
          
        "should have correct MatcherResult" in {
            
          mr2 should have (
            'matches (false),
            'failureMessage ("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'negatedFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'failureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb")))    
          )
            
        }
          
        val nmr = mr2.negated
      
        "should have correct negated MatcherResult" in {
          nmr should have (
            'matches (true),
            'failureMessage ("\"abbbc\" fully matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" fully matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'rawFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3}"),
            'failureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb")))    
          )
        }
          
      }
      
      val mr3 = mt("ABBC")
      
      "when apply with \"ABBC\"" - {
        
        "should have correct MatcherResult" in {
          mr3 should have (
            'matches (false),
            'failureMessage ("\"ABBC\" did not fully match the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" fully matched the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" did not fully match the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" fully matched the regular expression a(b*)c"),
            'rawFailureMessage ("{0} did not fully match the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} did not fully match the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
            'failureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'negatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceNegatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c")))    
          )
        }
        
        val nmr = mr3.negated
      
        "should have correct negated MatcherResult" in {
          nmr should have (
            'matches (true),
            'failureMessage ("\"ABBC\" fully matched the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" did not fully match the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" fully matched the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" did not fully match the regular expression a(b*)c"),
            'rawFailureMessage ("{0} fully matched the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
            'failureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'negatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceNegatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c")))    
          )
        }
      }
    }
    
    "regex(a(b*)(c*) withGroup bb) method returns Matcher" - {
      val bb = "bb"
      val cc = "cc"
      
      val mt = fullyMatch regex ("""a(b*)(c*)""" withGroups (bb, cc))
      
      "should have pretty toString" in {
        mt.toString should be ("fullyMatch regex a(b*)(c*) withGroups " + bb + ", " + cc)
      }
      
      val mr = mt("abbccc")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (false),
          'failureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'negatedFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*) and group bb, cc"),
          'rawFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
          'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
          'failureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'negatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'midSentenceFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'midSentenceNegatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc")))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (true),
          'failureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*) and group bb, cc"),
          'negatedFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" fully matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'rawFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
          'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1} and group {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'failureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'negatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'midSentenceFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'midSentenceNegatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1))    
        )
      }
    }
    
    "regex(Regex) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = fullyMatch regex decimal.r
      
      "should have pretty toString" in {
        mt.toString should be ("fullyMatch regex " + decimal)
      }
      
      val mr = mt("2.7")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (true),
          'failureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} fully matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (false),
          'failureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7\" fully matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7\" did not fully match the regular expression " + decimal),
          'rawFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} fully matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not fully match the regular expression {1}"),
          'failureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7", UnquotedString(decimal)))    
        )
      }
    }
    
  }
  
}