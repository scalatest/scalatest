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

class EndWithWordSpec extends Spec with FileMocks {
  
  object `EndWithWord ` {
    
    object `apply(String) method returns Matcher` {
      
      val mt = endWith ("er")
      
      def `should have pretty toString` {
        mt.toString should be ("endWith \"er\"")
      }
      
      val mr = mt("Programmer")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("\"Programmer\" did not end with substring \"er\""),
          'negatedFailureMessage ("\"Programmer\" ended with substring \"er\""),
          'midSentenceFailureMessage ("\"Programmer\" did not end with substring \"er\""),
          'midSentenceNegatedFailureMessage ("\"Programmer\" ended with substring \"er\""),
          'rawFailureMessage ("{0} did not end with substring {1}"),
          'rawNegatedFailureMessage ("{0} ended with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} did not end with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with substring {1}"),
          'failureMessageArgs(Vector("Programmer", "er")),
          'negatedFailureMessageArgs(Vector("Programmer", "er")),
          'midSentenceFailureMessageArgs(Vector("Programmer", "er")),
          'midSentenceNegatedFailureMessageArgs(Vector("Programmer", "er"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"Programmer\" ended with substring \"er\""),
          'negatedFailureMessage ("\"Programmer\" did not end with substring \"er\""),
          'midSentenceFailureMessage ("\"Programmer\" ended with substring \"er\""),
          'midSentenceNegatedFailureMessage ("\"Programmer\" did not end with substring \"er\""),
          'rawFailureMessage ("{0} ended with substring {1}"),
          'rawNegatedFailureMessage ("{0} did not end with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} ended with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not end with substring {1}"),
          'failureMessageArgs(Vector("Programmer", "er")),
          'negatedFailureMessageArgs(Vector("Programmer", "er")),
          'midSentenceFailureMessageArgs(Vector("Programmer", "er")),
          'midSentenceNegatedFailureMessageArgs(Vector("Programmer", "er"))    
        )
      }
    }
    
    object `regex(String) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = endWith regex decimal
      
      def `should have pretty toString` {
        mt.toString should be ("endWith regex " + decimal)
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
    }
    
    object `regex(Regex) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = endWith regex decimal.r
      
      def `should have pretty toString` {
        mt.toString should be ("endWith regex " + decimal)
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" ended with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" did not end with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
    }
    
    object `regex(a(b*)c withGroup bb) method returns Matcher` {
      
      val bb = "bb"
      
      val mt = endWith regex ("""a(b*)c""" withGroup bb)
      
      def `should have pretty toString` {
        mt.toString should be ("endWith regex a(b*)c withGroup " + bb)
      }
      
      val mr1 = mt("abbc")
      
      object `when apply with "abbc"` {
      
        def `should have correct MatcherResult` {
          mr1 should have (
            'matches (true),
            'failureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'negatedFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'failureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb")))    
          )
        }
      
        val nmr = mr1.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" ended with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'failureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb")))    
          )
        }
        
      }
      
      val mr2 = mt("abbbc")
        
      object `when apply with "abbbc"` {
          
        def `should have correct MatcherResult` {
            
          mr2 should have (
            'matches (false),
            'failureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'negatedFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'failureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb")))    
          )
            
        }
          
        val nmr = mr2.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (true),
            'failureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" ended with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'failureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'negatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))),
            'midSentenceFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))),
            'midSentenceNegatedFailureMessageArgs(Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb")))    
          )
        }
          
      }
      
      val mr3 = mt("ABBC")
      
      object `when apply with "ABBC"` {
        
        def `should have correct MatcherResult` {
          mr3 should have (
            'matches (false),
            'failureMessage ("\"ABBC\" did not end with a substring that matched the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" ended with a substring that matched the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" did not end with a substring that matched the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" ended with a substring that matched the regular expression a(b*)c"),
            'rawFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
            'failureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'negatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceNegatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c")))    
          )
        }
        
        val nmr = mr3.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (true),
            'failureMessage ("\"ABBC\" ended with a substring that matched the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" did not end with a substring that matched the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" ended with a substring that matched the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" did not end with a substring that matched the regular expression a(b*)c"),
            'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} did not end with a substring that matched the regular expression {1}"),
            'failureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'negatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c"))),
            'midSentenceNegatedFailureMessageArgs(Vector("ABBC", UnquotedString("a(b*)c")))    
          )
        }
      }
    }
    
    object `regex(a(b*)(c*) withGroup bb) method returns Matcher` {
      val bb = "bb"
      val cc = "cc"
      
      val mt = endWith regex ("""a(b*)(c*)""" withGroups (bb, cc))
      
      def `should have pretty toString` {
        mt.toString should be ("endWith regex a(b*)(c*) withGroups " + bb + ", " + cc)
      }
      
      val mr = mt("abbccc")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'negatedFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
          'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
          'failureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'negatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'midSentenceFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'midSentenceNegatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'negatedFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" ended with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'rawFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
          'rawNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceFailureMessage ("{0} ended with a substring that matched the regular expression {1} and group {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'failureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'negatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'midSentenceFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'midSentenceNegatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1))    
        )
      }
    }
  }
}