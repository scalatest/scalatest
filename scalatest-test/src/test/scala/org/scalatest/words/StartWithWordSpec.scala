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

class StartWithWordSpec extends FreeSpec with FileMocks {
  
  "StartWithWord " - {
    
    "should have pretty toString" in {
      startWith.toString should be ("startWith")
    }
    
    "apply(String) method returns Matcher" - {
      
      val mt = startWith ("Pr")
      
      "should have pretty toString" in {
        mt.toString should be ("startWith (\"Pr\")")
      }
      
      val mr = mt("Programmer")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (true),
          'failureMessage ("\"Programmer\" did not start with substring \"Pr\""),
          'negatedFailureMessage ("\"Programmer\" started with substring \"Pr\""),
          'midSentenceFailureMessage ("\"Programmer\" did not start with substring \"Pr\""),
          'midSentenceNegatedFailureMessage ("\"Programmer\" started with substring \"Pr\""),
          'rawFailureMessage ("{0} did not start with substring {1}"),
          'rawNegatedFailureMessage ("{0} started with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} did not start with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with substring {1}"),
          'failureMessageArgs(Vector("Programmer", "Pr")),
          'negatedFailureMessageArgs(Vector("Programmer", "Pr")),
          'midSentenceFailureMessageArgs(Vector("Programmer", "Pr")),
          'midSentenceNegatedFailureMessageArgs(Vector("Programmer", "Pr"))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (false),
          'failureMessage ("\"Programmer\" started with substring \"Pr\""),
          'negatedFailureMessage ("\"Programmer\" did not start with substring \"Pr\""),
          'midSentenceFailureMessage ("\"Programmer\" started with substring \"Pr\""),
          'midSentenceNegatedFailureMessage ("\"Programmer\" did not start with substring \"Pr\""),
          'rawFailureMessage ("{0} started with substring {1}"),
          'rawNegatedFailureMessage ("{0} did not start with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} started with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not start with substring {1}"),
          'failureMessageArgs(Vector("Programmer", "Pr")),
          'negatedFailureMessageArgs(Vector("Programmer", "Pr")),
          'midSentenceFailureMessageArgs(Vector("Programmer", "Pr")),
          'midSentenceNegatedFailureMessageArgs(Vector("Programmer", "Pr"))    
        )
      }
    }
    
    "regex(String) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = startWith regex decimal
      
      "should have pretty toString" in {
        mt.toString should be ("startWith regex " + decimal)
      }
      
      val mr = mt("2.7b")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (true),
          'failureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (false),
          'failureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal)))    
        )
      }
    }
    
    "regex(Regex) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = startWith regex decimal.r
      
      "should have pretty toString" in {
        mt.toString should be ("startWith regex " + decimal)
      }
      
      val mr = mt("2.7b")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (true),
          'failureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr should have (
          'matches (false),
          'failureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'negatedFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'midSentenceFailureMessage ("\"2.7b\" started with a substring that matched the regular expression " + decimal),
          'midSentenceNegatedFailureMessage ("\"2.7b\" did not start with a substring that matched the regular expression " + decimal),
          'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
          'failureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("2.7b", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", UnquotedString(decimal)))    
        )
      }
    }
    
    "regex(a(b*)c withGroup bb) method returns Matcher" - {
      
      val bb = "bb"
      
      val mt = startWith regex ("""a(b*)c""" withGroup bb)
      
      "should have pretty toString" in {
        mt.toString should be ("startWith regex \"a(b*)c\" withGroup (\"" + bb + "\")")
      }
      
      val mr1 = mt("abbc")
      
      "when apply with \"abbc\"" - {
      
        "should have correct MatcherResult" in {
          mr1 should have (
            'matches (true),
            'failureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'negatedFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
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
            'failureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbc\" started with a substring that matched the regular expression a(b*)c, but \"bb\" did not match group bb"),
            'rawFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
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
            'failureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'negatedFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
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
            'failureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'negatedFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'midSentenceFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c and group bb"),
            'midSentenceNegatedFailureMessage ("\"abbbc\" started with a substring that matched the regular expression a(b*)c, but \"bbb\" did not match group bb"),
            'rawFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
            'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
            'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3}"),
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
            'failureMessage ("\"ABBC\" did not start with a substring that matched the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" started with a substring that matched the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" did not start with a substring that matched the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" started with a substring that matched the regular expression a(b*)c"),
            'rawFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
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
            'failureMessage ("\"ABBC\" started with a substring that matched the regular expression a(b*)c"),
            'negatedFailureMessage ("\"ABBC\" did not start with a substring that matched the regular expression a(b*)c"),
            'midSentenceFailureMessage ("\"ABBC\" started with a substring that matched the regular expression a(b*)c"),
            'midSentenceNegatedFailureMessage ("\"ABBC\" did not start with a substring that matched the regular expression a(b*)c"),
            'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
            'rawNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
            'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} did not start with a substring that matched the regular expression {1}"),
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
      
      val mt = startWith regex ("""a(b*)(c*)""" withGroups (bb, cc))
      
      "should have pretty toString" in {
        mt.toString should be ("startWith regex \"a(b*)(c*)\" withGroups (\"" + bb + "\", \"" + cc + "\")")
      }
      
      val mr = mt("abbccc")
      
      "should have correct MatcherResult" in {
        mr should have (
          'matches (false),
          'failureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'negatedFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'rawFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
          'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
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
          'failureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'negatedFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'midSentenceFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*) and group bb, cc"),
          'midSentenceNegatedFailureMessage ("\"abbccc\" started with a substring that matched the regular expression a(b*)(c*), but \"ccc\" did not match group cc at index 1"),
          'rawFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
          'rawNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'rawMidSentenceFailureMessage ("{0} started with a substring that matched the regular expression {1} and group {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with a substring that matched the regular expression {1}, but {2} did not match group {3} at index {4}"),
          'failureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'negatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)),
          'midSentenceFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))),
          'midSentenceNegatedFailureMessageArgs(Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1))    
        )
      }
    }
  }
}
