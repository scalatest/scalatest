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

class IncludeWordSpec extends FreeSpec with Matchers {
  
  "IncludeWord " - {
    
    "should have pretty toString" in {
      include.toString should be ("include")
    }
    
    "apply(String) method returns Matcher" - {
      
      val mt = include ("er")
      
      "should have pretty toString" in {
        mt.toString should be ("include (\"er\")")
      }
      
      val mr = mt("Programmer")
      
      "should have correct MatcherResult" in {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "\"Programmer\" did not include substring \"er\""
        mr.negatedFailureMessage shouldBe "\"Programmer\" included substring \"er\""
        mr.midSentenceFailureMessage shouldBe "\"Programmer\" did not include substring \"er\""
        mr.midSentenceNegatedFailureMessage shouldBe "\"Programmer\" included substring \"er\""
        mr.rawFailureMessage shouldBe "{0} did not include substring {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} included substring {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not include substring {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring {1}"
        mr.failureMessageArgs shouldBe Vector("Programmer", "er")
        mr.negatedFailureMessageArgs shouldBe Vector("Programmer", "er")
        mr.midSentenceFailureMessageArgs shouldBe Vector("Programmer", "er")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("Programmer", "er")

      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "\"Programmer\" included substring \"er\""
        nmr.negatedFailureMessage shouldBe "\"Programmer\" did not include substring \"er\""
        nmr.midSentenceFailureMessage shouldBe "\"Programmer\" included substring \"er\""
        nmr.midSentenceNegatedFailureMessage shouldBe "\"Programmer\" did not include substring \"er\""
        nmr.rawFailureMessage shouldBe "{0} included substring {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not include substring {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring {1}"
        nmr.failureMessageArgs shouldBe Vector("Programmer", "er")
        nmr.negatedFailureMessageArgs shouldBe Vector("Programmer", "er")
        nmr.midSentenceFailureMessageArgs shouldBe Vector("Programmer", "er")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("Programmer", "er")

      }
    }
    
    "regex(String) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = include regex decimal
      
      "should have pretty toString" in {
        mt.toString should be ("include regex \"" + decimal + "\"")
      }
      
      val mr = mt("b2.7")
      
      "should have correct MatcherResult" in {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.negatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.rawFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.negatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
    }
    
    "regex(Regex) method returns Matcher" - {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = include regex decimal.r
      
      "should have pretty toString" in {
        mt.toString should be ("include regex \"" + decimal + "\"")
      }
      
      val mr = mt("b2.7")
      
      "should have correct MatcherResult" in {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.negatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.midSentenceFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        mr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        mr.rawFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
        mr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.negatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.midSentenceFailureMessage shouldBe "\"b2.7\" included substring that matched regex " + decimal
        nmr.midSentenceNegatedFailureMessage shouldBe "\"b2.7\" did not include substring that matched regex " + decimal
        nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
        nmr.failureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.negatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("b2.7", UnquotedString(decimal))

      }
    }
    
    "regex(a(b*)c withGroup bb) method returns Matcher" - {
      
      val bb = "bb"
      
      val mt = include regex ("""a(b*)c""" withGroup bb)
      
      "should have pretty toString" in {
        mt.toString should be ("include regex \"a(b*)c\" withGroup (\"" + bb + "\")")
      }
      
      val mr1 = mt("abbc")
      
      "when apply with \"abbc\"" - {
      
        "should have correct MatcherResult" in {
          mr1.matches shouldBe true
          mr1.failureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c, but \"bb\" did not match group bb"
          mr1.negatedFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c and group bb"
          mr1.midSentenceFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c, but \"bb\" did not match group bb"
          mr1.midSentenceNegatedFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c and group bb"
          mr1.rawFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          mr1.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          mr1.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          mr1.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          mr1.failureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))
          mr1.negatedFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
          mr1.midSentenceFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))
          mr1.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))

        }
      
        val nmr = mr1.negated
      
        "should have correct negated MatcherResult" in {
          nmr.matches shouldBe false
          nmr.failureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c and group bb"
          nmr.negatedFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c, but \"bb\" did not match group bb"
          nmr.midSentenceFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c and group bb"
          nmr.midSentenceNegatedFailureMessage shouldBe "\"abbc\" included substring that matched regex a(b*)c, but \"bb\" did not match group bb"
          nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          nmr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          nmr.failureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
          nmr.negatedFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))
          nmr.midSentenceFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbc", UnquotedString("a(b*)c"), "bb", UnquotedString("bb"))

        }
        
      }
      
      val mr2 = mt("abbbc")
        
      "when apply with \"abbbc\"" - {
          
        "should have correct MatcherResult" in {
            
          mr2.matches shouldBe false
            
          mr2.failureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb"
            
          mr2.negatedFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c and group bb"
            
          mr2.midSentenceFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb"
            
          mr2.midSentenceNegatedFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c and group bb"
            
          mr2.rawFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
            
          mr2.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
            
          mr2.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
            
          mr2.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
            
          mr2.failureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))
            
          mr2.negatedFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
            
          mr2.midSentenceFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))
            
          mr2.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))

            
        }
          
        val nmr = mr2.negated
      
        "should have correct negated MatcherResult" in {
          nmr.matches shouldBe true
          nmr.failureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c and group bb"
          nmr.negatedFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb"
          nmr.midSentenceFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c and group bb"
          nmr.midSentenceNegatedFailureMessage shouldBe "\"abbbc\" included substring that matched regex a(b*)c, but \"bbb\" did not match group bb"
          nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          nmr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3}"
          nmr.failureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
          nmr.negatedFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))
          nmr.midSentenceFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), UnquotedString("bb"))
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbbc", UnquotedString("a(b*)c"), "bbb", UnquotedString("bb"))

        }
          
      }
      
      val mr3 = mt("ABBC")
      
      "when apply with \"ABBC\"" - {
        
        "should have correct MatcherResult" in {
          mr3.matches shouldBe false
          mr3.failureMessage shouldBe "\"ABBC\" did not include substring that matched regex a(b*)c"
          mr3.negatedFailureMessage shouldBe "\"ABBC\" included substring that matched regex a(b*)c"
          mr3.midSentenceFailureMessage shouldBe "\"ABBC\" did not include substring that matched regex a(b*)c"
          mr3.midSentenceNegatedFailureMessage shouldBe "\"ABBC\" included substring that matched regex a(b*)c"
          mr3.rawFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
          mr3.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
          mr3.rawMidSentenceFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
          mr3.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}"
          mr3.failureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          mr3.negatedFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          mr3.midSentenceFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          mr3.midSentenceNegatedFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))

        }
        
        val nmr = mr3.negated
      
        "should have correct negated MatcherResult" in {
          nmr.matches shouldBe true
          nmr.failureMessage shouldBe "\"ABBC\" included substring that matched regex a(b*)c"
          nmr.negatedFailureMessage shouldBe "\"ABBC\" did not include substring that matched regex a(b*)c"
          nmr.midSentenceFailureMessage shouldBe "\"ABBC\" included substring that matched regex a(b*)c"
          nmr.midSentenceNegatedFailureMessage shouldBe "\"ABBC\" did not include substring that matched regex a(b*)c"
          nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1}"
          nmr.rawNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
          nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not include substring that matched regex {1}"
          nmr.failureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          nmr.negatedFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          nmr.midSentenceFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("ABBC", UnquotedString("a(b*)c"))

        }
      }
    }
    
    "regex(a(b*)(c*) withGroup bb) method returns Matcher" - {
      val bb = "bb"
      val cc = "cc"
      
      val mt = include regex ("""a(b*)(c*)""" withGroups (bb, cc))
      
      "should have pretty toString" in {
        mt.toString should be ("include regex \"a(b*)(c*)\" withGroups (\"" + bb + "\", \"" + cc + "\")")
      }
      
      val mr = mt("abbccc")
      
      "should have correct MatcherResult" in {
        mr.matches shouldBe false
        mr.failureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*), but \"ccc\" did not match group cc at index 1"
        mr.negatedFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*) and group bb, cc"
        mr.midSentenceFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*), but \"ccc\" did not match group cc at index 1"
        mr.midSentenceNegatedFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*) and group bb, cc"
        mr.rawFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3} at index {4}"
        mr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3} at index {4}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
        mr.failureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)
        mr.negatedFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))
        mr.midSentenceFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))

      }
      
      val nmr = mr.negated
      
      "should have correct negated MatcherResult" in {
        nmr.matches shouldBe true
        nmr.failureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*) and group bb, cc"
        nmr.negatedFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*), but \"ccc\" did not match group cc at index 1"
        nmr.midSentenceFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*) and group bb, cc"
        nmr.midSentenceNegatedFailureMessage shouldBe "\"abbccc\" included substring that matched regex a(b*)(c*), but \"ccc\" did not match group cc at index 1"
        nmr.rawFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3} at index {4}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} included substring that matched regex {1} and group {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} included substring that matched regex {1}, but {2} did not match group {3} at index {4}"
        nmr.failureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))
        nmr.negatedFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), UnquotedString("bb, cc"))
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector("abbccc", UnquotedString("a(b*)(c*)"), "ccc", UnquotedString("cc"), 1)

      }
    }
    
  }
  
}