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
import SharedHelpers.createTempDirectory
import Matchers._
import matchers.{BePropertyMatcher, 
                 BePropertyMatchResult, 
                 AMatcher, 
                 AnMatcher, 
                 BeMatcher, 
                 MatchResult}
import matchers.{NegatedFailureMessage, 
                 MidSentenceFailureMessage, 
                 MidSentenceNegatedFailureMessage}
import java.io.File

class NotWordSpec extends Spec with FileMocks {
  
  object `NotWord ` {
    
    def `should have pretty toString` {
      not.toString should be ("not")
    }
    
    object `apply(Matcher) method returns Matcher` {
      
      val mt = not (be < 3)
      
      def `should have pretty toString` {
        mt.toString should be ("not (be < 3)")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("0 was less than 3"),
          'negatedFailureMessage ("0 was not less than 3"),
          'midSentenceFailureMessage ("0 was less than 3"),
          'midSentenceNegatedFailureMessage ("0 was not less than 3"),
          'rawFailureMessage ("{0} was less than {1}"),
          'rawNegatedFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("0 was not less than 3"),
          'negatedFailureMessage ("0 was less than 3"),
          'midSentenceFailureMessage ("0 was not less than 3"),
          'midSentenceNegatedFailureMessage ("0 was less than 3"),
          'rawFailureMessage ("{0} was not less than {1}"),
          'rawNegatedFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `apply(MatcherFactory1) method returns MatcherFactory1` {
      
      val mtf = not (equal (3))
      val mt = mtf.matcher[Int]
      
      def `should have pretty toString` {
        mt.toString should be ("not (equal (3))")
      }
      
      val mr = mt(3)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("3 equaled 3"),
          'negatedFailureMessage ("3 did not equal 3"),
          'midSentenceFailureMessage ("3 equaled 3"),
          'midSentenceNegatedFailureMessage ("3 did not equal 3"),
          'rawFailureMessage ("{0} equaled {1}"),
          'rawNegatedFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1}"),
          'failureMessageArgs(Vector(3, 3)),
          'negatedFailureMessageArgs(Vector(3, 3)),
          'midSentenceFailureMessageArgs(Vector(3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("3 did not equal 3"),
          'negatedFailureMessage ("3 equaled 3"),
          'midSentenceFailureMessage ("3 did not equal 3"),
          'midSentenceNegatedFailureMessage ("3 equaled 3"),
          'rawFailureMessage ("{0} did not equal {1}"),
          'rawNegatedFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
          'failureMessageArgs(Vector(3, 3)),
          'negatedFailureMessageArgs(Vector(3, 3)),
          'midSentenceFailureMessageArgs(Vector(3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3))    
        )
      }
    }
    
    object `apply(MatcherFactory2) method returns MatcherFactory2` {
      
      val mtf1 = equal (3)
      val mt1 = mtf1.matcher[Int]
      val mtf2 = not equal ("3")
      val mt2 = mtf2.matcher[Int]
      val matcherFactory2 = mtf1 and mtf2
      val mtf = not (matcherFactory2)
      val mt = mtf.matcher[Int]
      
      def `should have pretty toString` {
        mt.toString should be ("not (" + matcherFactory2 + ")")
      }
      
      val mr = mt(3)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("3 equaled 3, and 3 did not equal \"3\""),
          'negatedFailureMessage ("3 equaled 3, but 3 equaled \"3\""),
          'midSentenceFailureMessage ("3 equaled 3, and 3 did not equal \"3\""),
          'midSentenceNegatedFailureMessage ("3 equaled 3, but 3 equaled \"3\""),
          'rawFailureMessage ("{0}, and {1}"),
          'rawNegatedFailureMessage ("{0}, but {1}"),
          'rawMidSentenceFailureMessage ("{0}, and {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0}, but {1}"),
          'failureMessageArgs (Vector(NegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))),
          'negatedFailureMessageArgs (Vector(NegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))),
          'midSentenceFailureMessageArgs (Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))),
          'midSentenceNegatedFailureMessageArgs (Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3))))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("3 equaled 3, but 3 equaled \"3\""),
          'negatedFailureMessage ("3 equaled 3, and 3 did not equal \"3\""),
          'midSentenceFailureMessage ("3 equaled 3, but 3 equaled \"3\""),
          'midSentenceNegatedFailureMessage ("3 equaled 3, and 3 did not equal \"3\""),
          'rawFailureMessage ("{0}, but {1}"),
          'rawNegatedFailureMessage ("{0}, and {1}"),
          'rawMidSentenceFailureMessage ("{0}, but {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0}, and {1}"),
          'failureMessageArgs(Vector(NegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))),
          'negatedFailureMessageArgs(Vector(NegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3)))),
          'midSentenceFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceFailureMessage(mt2(3)))),
          'midSentenceNegatedFailureMessageArgs(Vector(MidSentenceNegatedFailureMessage(mt1(3)), MidSentenceNegatedFailureMessage(mt2(3))))    
        )
      }
    }
    
    object `apply(BeMatcher) method returns Matcher` {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = not (odd)
      
      def `should have pretty toString` {
        mt.toString should be ("not (" + odd + ")")
      }
      
      val mr = mt(1)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("1 was odd"),
          'negatedFailureMessage ("1 was even"),
          'midSentenceFailureMessage ("1 was odd"),
          'midSentenceNegatedFailureMessage ("1 was even"),
          'rawFailureMessage ("1 was odd"),
          'rawNegatedFailureMessage ("1 was even"),
          'rawMidSentenceFailureMessage ("1 was odd"),
          'rawMidSentenceNegatedFailureMessage ("1 was even"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("1 was even"),
          'negatedFailureMessage ("1 was odd"),
          'midSentenceFailureMessage ("1 was even"),
          'midSentenceNegatedFailureMessage ("1 was odd"),
          'rawFailureMessage ("1 was even"),
          'rawNegatedFailureMessage ("1 was odd"),
          'rawMidSentenceFailureMessage ("1 was even"),
          'rawMidSentenceNegatedFailureMessage ("1 was odd"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
    }
    
    object `equal(Any) method returns MatcherFactory1` {
      
      val mtf = not equal (3)
      val mt = mtf.matcher[Int]
      
      def `should have pretty toString` {
        mt.toString should be ("not (equal (3))")
      }
      
      val mr = mt(3)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("3 equaled 3"),
          'negatedFailureMessage ("3 did not equal 3"),
          'midSentenceFailureMessage ("3 equaled 3"),
          'midSentenceNegatedFailureMessage ("3 did not equal 3"),
          'rawFailureMessage ("{0} equaled {1}"),
          'rawNegatedFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1}"),
          'failureMessageArgs(Vector(3, 3)),
          'negatedFailureMessageArgs(Vector(3, 3)),
          'midSentenceFailureMessageArgs(Vector(3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("3 did not equal 3"),
          'negatedFailureMessage ("3 equaled 3"),
          'midSentenceFailureMessage ("3 did not equal 3"),
          'midSentenceNegatedFailureMessage ("3 equaled 3"),
          'rawFailureMessage ("{0} did not equal {1}"),
          'rawNegatedFailureMessage ("{0} equaled {1}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
          'failureMessageArgs(Vector(3, 3)),
          'negatedFailureMessageArgs(Vector(3, 3)),
          'midSentenceFailureMessageArgs(Vector(3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3))    
        )
      }
    }
    
    object `equal(Spread) method returns MatcherFactory1` {
      
      val mt = not equal (3 plusOrMinus 1)
      
      def `should have pretty toString` {
        mt.toString should be ("not equal 3 plusOrMinus 1")
      }
      
      val mr = mt(3)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("3 equaled 3 plus or minus 1"),
          'negatedFailureMessage ("3 did not equal 3 plus or minus 1"),
          'midSentenceFailureMessage ("3 equaled 3 plus or minus 1"),
          'midSentenceNegatedFailureMessage ("3 did not equal 3 plus or minus 1"),
          'rawFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'failureMessageArgs(Vector(3, 3, 1)),
          'negatedFailureMessageArgs(Vector(3, 3, 1)),
          'midSentenceFailureMessageArgs(Vector(3, 3, 1)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3, 1))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("3 did not equal 3 plus or minus 1"),
          'negatedFailureMessage ("3 equaled 3 plus or minus 1"),
          'midSentenceFailureMessage ("3 did not equal 3 plus or minus 1"),
          'midSentenceNegatedFailureMessage ("3 equaled 3 plus or minus 1"),
          'rawFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} did not equal {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} equaled {1} plus or minus {2}"),
          'failureMessageArgs(Vector(3, 3, 1)),
          'negatedFailureMessageArgs(Vector(3, 3, 1)),
          'midSentenceFailureMessageArgs(Vector(3, 3, 1)),
          'midSentenceNegatedFailureMessageArgs(Vector(3, 3, 1))    
        )
      }
    }
    
    object `val exists of type MatcherFactory1` {
      
      val tempDir = createTempDirectory()
      val lhs = File.createTempFile("delete", "me", tempDir)
      
      val existVal = not.exist
      val mt = existVal.matcher[File]
      
      def `should have pretty toString` {
        mt.toString should be ("not exist")
      }
      
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " exists"),
          'negatedFailureMessage (lhs + " does not exist"),
          'midSentenceFailureMessage (lhs + " exists"),
          'midSentenceNegatedFailureMessage (lhs + " does not exist"),
          'rawFailureMessage ("{0} exists"),
          'rawNegatedFailureMessage ("{0} does not exist"),
          'rawMidSentenceFailureMessage ("{0} exists"),
          'rawMidSentenceNegatedFailureMessage ("{0} does not exist"),
          'failureMessageArgs(Vector(lhs)),
          'negatedFailureMessageArgs(Vector(lhs)),
          'midSentenceFailureMessageArgs(Vector(lhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " does not exist"),
          'negatedFailureMessage (lhs + " exists"),
          'midSentenceFailureMessage (lhs + " does not exist"),
          'midSentenceNegatedFailureMessage (lhs + " exists"),
          'rawFailureMessage ("{0} does not exist"),
          'rawNegatedFailureMessage ("{0} exists"),
          'rawMidSentenceFailureMessage ("{0} does not exist"),
          'rawMidSentenceNegatedFailureMessage ("{0} exists"),
          'failureMessageArgs(Vector(lhs)),
          'negatedFailureMessageArgs(Vector(lhs)),
          'midSentenceFailureMessageArgs(Vector(lhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs))    
        )
      }
    }
    
    object `equal(null) method returns MatcherFactory1` {
      
      val mt = not equal (null)
      
      def `should have pretty toString` {
        mt.toString should be ("not equal null")
      }
      
      val mr = mt("Bob")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("The reference equaled null"),
          'negatedFailureMessage ("\"Bob\" did not equal null"),
          'midSentenceFailureMessage ("the reference equaled null"),
          'midSentenceNegatedFailureMessage ("\"Bob\" did not equal null"),
          'rawFailureMessage ("The reference equaled null"),
          'rawNegatedFailureMessage ("{0} did not equal null"),
          'rawMidSentenceFailureMessage ("the reference equaled null"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
          'failureMessageArgs(Vector.empty),
          'negatedFailureMessageArgs(Vector("Bob")),
          'midSentenceFailureMessageArgs(Vector.empty),
          'midSentenceNegatedFailureMessageArgs(Vector("Bob"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"Bob\" did not equal null"),
          'negatedFailureMessage ("The reference equaled null"),
          'midSentenceFailureMessage ("\"Bob\" did not equal null"),
          'midSentenceNegatedFailureMessage ("the reference equaled null"),
          'rawFailureMessage ("{0} did not equal null"),
          'rawNegatedFailureMessage ("The reference equaled null"),
          'rawMidSentenceFailureMessage ("{0} did not equal null"),
          'rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
          'failureMessageArgs(Vector("Bob")),
          'negatedFailureMessageArgs(Vector.empty),
          'midSentenceFailureMessageArgs(Vector("Bob")),
          'midSentenceNegatedFailureMessageArgs(Vector.empty)    
        )
      }
    }
    
    object `be(BeMatcher) method returns Matcher` {
      class OddMatcher extends BeMatcher[Int] {
        def apply(left: Int): MatchResult = {
          MatchResult(
            left % 2 == 1,
            left.toString + " was even",
            left.toString + " was odd", 
            Vector(left)
          )
        }
      }
      val odd = new OddMatcher
      
      val mt = not be (odd)
      
      def `should have pretty toString` {
        mt.toString should be ("not be " + odd)
      }
      
      val mr = mt(1)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("1 was odd"),
          'negatedFailureMessage ("1 was even"),
          'midSentenceFailureMessage ("1 was odd"),
          'midSentenceNegatedFailureMessage ("1 was even"),
          'rawFailureMessage ("1 was odd"),
          'rawNegatedFailureMessage ("1 was even"),
          'rawMidSentenceFailureMessage ("1 was odd"),
          'rawMidSentenceNegatedFailureMessage ("1 was even"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("1 was even"),
          'negatedFailureMessage ("1 was odd"),
          'midSentenceFailureMessage ("1 was even"),
          'midSentenceNegatedFailureMessage ("1 was odd"),
          'rawFailureMessage ("1 was even"),
          'rawNegatedFailureMessage ("1 was odd"),
          'rawMidSentenceFailureMessage ("1 was even"),
          'rawMidSentenceNegatedFailureMessage ("1 was odd"),
          'failureMessageArgs(Vector(1)),
          'negatedFailureMessageArgs(Vector(1)),
          'midSentenceFailureMessageArgs(Vector(1)),
          'midSentenceNegatedFailureMessageArgs(Vector(1))    
        )
      }
    }
    
    object `be(null) method returns MatcherFactory1` {
      
      val mt = not be (null)
      
      def `should have pretty toString` {
        mt.toString should be ("not be null")
      }
      
      val mr = mt("Bob")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("The reference was null"),
          'negatedFailureMessage ("\"Bob\" was not null"),
          'midSentenceFailureMessage ("the reference was null"),
          'midSentenceNegatedFailureMessage ("\"Bob\" was not null"),
          'rawFailureMessage ("The reference was null"),
          'rawNegatedFailureMessage ("{0} was not null"),
          'rawMidSentenceFailureMessage ("the reference was null"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not null"),
          'failureMessageArgs(Vector.empty),
          'negatedFailureMessageArgs(Vector("Bob")),
          'midSentenceFailureMessageArgs(Vector.empty),
          'midSentenceNegatedFailureMessageArgs(Vector("Bob"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"Bob\" was not null"),
          'negatedFailureMessage ("The reference was null"),
          'midSentenceFailureMessage ("\"Bob\" was not null"),
          'midSentenceNegatedFailureMessage ("the reference was null"),
          'rawFailureMessage ("{0} was not null"),
          'rawNegatedFailureMessage ("The reference was null"),
          'rawMidSentenceFailureMessage ("{0} was not null"),
          'rawMidSentenceNegatedFailureMessage ("the reference was null"),
          'failureMessageArgs(Vector("Bob")),
          'negatedFailureMessageArgs(Vector.empty),
          'midSentenceFailureMessageArgs(Vector("Bob")),
          'midSentenceNegatedFailureMessageArgs(Vector.empty)    
        )
      }
    }
    
    object `be(ResultOfLessThanComparison) method returns Matcher` {
      
      val mt = not be < (3)
      
      def `should have pretty toString` {
        mt.toString should be ("not be < 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("0 was less than 3"),
          'negatedFailureMessage ("0 was not less than 3"),
          'midSentenceFailureMessage ("0 was less than 3"),
          'midSentenceNegatedFailureMessage ("0 was not less than 3"),
          'rawFailureMessage ("{0} was less than {1}"),
          'rawNegatedFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("0 was not less than 3"),
          'negatedFailureMessage ("0 was less than 3"),
          'midSentenceFailureMessage ("0 was not less than 3"),
          'midSentenceNegatedFailureMessage ("0 was less than 3"),
          'rawFailureMessage ("{0} was not less than {1}"),
          'rawNegatedFailureMessage ("{0} was less than {1}"),
          'rawMidSentenceFailureMessage ("{0} was not less than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was less than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `be(ResultOfGreaterThanComparison) method returns Matcher` {
      
      val mt = not be > (3)
      
      def `should have pretty toString` {
        mt.toString should be ("not be > 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("0 was greater than 3"),
          'negatedFailureMessage ("0 was not greater than 3"),
          'midSentenceFailureMessage ("0 was greater than 3"),
          'midSentenceNegatedFailureMessage ("0 was not greater than 3"),
          'rawFailureMessage ("{0} was greater than {1}"),
          'rawNegatedFailureMessage ("{0} was not greater than {1}"),
          'rawMidSentenceFailureMessage ("{0} was greater than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not greater than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("0 was not greater than 3"),
          'negatedFailureMessage ("0 was greater than 3"),
          'midSentenceFailureMessage ("0 was not greater than 3"),
          'midSentenceNegatedFailureMessage ("0 was greater than 3"),
          'rawFailureMessage ("{0} was not greater than {1}"),
          'rawNegatedFailureMessage ("{0} was greater than {1}"),
          'rawMidSentenceFailureMessage ("{0} was not greater than {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was greater than {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )        
      }
    }
    
    object `be(ResultOfLessThanOrEqualToComparison) method returns Matcher` {
      
      val mt = not be <= (3)
      
      def `should have pretty toString` {
        mt.toString should be ("not be <= 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("0 was less than or equal to 3"),
          'negatedFailureMessage ("0 was not less than or equal to 3"),
          'midSentenceFailureMessage ("0 was less than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was not less than or equal to 3"),
          'rawFailureMessage ("{0} was less than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not less than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was less than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not less than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("0 was not less than or equal to 3"),
          'negatedFailureMessage ("0 was less than or equal to 3"),
          'midSentenceFailureMessage ("0 was not less than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was less than or equal to 3"),
          'rawFailureMessage ("{0} was not less than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was less than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not less than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was less than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `be(ResultOfGreaterThanOrEqualToComparison) method returns Matcher` {
      
      val mt = not be >= (3)
      
      def `should have pretty toString` {
        mt.toString should be ("not be >= 3")
      }
      
      val mr = mt(0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("0 was greater than or equal to 3"),
          'negatedFailureMessage ("0 was not greater than or equal to 3"),
          'midSentenceFailureMessage ("0 was greater than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was not greater than or equal to 3"),
          'rawFailureMessage ("{0} was greater than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was greater than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not greater than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("0 was not greater than or equal to 3"),
          'negatedFailureMessage ("0 was greater than or equal to 3"),
          'midSentenceFailureMessage ("0 was not greater than or equal to 3"),
          'midSentenceNegatedFailureMessage ("0 was greater than or equal to 3"),
          'rawFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawNegatedFailureMessage ("{0} was greater than or equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not greater than or equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was greater than or equal to {1}"),
          'failureMessageArgs(Vector(0, 3)),
          'negatedFailureMessageArgs(Vector(0, 3)),
          'midSentenceFailureMessageArgs(Vector(0, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(0, 3))    
        )
      }
    }
    
    object `beTripleEqualsInvocation) method returns Matcher` {
      
      val mt = not be === ("cheese")
      
      def `should have pretty toString` {
        mt.toString should be ("not be === \"cheese\"")
      }
      
      val mr = mt("chese")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("\"chese\" was equal to \"cheese\""),
          'negatedFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'midSentenceFailureMessage ("\"chese\" was equal to \"cheese\""),
          'midSentenceNegatedFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'rawFailureMessage ("{0} was equal to {1}"),
          'rawNegatedFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
          'failureMessageArgs(Vector("chese", "cheese")),
          'negatedFailureMessageArgs(Vector("che[]se", "che[e]se")),
          'midSentenceFailureMessageArgs(Vector("chese", "cheese")),
          'midSentenceNegatedFailureMessageArgs(Vector("che[]se", "che[e]se"))
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'negatedFailureMessage ("\"chese\" was equal to \"cheese\""),
          'midSentenceFailureMessage ("\"che[]se\" was not equal to \"che[e]se\""),
          'midSentenceNegatedFailureMessage ("\"chese\" was equal to \"cheese\""),
          'rawFailureMessage ("{0} was not equal to {1}"),
          'rawNegatedFailureMessage ("{0} was equal to {1}"),
          'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
          'failureMessageArgs(Vector("che[]se", "che[e]se")),
          'negatedFailureMessageArgs(Vector("chese", "cheese")),
          'midSentenceFailureMessageArgs(Vector("che[]se", "che[e]se")),
          'midSentenceNegatedFailureMessageArgs(Vector("chese", "cheese"))    
        )
      }
    }
    
    object `be(Symbol) method returns Matcher` {
      val mt = not be ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be 'file")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fileMock + " was file"),
          'negatedFailureMessage (fileMock + " was not file"),
          'midSentenceFailureMessage (fileMock + " was file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not file"),
          'rawFailureMessage ("{0} was {1}"),
          'rawNegatedFailureMessage ("{0} was not {1}"),
          'rawMidSentenceFailureMessage ("{0} was {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fileMock + " was not file"),
          'negatedFailureMessage (fileMock + " was file"),
          'midSentenceFailureMessage (fileMock + " was not file"),
          'midSentenceNegatedFailureMessage (fileMock + " was file"),
          'rawFailureMessage ("{0} was not {1}"),
          'rawNegatedFailureMessage ("{0} was {1}"),
          'rawMidSentenceFailureMessage ("{0} was not {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `be(BePropertyMatcher) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be " + file)
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was file"),
          'negatedFailureMessage (myFile + " was not file"),
          'midSentenceFailureMessage (myFile + " was file"),
          'midSentenceNegatedFailureMessage (myFile + " was not file"),
          'rawFailureMessage ("{0} was {1}"),
          'rawNegatedFailureMessage ("{0} was not {1}"),
          'rawMidSentenceFailureMessage ("{0} was {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not file"),
          'negatedFailureMessage (myFile + " was file"),
          'midSentenceFailureMessage (myFile + " was not file"),
          'midSentenceNegatedFailureMessage (myFile + " was file"),
          'rawFailureMessage ("{0} was not {1}"),
          'rawNegatedFailureMessage ("{0} was {1}"),
          'rawMidSentenceFailureMessage ("{0} was not {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `apply(ResultOfAWordToSymbolApplication) method returns Matcher` {
      val mt = not be a ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be a 'file")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fileMock + " was a file"),
          'negatedFailureMessage (fileMock + " was not a file"),
          'midSentenceFailureMessage (fileMock + " was a file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fileMock + " was not a file"),
          'negatedFailureMessage (fileMock + " was a file"),
          'midSentenceFailureMessage (fileMock + " was not a file"),
          'midSentenceNegatedFailureMessage (fileMock + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfAWordToBePropertyMatcherApplication) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be a " + file)
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was a file"),
          'negatedFailureMessage (myFile + " was not a file"),
          'midSentenceFailureMessage (myFile + " was a file"),
          'midSentenceNegatedFailureMessage (myFile + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not a file"),
          'negatedFailureMessage (myFile + " was a file"),
          'midSentenceFailureMessage (myFile + " was not a file"),
          'midSentenceNegatedFailureMessage (myFile + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfAWordToAMatcherApplication) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not be a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be a file")
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was a file"),
          'negatedFailureMessage (myFile + " was not a file"),
          'midSentenceFailureMessage (myFile + " was a file"),
          'midSentenceNegatedFailureMessage (myFile + " was not a file"),
          'rawFailureMessage ("{0} was a {1}"),
          'rawNegatedFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceFailureMessage ("{0} was a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not a file"),
          'negatedFailureMessage (myFile + " was a file"),
          'midSentenceFailureMessage (myFile + " was not a file"),
          'midSentenceNegatedFailureMessage (myFile + " was a file"),
          'rawFailureMessage ("{0} was not a {1}"),
          'rawNegatedFailureMessage ("{0} was a {1}"),
          'rawMidSentenceFailureMessage ("{0} was not a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was a {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfAnWordToSymbolApplication) method returns Matcher` {
      val mt = not be an ('file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be an 'file")
      }
      
      val mr = mt(fileMock)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fileMock + " was an file"),
          'negatedFailureMessage (fileMock + " was not an file"),
          'midSentenceFailureMessage (fileMock + " was an file"),
          'midSentenceNegatedFailureMessage (fileMock + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fileMock + " was not an file"),
          'negatedFailureMessage (fileMock + " was an file"),
          'midSentenceFailureMessage (fileMock + " was not an file"),
          'midSentenceNegatedFailureMessage (fileMock + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(fileMock, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(fileMock, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfAnWordToBePropertyMatcherApplication) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      class FileBePropertyMatcher extends BePropertyMatcher[MyFile] {
        def apply(file: MyFile) = {
          new BePropertyMatchResult(file.file, "file")
        }
      }
      
      val myFile = MyFile("test", true, false)
      val file = new FileBePropertyMatcher
      
      val mt = not be an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be an " + file)
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was an file"),
          'negatedFailureMessage (myFile + " was not an file"),
          'midSentenceFailureMessage (myFile + " was an file"),
          'midSentenceNegatedFailureMessage (myFile + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not an file"),
          'negatedFailureMessage (myFile + " was an file"),
          'midSentenceFailureMessage (myFile + " was not an file"),
          'midSentenceNegatedFailureMessage (myFile + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfAnWordToAnMatcherApplication) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not be an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not be an file")
      }
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was an file"),
          'negatedFailureMessage (myFile + " was not an file"),
          'midSentenceFailureMessage (myFile + " was an file"),
          'midSentenceNegatedFailureMessage (myFile + " was not an file"),
          'rawFailureMessage ("{0} was an {1}"),
          'rawNegatedFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceFailureMessage ("{0} was an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not an file"),
          'negatedFailureMessage (myFile + " was an file"),
          'midSentenceFailureMessage (myFile + " was not an file"),
          'midSentenceNegatedFailureMessage (myFile + " was an file"),
          'rawFailureMessage ("{0} was not an {1}"),
          'rawNegatedFailureMessage ("{0} was an {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString("file")))    
        )
      }
    }
    
    object `be(ResultOfTheSameInstanceAsApplication) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val myFileLeft = MyFile("left", true, false)
      val myFileRight = MyFile("right", true, false)
      
      val mt = not be theSameInstanceAs (myFileRight)
      
      def `should have pretty toString` {
        mt.toString should be ("not be theSameInstanceAs " + myFileRight)
      }
      
      val mr = mt(myFileLeft)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'rawFailureMessage ("{0} was the same instance as {1}"),
          'rawNegatedFailureMessage ("{0} was not the same instance as {1}"),
          'rawMidSentenceFailureMessage ("{0} was the same instance as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not the same instance as {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'negatedFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'midSentenceFailureMessage (myFileLeft + " was not the same instance as " + myFileRight),
          'midSentenceNegatedFailureMessage (myFileLeft + " was the same instance as " + myFileRight),
          'rawFailureMessage ("{0} was not the same instance as {1}"),
          'rawNegatedFailureMessage ("{0} was the same instance as {1}"),
          'rawMidSentenceFailureMessage ("{0} was not the same instance as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was the same instance as {1}"),
          'failureMessageArgs(Vector(myFileLeft, myFileRight)),
          'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
        )
      }
    }
    
    object `be(Spread) method returns Matcher` {
      val spread = 7.1 plusOrMinus 0.2
      val mt = not be (spread)
      
      def `should have pretty toString` {
        mt.toString should be ("not be 7.1 plusOrMinus 0.2")
      }
      
      val mr = mt(7.0)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'negatedFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'midSentenceFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'midSentenceNegatedFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'rawFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not {1} plus or minus {2}"),
          'failureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'negatedFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceNegatedFailureMessageArgs(Vector(7.0, 7.1, 0.2))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'negatedFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'midSentenceFailureMessage ("7.0 was not 7.1 plus or minus 0.2"),
          'midSentenceNegatedFailureMessage ("7.0 was 7.1 plus or minus 0.2"),
          'rawFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawNegatedFailureMessage ("{0} was {1} plus or minus {2}"),
          'rawMidSentenceFailureMessage ("{0} was not {1} plus or minus {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was {1} plus or minus {2}"),
          'failureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'negatedFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceFailureMessageArgs(Vector(7.0, 7.1, 0.2)),
          'midSentenceNegatedFailureMessageArgs(Vector(7.0, 7.1, 0.2))    
        )
      }
    }
    
    object `be(ResultOfDefinedAt) method returns Matcher` {
      
      val fraction = new PartialFunction[Int, Int] {
        def apply(d: Int) = 42 / d
        def isDefinedAt(d: Int) = d != 0
      }
      
      val mt = not be definedAt (8)
      
      def `should have pretty toString` {
        mt.toString should be ("not be definedAt 8")
      }
      
      val mr = mt(fraction)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (fraction + " was defined at 8"),
          'negatedFailureMessage (fraction + " was not defined at 8"),
          'midSentenceFailureMessage (fraction + " was defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was not defined at 8"),
          'rawFailureMessage ("{0} was defined at {1}"),
          'rawNegatedFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (fraction + " was not defined at 8"),
          'negatedFailureMessage (fraction + " was defined at 8"),
          'midSentenceFailureMessage (fraction + " was not defined at 8"),
          'midSentenceNegatedFailureMessage (fraction + " was defined at 8"),
          'rawFailureMessage ("{0} was not defined at {1}"),
          'rawNegatedFailureMessage ("{0} was defined at {1}"),
          'rawMidSentenceFailureMessage ("{0} was not defined at {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was defined at {1}"),
          'failureMessageArgs(Vector(fraction, 8)),
          'negatedFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceFailureMessageArgs(Vector(fraction, 8)),
          'midSentenceNegatedFailureMessageArgs(Vector(fraction, 8))    
        )
      }
    }
    
    object `be(Any) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )

      val myFileRight = MyFile("test right", true, false)
      
      val mt = not be (myFileRight)
      
      def `should have pretty toString` {
        mt.toString should be ("not be " + myFileRight)
      }
      
      object `when left is not null` {
      
        val myFileLeft = MyFile("test left", true, false)
        val mr = mt(myFileLeft)
      
        def `should have correct MatcherResult` {
          mr should have (
            'matches (true),
            'failureMessage (myFileLeft + " was equal to " + myFileRight),
            'negatedFailureMessage (myFileLeft + " was not equal to " + myFileRight),
            'midSentenceFailureMessage (myFileLeft + " was equal to " + myFileRight),
            'midSentenceNegatedFailureMessage (myFileLeft + " was not equal to " + myFileRight),
            'rawFailureMessage ("{0} was equal to {1}"),
            'rawNegatedFailureMessage ("{0} was not equal to {1}"),
            'rawMidSentenceFailureMessage ("{0} was equal to {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} was not equal to {1}"),
            'failureMessageArgs(Vector(myFileLeft, myFileRight)),
            'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
            'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
            'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
          )
        }
      
        val nmr = mr.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage (myFileLeft + " was not equal to " + myFileRight),
            'negatedFailureMessage (myFileLeft + " was equal to " + myFileRight),
            'midSentenceFailureMessage (myFileLeft + " was not equal to " + myFileRight),
            'midSentenceNegatedFailureMessage (myFileLeft + " was equal to " + myFileRight),
            'rawFailureMessage ("{0} was not equal to {1}"),
            'rawNegatedFailureMessage ("{0} was equal to {1}"),
            'rawMidSentenceFailureMessage ("{0} was not equal to {1}"),
            'rawMidSentenceNegatedFailureMessage ("{0} was equal to {1}"),
            'failureMessageArgs(Vector(myFileLeft, myFileRight)),
            'negatedFailureMessageArgs(Vector(myFileLeft, myFileRight)),
            'midSentenceFailureMessageArgs(Vector(myFileLeft, myFileRight)),
            'midSentenceNegatedFailureMessageArgs(Vector(myFileLeft, myFileRight))    
          )
        }
      }
      
      object `when left is null` {
      
        val myFileLeft: MyFile = null
        val mr = mt(myFileLeft)
      
        def `should have correct MatcherResult` {
          mr should have (
            'matches (true),
            'failureMessage ("The reference was null"),
            'negatedFailureMessage (myFileRight + " was not null"),
            'midSentenceFailureMessage ("the reference was null"),
            'midSentenceNegatedFailureMessage (myFileRight + " was not null"),
            'rawFailureMessage ("The reference was null"),
            'rawNegatedFailureMessage ("{0} was not null"),
            'rawMidSentenceFailureMessage ("the reference was null"),
            'rawMidSentenceNegatedFailureMessage ("{0} was not null"),
            'failureMessageArgs(Vector.empty),
            'negatedFailureMessageArgs(Vector(myFileRight)),
            'midSentenceFailureMessageArgs(Vector.empty),
            'midSentenceNegatedFailureMessageArgs(Vector(myFileRight))    
          )
        }
      
        val nmr = mr.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage (myFileRight + " was not null"),
            'negatedFailureMessage ("The reference was null"),
            'midSentenceFailureMessage (myFileRight + " was not null"),
            'midSentenceNegatedFailureMessage ("the reference was null"),
            'rawFailureMessage ("{0} was not null"),
            'rawNegatedFailureMessage ("The reference was null"),
            'rawMidSentenceFailureMessage ("{0} was not null"),
            'rawMidSentenceNegatedFailureMessage ("the reference was null"),
            'failureMessageArgs(Vector(myFileRight)),
            'negatedFailureMessageArgs(Vector.empty),
            'midSentenceFailureMessageArgs(Vector(myFileRight)),
            'midSentenceNegatedFailureMessageArgs(Vector.empty)    
          )
        }
      }
    }
    
    object `be(SortedWord) method returns MatcherFactory` {
      
      val mtf = not be (sorted)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not (be (sorted))")
        mt.toString should be ("not (be (sorted))")
      }
      
      val leftList = List(1, 2, 3)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " was sorted"),
          'negatedFailureMessage (leftList + " was not sorted"),
          'midSentenceFailureMessage (leftList + " was sorted"),
          'midSentenceNegatedFailureMessage (leftList + " was not sorted"),
          'rawFailureMessage ("{0} was sorted"),
          'rawNegatedFailureMessage ("{0} was not sorted"),
          'rawMidSentenceFailureMessage ("{0} was sorted"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not sorted"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " was not sorted"),
          'negatedFailureMessage (leftList + " was sorted"),
          'midSentenceFailureMessage (leftList + " was not sorted"),
          'midSentenceNegatedFailureMessage (leftList + " was sorted"),
          'rawFailureMessage ("{0} was not sorted"),
          'rawNegatedFailureMessage ("{0} was sorted"),
          'rawMidSentenceFailureMessage ("{0} was not sorted"),
          'rawMidSentenceNegatedFailureMessage ("{0} was sorted"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
    }
    
    object `be(ReadableWord) method returns MatcherFactory` {
      
      class MyFile {
        def isReadable: Boolean = true
      }
      
      val mtf = not be (readable)
      val mt = mtf.matcher[MyFile]
      
      def `should have pretty toString` {
        mtf.toString should be ("not (be (readable))")
        mt.toString should be ("not (be (readable))")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was readable"),
          'negatedFailureMessage (myFile + " was not readable"),
          'midSentenceFailureMessage (myFile + " was readable"),
          'midSentenceNegatedFailureMessage (myFile + " was not readable"),
          'rawFailureMessage ("{0} was readable"),
          'rawNegatedFailureMessage ("{0} was not readable"),
          'rawMidSentenceFailureMessage ("{0} was readable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not readable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not readable"),
          'negatedFailureMessage (myFile + " was readable"),
          'midSentenceFailureMessage (myFile + " was not readable"),
          'midSentenceNegatedFailureMessage (myFile + " was readable"),
          'rawFailureMessage ("{0} was not readable"),
          'rawNegatedFailureMessage ("{0} was readable"),
          'rawMidSentenceFailureMessage ("{0} was not readable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was readable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
    }
    
    object `be(WritableWord) method returns MatcherFactory` {
      
      class MyFile {
        def isWritable: Boolean = true
      }
      
      val mtf = not be (writable)
      val mt = mtf.matcher[MyFile]
      
      def `should have pretty toString` {
        mtf.toString should be ("not (be (writable))")
        mt.toString should be ("not (be (writable))")
      }
      
      val myFile = new MyFile
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was writable"),
          'negatedFailureMessage (myFile + " was not writable"),
          'midSentenceFailureMessage (myFile + " was writable"),
          'midSentenceNegatedFailureMessage (myFile + " was not writable"),
          'rawFailureMessage ("{0} was writable"),
          'rawNegatedFailureMessage ("{0} was not writable"),
          'rawMidSentenceFailureMessage ("{0} was writable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not writable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not writable"),
          'negatedFailureMessage (myFile + " was writable"),
          'midSentenceFailureMessage (myFile + " was not writable"),
          'midSentenceNegatedFailureMessage (myFile + " was writable"),
          'rawFailureMessage ("{0} was not writable"),
          'rawNegatedFailureMessage ("{0} was writable"),
          'rawMidSentenceFailureMessage ("{0} was not writable"),
          'rawMidSentenceNegatedFailureMessage ("{0} was writable"),
          'failureMessageArgs(Vector(myFile)),
          'negatedFailureMessageArgs(Vector(myFile)),
          'midSentenceFailureMessageArgs(Vector(myFile)),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile))    
        )
      }
    }
    
    object `be(EmptyWord) method returns MatcherFactory` {
      
      val mtf = not be (empty)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not (be (empty))")
        mt.toString should be ("not (be (empty))")
      }
      
      val leftList = List.empty[Int]
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " was empty"),
          'negatedFailureMessage (leftList + " was not empty"),
          'midSentenceFailureMessage (leftList + " was empty"),
          'midSentenceNegatedFailureMessage (leftList + " was not empty"),
          'rawFailureMessage ("{0} was empty"),
          'rawNegatedFailureMessage ("{0} was not empty"),
          'rawMidSentenceFailureMessage ("{0} was empty"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not empty"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " was not empty"),
          'negatedFailureMessage (leftList + " was empty"),
          'midSentenceFailureMessage (leftList + " was not empty"),
          'midSentenceNegatedFailureMessage (leftList + " was empty"),
          'rawFailureMessage ("{0} was not empty"),
          'rawNegatedFailureMessage ("{0} was empty"),
          'rawMidSentenceFailureMessage ("{0} was not empty"),
          'rawMidSentenceNegatedFailureMessage ("{0} was empty"),
          'failureMessageArgs(Vector(leftList)),
          'negatedFailureMessageArgs(Vector(leftList)),
          'midSentenceFailureMessageArgs(Vector(leftList)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList))    
        )
      }
    }
    
    object `be(DefinedWord) method returns MatcherFactory` {
      
      val mtf = not be (defined)
      val mt = mtf.matcher[Option[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not (be (defined))")
        mt.toString should be ("not (be (defined))")
      }
      
      val leftOption = Some(1)
      val mr = mt(leftOption)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftOption + " was defined"),
          'negatedFailureMessage (leftOption + " was not defined"),
          'midSentenceFailureMessage (leftOption + " was defined"),
          'midSentenceNegatedFailureMessage (leftOption + " was not defined"),
          'rawFailureMessage ("{0} was defined"),
          'rawNegatedFailureMessage ("{0} was not defined"),
          'rawMidSentenceFailureMessage ("{0} was defined"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not defined"),
          'failureMessageArgs(Vector(leftOption)),
          'negatedFailureMessageArgs(Vector(leftOption)),
          'midSentenceFailureMessageArgs(Vector(leftOption)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftOption + " was not defined"),
          'negatedFailureMessage (leftOption + " was defined"),
          'midSentenceFailureMessage (leftOption + " was not defined"),
          'midSentenceNegatedFailureMessage (leftOption + " was defined"),
          'rawFailureMessage ("{0} was not defined"),
          'rawNegatedFailureMessage ("{0} was defined"),
          'rawMidSentenceFailureMessage ("{0} was not defined"),
          'rawMidSentenceNegatedFailureMessage ("{0} was defined"),
          'failureMessageArgs(Vector(leftOption)),
          'negatedFailureMessageArgs(Vector(leftOption)),
          'midSentenceFailureMessageArgs(Vector(leftOption)),
          'midSentenceNegatedFailureMessageArgs(Vector(leftOption))    
        )
      }
    }
    
    object `be(ResultOfATypeInvocation) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAType = new ResultOfATypeInvocation(clazz)
      
      val mt = not be (resultOfAType)
      
      def `should have pretty toString` {
        mt.toString should be ("not be a " + clazz.getName)
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was not an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was not an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
    }
    
    object `be(ResultOfAnTypeInvocation) method returns Matcher` {
      
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val clazz = classOf[MyFile]
      val resultOfAnType = new ResultOfAnTypeInvocation(clazz)
      
      val mt = not be (resultOfAnType)
      
      def `should have pretty toString` {
        mt.toString should be ("not be an " + clazz.getName)
      }
      
      val myFile = new MyFile("test", true, false)
      
      val mr = mt(myFile)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (myFile + " was an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was not an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was not an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (myFile + " was not an instance of " + clazz.getName),
          'negatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'midSentenceFailureMessage (myFile + " was not an instance of " + clazz.getName),
          'midSentenceNegatedFailureMessage (myFile + " was an instance of " + clazz.getName),
          'rawFailureMessage ("{0} was not an instance of {1}"),
          'rawNegatedFailureMessage ("{0} was an instance of {1}"),
          'rawMidSentenceFailureMessage ("{0} was not an instance of {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} was an instance of {1}"),
          'failureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'negatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName))),
          'midSentenceNegatedFailureMessageArgs(Vector(myFile, UnquotedString(clazz.getName)))    
        )
      }
    }
    
    object `fullyMatch(ResultOfRegexWordApplication) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not fullyMatch regex (decimal)
      
      def `should have pretty toString` {
        mt.toString should be ("not fullyMatch regex " + decimal)
      }
      
      val mr = mt("2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
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
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
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
      
    }
    
    object `include(ResultOfRegexWordApplication) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not include regex (decimal)
      
      def `should have pretty toString` {
        mt.toString should be ("not include regex " + decimal)
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"b2.7\" included substring that matched regex " + decimal),
          'negatedFailureMessage ("\"b2.7\" did not include substring that matched regex " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" included substring that matched regex " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" did not include substring that matched regex " + decimal),
          'rawFailureMessage ("{0} included substring that matched regex {1}"),
          'rawNegatedFailureMessage ("{0} did not include substring that matched regex {1}"),
          'rawMidSentenceFailureMessage ("{0} included substring that matched regex {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not include substring that matched regex {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"b2.7\" did not include substring that matched regex " + decimal),
          'negatedFailureMessage ("\"b2.7\" included substring that matched regex " + decimal),
          'midSentenceFailureMessage ("\"b2.7\" did not include substring that matched regex " + decimal),
          'midSentenceNegatedFailureMessage ("\"b2.7\" included substring that matched regex " + decimal),
          'rawFailureMessage ("{0} did not include substring that matched regex {1}"),
          'rawNegatedFailureMessage ("{0} included substring that matched regex {1}"),
          'rawMidSentenceFailureMessage ("{0} did not include substring that matched regex {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} included substring that matched regex {1}"),
          'failureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'negatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceFailureMessageArgs(Vector("b2.7", UnquotedString(decimal))),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", UnquotedString(decimal)))    
        )
      }
    }
    
    object `include(String) method returns Matcher` {
      
      val decimal = "2.7"
      val mt = not include (decimal)
      
      def `should have pretty toString` {
        mt.toString should be ("not include \"2.7\"")
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"b2.7\" included substring \"2.7\""),
          'negatedFailureMessage ("\"b2.7\" did not include substring \"2.7\""),
          'midSentenceFailureMessage ("\"b2.7\" included substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"b2.7\" did not include substring \"2.7\""),
          'rawFailureMessage ("{0} included substring {1}"),
          'rawNegatedFailureMessage ("{0} did not include substring {1}"),
          'rawMidSentenceFailureMessage ("{0} included substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not include substring {1}"),
          'failureMessageArgs(Vector("b2.7", "2.7")),
          'negatedFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", "2.7"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"b2.7\" did not include substring \"2.7\""),
          'negatedFailureMessage ("\"b2.7\" included substring \"2.7\""),
          'midSentenceFailureMessage ("\"b2.7\" did not include substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"b2.7\" included substring \"2.7\""),
          'rawFailureMessage ("{0} did not include substring {1}"),
          'rawNegatedFailureMessage ("{0} included substring {1}"),
          'rawMidSentenceFailureMessage ("{0} did not include substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} included substring {1}"),
          'failureMessageArgs(Vector("b2.7", "2.7")),
          'negatedFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", "2.7"))    
        )
      }
    }
    
    object `startWith(ResultOfRegexWordApplication) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not startWith regex (decimal)
      
      def `should have pretty toString` {
        mt.toString should be ("not startWith regex " + decimal)
      }
      
      val mr = mt("2.7b")
      
      def `should have correct MatcherResult` {
        mr should have (
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
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
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
    }
    
    object `startWith(String) method returns Matcher` {
      
      val mt = not startWith ("2.7")
      
      def `should have pretty toString` {
        mt.toString should be ("not startWith \"2.7\"")
      }
      
      val mr = mt("2.7b")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"2.7b\" started with substring \"2.7\""),
          'negatedFailureMessage ("\"2.7b\" did not start with substring \"2.7\""),
          'midSentenceFailureMessage ("\"2.7b\" started with substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"2.7b\" did not start with substring \"2.7\""),
          'rawFailureMessage ("{0} started with substring {1}"),
          'rawNegatedFailureMessage ("{0} did not start with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} started with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not start with substring {1}"),
          'failureMessageArgs(Vector("2.7b", "2.7")),
          'negatedFailureMessageArgs(Vector("2.7b", "2.7")),
          'midSentenceFailureMessageArgs(Vector("2.7b", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", "2.7"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"2.7b\" did not start with substring \"2.7\""),
          'negatedFailureMessage ("\"2.7b\" started with substring \"2.7\""),
          'midSentenceFailureMessage ("\"2.7b\" did not start with substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"2.7b\" started with substring \"2.7\""),
          'rawFailureMessage ("{0} did not start with substring {1}"),
          'rawNegatedFailureMessage ("{0} started with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} did not start with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} started with substring {1}"),
          'failureMessageArgs(Vector("2.7b", "2.7")),
          'negatedFailureMessageArgs(Vector("2.7b", "2.7")),
          'midSentenceFailureMessageArgs(Vector("2.7b", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("2.7b", "2.7"))    
        )
      }
    }
    
    object `endWith(ResultOfRegexWordApplication) method returns Matcher` {
      
      val decimal = """(-)?(\d+)(\.\d*)?"""
      val mt = not endWith regex (decimal)
      
      def `should have pretty toString` {
        mt.toString should be ("not endWith regex " + decimal)
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
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
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
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
    }
    
    object `endWith(String) method returns Matcher` {
      
      val mt = not endWith ("2.7")
      
      def `should have pretty toString` {
        mt.toString should be ("not endWith \"2.7\"")
      }
      
      val mr = mt("b2.7")
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("\"b2.7\" ended with substring \"2.7\""),
          'negatedFailureMessage ("\"b2.7\" did not end with substring \"2.7\""),
          'midSentenceFailureMessage ("\"b2.7\" ended with substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"b2.7\" did not end with substring \"2.7\""),
          'rawFailureMessage ("{0} ended with substring {1}"),
          'rawNegatedFailureMessage ("{0} did not end with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} ended with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not end with substring {1}"),
          'failureMessageArgs(Vector("b2.7", "2.7")),
          'negatedFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", "2.7"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("\"b2.7\" did not end with substring \"2.7\""),
          'negatedFailureMessage ("\"b2.7\" ended with substring \"2.7\""),
          'midSentenceFailureMessage ("\"b2.7\" did not end with substring \"2.7\""),
          'midSentenceNegatedFailureMessage ("\"b2.7\" ended with substring \"2.7\""),
          'rawFailureMessage ("{0} did not end with substring {1}"),
          'rawNegatedFailureMessage ("{0} ended with substring {1}"),
          'rawMidSentenceFailureMessage ("{0} did not end with substring {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} ended with substring {1}"),
          'failureMessageArgs(Vector("b2.7", "2.7")),
          'negatedFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceFailureMessageArgs(Vector("b2.7", "2.7")),
          'midSentenceNegatedFailureMessageArgs(Vector("b2.7", "2.7"))    
        )
      }
    }
    
    object `contain(T) method returns MatcherFactory1` {
      
      val mtf = not contain (2)
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain 2")
        mt.toString should be ("not contain 2")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) contained element 2"),
          'negatedFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'midSentenceFailureMessage ("Array(1, 2, 3) contained element 2"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'rawFailureMessage ("{0} contained element {1}"),
          'rawNegatedFailureMessage ("{0} did not contain element {1}"),
          'rawMidSentenceFailureMessage ("{0} contained element {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain element {1}"),
          'failureMessageArgs(Vector(lhs, 2)),
          'negatedFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 2))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) did not contain element 2"),
          'negatedFailureMessage ("Array(1, 2, 3) contained element 2"),
          'midSentenceFailureMessage ("Array(1, 2, 3) did not contain element 2"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) contained element 2"),
          'rawFailureMessage ("{0} did not contain element {1}"),
          'rawNegatedFailureMessage ("{0} contained element {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain element {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained element {1}"),
          'failureMessageArgs(Vector(lhs, 2)),
          'negatedFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceFailureMessageArgs(Vector(lhs, 2)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 2))    
        )
      }
    }
    
    object `contain(ResultOfOneOfApplication) method returns MatcherFactory1` {
      
      val mtf = not contain oneOf (2, 8)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain oneOf (2, 8)")
        mt.toString should be ("not contain oneOf (2, 8)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained one of (2, 8)"),
          'negatedFailureMessage (lhs + " did not contain one of (2, 8)"),
          'midSentenceFailureMessage (lhs + " contained one of (2, 8)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain one of (2, 8)"),
          'rawFailureMessage ("{0} contained one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain one of (2, 8)"),
          'negatedFailureMessage (lhs + " contained one of (2, 8)"),
          'midSentenceFailureMessage (lhs + " did not contain one of (2, 8)"),
          'midSentenceNegatedFailureMessage (lhs + " contained one of (2, 8)"),
          'rawFailureMessage ("{0} did not contain one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("2, 8"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("2, 8")))    
        )
      }
    }
    
    object `contain(ResultOfAtLeastOneOfApplication) method returns MatcherFactory1` {
      
      val mtf = not contain atLeastOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain atLeastOneOf (1, 2)")
        mt.toString should be ("not contain atLeastOneOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained at least one of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained at least one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'rawFailureMessage ("{0} contained at least one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained at least one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain at least one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain at least one of (1, 2)"),
          'negatedFailureMessage (lhs + " contained at least one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain at least one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained at least one of (1, 2)"),
          'rawFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained at least one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain at least one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained at least one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfNoneOfApplication) method returns MatcherFactory1` {
      
      val mtf = not contain noneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain noneOf (1, 2)")
        mt.toString should be ("not contain noneOf (1, 2)")
      }
      
      val lhs = List(7, 8, 9)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " did not contain one of (1, 2)"),
          'negatedFailureMessage (lhs + " contained one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained one of (1, 2)"),
          'rawFailureMessage ("{0} did not contain one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " contained one of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain one of (1, 2)"),
          'rawFailureMessage ("{0} contained one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfTheSameElementsAsApplication) method returns MatcherFactory1` {
      
      val rhs = List(1, 2, 3)
      val mtf = not contain theSameElementsAs (rhs)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain theSameElementsAs List(1, 2, 3)")
        mt.toString should be ("not contain theSameElementsAs List(1, 2, 3)")
      }
      
      val lhs = List(3, 2, 1)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained the same elements as " + rhs),
          'negatedFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'midSentenceFailureMessage (lhs + " contained the same elements as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'rawFailureMessage ("{0} contained the same elements as {1}"),
          'rawNegatedFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawMidSentenceFailureMessage ("{0} contained the same elements as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain the same elements as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain the same elements as " + rhs),
          'negatedFailureMessage (lhs + " contained the same elements as " + rhs),
          'midSentenceFailureMessage (lhs + " did not contain the same elements as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " contained the same elements as " + rhs),
          'rawFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawNegatedFailureMessage ("{0} contained the same elements as {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain the same elements as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained the same elements as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
    }
    
    object `contain(ResultOfTheSameElementsInOrderAsApplication) method returns MatcherFactory1` {
      
      val rhs = List(1, 2, 3)
      val mtf = not contain theSameElementsInOrderAs (rhs)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain theSameElementsInOrderAs List(1, 2, 3)")
        mt.toString should be ("not contain theSameElementsInOrderAs List(1, 2, 3)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'negatedFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'midSentenceFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'rawFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawNegatedFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'negatedFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'midSentenceFailureMessage (lhs + " did not contain the same elements in the same (iterated) order as " + rhs),
          'midSentenceNegatedFailureMessage (lhs + " contained the same elements in the same (iterated) order as " + rhs),
          'rawFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawNegatedFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain the same elements in the same (iterated) order as {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained the same elements in the same (iterated) order as {1}"),
          'failureMessageArgs(Vector(lhs, rhs)),
          'negatedFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceFailureMessageArgs(Vector(lhs, rhs)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, rhs))    
        )
      }
    }
    
    object `contain(ResultOfOnlyApplication) method returns MatcherFactory1` {
      
      val mtf = not contain only (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain only (1, 2)")
        mt.toString should be ("not contain only (1, 2)")
      }
      
      val lhs = List(2, 1)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained only (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain only (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained only (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain only (1, 2)"),
          'rawFailureMessage ("{0} contained only ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain only ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained only ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain only ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain only (1, 2)"),
          'negatedFailureMessage (lhs + " contained only (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain only (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained only (1, 2)"),
          'rawFailureMessage ("{0} did not contain only ({1})"),
          'rawNegatedFailureMessage ("{0} contained only ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain only ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained only ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfInOrderOnlyApplication) method returns MatcherFactory1` {
      
      val mtf = not contain inOrderOnly (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain inOrderOnly (1, 2)")
        mt.toString should be ("not contain inOrderOnly (1, 2)")
      }
      
      val lhs = List(1, 2)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained only (1, 2) in order"),
          'negatedFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " contained only (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'rawFailureMessage ("{0} contained only ({1}) in order"),
          'rawNegatedFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} contained only ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain only ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain only (1, 2) in order"),
          'negatedFailureMessage (lhs + " contained only (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " did not contain only (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " contained only (1, 2) in order"),
          'rawFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawNegatedFailureMessage ("{0} contained only ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} did not contain only ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained only ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfAllOfApplication) method returns MatcherFactory1` {
      
      val mtf = not contain allOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain allOf (1, 2)")
        mt.toString should be ("not contain allOf (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained all of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain all of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained all of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain all of (1, 2)"),
          'rawFailureMessage ("{0} contained all of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain all of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained all of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain all of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain all of (1, 2)"),
          'negatedFailureMessage (lhs + " contained all of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain all of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained all of (1, 2)"),
          'rawFailureMessage ("{0} did not contain all of ({1})"),
          'rawNegatedFailureMessage ("{0} contained all of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain all of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained all of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfInOrderApplication) method returns MatcherFactory1` {
      
      val mtf = not contain inOrder (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain inOrder (1, 2)")
        mt.toString should be ("not contain inOrder (1, 2)")
      }
      
      val lhs = List(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained all of (1, 2) in order"),
          'negatedFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " contained all of (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'rawFailureMessage ("{0} contained all of ({1}) in order"),
          'rawNegatedFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} contained all of ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain all of ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain all of (1, 2) in order"),
          'negatedFailureMessage (lhs + " contained all of (1, 2) in order"),
          'midSentenceFailureMessage (lhs + " did not contain all of (1, 2) in order"),
          'midSentenceNegatedFailureMessage (lhs + " contained all of (1, 2) in order"),
          'rawFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawNegatedFailureMessage ("{0} contained all of ({1}) in order"),
          'rawMidSentenceFailureMessage ("{0} did not contain all of ({1}) in order"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained all of ({1}) in order"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfAtMostOneOfApplication) method returns MatcherFactory1` {
      
      val mtf = not contain atMostOneOf (1, 2)
      val mt = mtf.matcher[List[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("not contain atMostOneOf (1, 2)")
        mt.toString should be ("not contain atMostOneOf (1, 2)")
      }
      
      val lhs = List(1, 6, 8)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained at most one of (1, 2)"),
          'negatedFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " contained at most one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'rawFailureMessage ("{0} contained at most one of ({1})"),
          'rawNegatedFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} contained at most one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain at most one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain at most one of (1, 2)"),
          'negatedFailureMessage (lhs + " contained at most one of (1, 2)"),
          'midSentenceFailureMessage (lhs + " did not contain at most one of (1, 2)"),
          'midSentenceNegatedFailureMessage (lhs + " contained at most one of (1, 2)"),
          'rawFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawNegatedFailureMessage ("{0} contained at most one of ({1})"),
          'rawMidSentenceFailureMessage ("{0} did not contain at most one of ({1})"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained at most one of ({1})"),
          'failureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'negatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceFailureMessageArgs(Vector(lhs, UnquotedString("1, 2"))),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, UnquotedString("1, 2")))    
        )
      }
    }
    
    object `contain(ResultOfKeyWordApplication) method returns MatcherFactory1` {
      
      val mtf = not contain key ("2")
      val mt = mtf.matcher[Map[String, String]]
      
      def `should have pretty toString` {
        mt.toString should be ("not contain key \"2\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained key \"2\""),
          'negatedFailureMessage (lhs + " did not contain key \"2\""),
          'midSentenceFailureMessage (lhs + " contained key \"2\""),
          'midSentenceNegatedFailureMessage (lhs + " did not contain key \"2\""),
          'rawFailureMessage ("{0} contained key {1}"),
          'rawNegatedFailureMessage ("{0} did not contain key {1}"),
          'rawMidSentenceFailureMessage ("{0} contained key {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain key {1}"),
          'failureMessageArgs(Vector(lhs, "2")),
          'negatedFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "2"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain key \"2\""),
          'negatedFailureMessage (lhs + " contained key \"2\""),
          'midSentenceFailureMessage (lhs + " did not contain key \"2\""),
          'midSentenceNegatedFailureMessage (lhs + " contained key \"2\""),
          'rawFailureMessage ("{0} did not contain key {1}"),
          'rawNegatedFailureMessage ("{0} contained key {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain key {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained key {1}"),
          'failureMessageArgs(Vector(lhs, "2")),
          'negatedFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceFailureMessageArgs(Vector(lhs, "2")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "2"))    
        )
      }
    }
    
    object `contain(ResultOfValueWordApplication) method returns MatcherFactory1` {
      
      val mtf = not contain value ("two")
      val mt = mtf.matcher[Map[String, String]]
      
      def `should have pretty toString` {
        mt.toString should be ("not contain value \"two\"")
      }
      
      val lhs = Map("1" -> "one", "2" -> "two", "3" -> "three")
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (lhs + " contained value \"two\""),
          'negatedFailureMessage (lhs + " did not contain value \"two\""),
          'midSentenceFailureMessage (lhs + " contained value \"two\""),
          'midSentenceNegatedFailureMessage (lhs + " did not contain value \"two\""),
          'rawFailureMessage ("{0} contained value {1}"),
          'rawNegatedFailureMessage ("{0} did not contain value {1}"),
          'rawMidSentenceFailureMessage ("{0} contained value {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain value {1}"),
          'failureMessageArgs(Vector(lhs, "two")),
          'negatedFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "two"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (lhs + " did not contain value \"two\""),
          'negatedFailureMessage (lhs + " contained value \"two\""),
          'midSentenceFailureMessage (lhs + " did not contain value \"two\""),
          'midSentenceNegatedFailureMessage (lhs + " contained value \"two\""),
          'rawFailureMessage ("{0} did not contain value {1}"),
          'rawNegatedFailureMessage ("{0} contained value {1}"),
          'rawMidSentenceFailureMessage ("{0} did not contain value {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained value {1}"),
          'failureMessageArgs(Vector(lhs, "two")),
          'negatedFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceFailureMessageArgs(Vector(lhs, "two")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "two"))    
        )
      }
    }
    
    object `contain(ResultOfAWordToAMatcherApplication) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not contain a (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not contain a file")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'negatedFailureMessage (leftList + " did not contain a file"),
          'midSentenceFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'midSentenceNegatedFailureMessage (leftList + " did not contain a file"),
          'rawFailureMessage ("{0} contained a {1}: {2}"),
          'rawNegatedFailureMessage ("{0} did not contain a {1}"),
          'rawMidSentenceFailureMessage ("{0} contained a {1}: {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain a {1}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " did not contain a file"),
          'negatedFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'midSentenceFailureMessage (leftList + " did not contain a file"),
          'midSentenceNegatedFailureMessage (leftList + " contained a file: " + myFile + " was a file"),
          'rawFailureMessage ("{0} did not contain a {1}"),
          'rawNegatedFailureMessage ("{0} contained a {1}: {2}"),
          'rawMidSentenceFailureMessage ("{0} did not contain a {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained a {1}: {2}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was a file")))    
        )
      }
    }
    
    object `contain(ResultOfAnWordToAnMatcherApplication) method returns Matcher` {
      case class MyFile(
        val name: String,
        val file: Boolean,
        val isDirectory: Boolean
      )
      
      val file = AnMatcher[MyFile]("file") { _.file  }
      val myFile = MyFile("test", true, false)
      
      val mt = not contain an (file)
      
      def `should have pretty toString` {
        mt.toString should be ("not contain an file")
      }
      
      val leftList = List(myFile)
      val mr = mt(leftList)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (false),
          'failureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'negatedFailureMessage (leftList + " did not contain an file"),
          'midSentenceFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'midSentenceNegatedFailureMessage (leftList + " did not contain an file"),
          'rawFailureMessage ("{0} contained an {1}: {2}"),
          'rawNegatedFailureMessage ("{0} did not contain an {1}"),
          'rawMidSentenceFailureMessage ("{0} contained an {1}: {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} did not contain an {1}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file")))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (true),
          'failureMessage (leftList + " did not contain an file"),
          'negatedFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'midSentenceFailureMessage (leftList + " did not contain an file"),
          'midSentenceNegatedFailureMessage (leftList + " contained an file: " + myFile + " was an file"),
          'rawFailureMessage ("{0} did not contain an {1}"),
          'rawNegatedFailureMessage ("{0} contained an {1}: {2}"),
          'rawMidSentenceFailureMessage ("{0} did not contain an {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} contained an {1}: {2}"),
          'failureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'negatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file"))),
          'midSentenceFailureMessageArgs(Vector(leftList, UnquotedString("file"))),
          'midSentenceNegatedFailureMessageArgs(Vector(leftList, UnquotedString("file"), UnquotedString(myFile + " was an file")))    
        )
      }
    }
    
  }
  
}