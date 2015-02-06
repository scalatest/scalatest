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
import matchers.{HavePropertyMatcher, HavePropertyMatchResult}

class HaveWordSpec extends Spec with Matchers {
  
  object `HaveWord ` {
    
    def `should have pretty toString` {
      have.toString should be ("have")
    }
    
    object `length(Long) method returns MatcherFactory1` {
      
      val mtf = have length 3
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("have length 3")
        mt.toString should be ("have length 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had length 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had length 3"),
          'rawFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawNegatedFailureMessage ("{0} had length {1}"),
          'rawMidSentenceFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had length {1}"),
          'failureMessageArgs(Vector(lhs, 3, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) had length 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had length 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'rawFailureMessage ("{0} had length {1}"),
          'rawNegatedFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawMidSentenceFailureMessage ("{0} had length {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'failureMessageArgs(Vector(lhs, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3, 3))    
        )
      }
    }
    
    object `size(Long) method returns MatcherFactory1` {
      
      val mtf = have size 3
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("have size 3")
        mt.toString should be ("have size 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had size 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had size 3"),
          'rawFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawNegatedFailureMessage ("{0} had size {1}"),
          'rawMidSentenceFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had size {1}"),
          'failureMessageArgs(Vector(lhs, 3, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) had size 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had size 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'rawFailureMessage ("{0} had size {1}"),
          'rawNegatedFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawMidSentenceFailureMessage ("{0} had size {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'failureMessageArgs(Vector(lhs, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3, 3))    
        )
      }
    }
    
    object `message(String) method returns MatcherFactory1` {
      
      val mtf = have message "Message from Mars!"
      val mt = mtf.matcher[RuntimeException]
      
      def `should have pretty toString` {
        mtf.toString should be ("have message \"Message from Mars!\"")
        mt.toString should be ("have message \"Message from Mars!\"")
      }
      
      val lhs = new RuntimeException("Message from Mars!")
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage (lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""),
          'negatedFailureMessage (lhs + " had message \"Message from Mars!\""),
          'midSentenceFailureMessage (lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""),
          'midSentenceNegatedFailureMessage (lhs + " had message \"Message from Mars!\""),
          'rawFailureMessage ("{0} had message {1} instead of expected message {2}"),
          'rawNegatedFailureMessage ("{0} had message {1}"),
          'rawMidSentenceFailureMessage ("{0} had message {1} instead of expected message {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had message {1}"),
          'failureMessageArgs(Vector(lhs, "Message from Mars!", "Message from Mars!")),
          'negatedFailureMessageArgs(Vector(lhs, "Message from Mars!")),
          'midSentenceFailureMessageArgs(Vector(lhs, "Message from Mars!", "Message from Mars!")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "Message from Mars!"))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage (lhs + " had message \"Message from Mars!\""),
          'negatedFailureMessage (lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""),
          'midSentenceFailureMessage (lhs + " had message \"Message from Mars!\""),
          'midSentenceNegatedFailureMessage (lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""),
          'rawFailureMessage ("{0} had message {1}"),
          'rawNegatedFailureMessage ("{0} had message {1} instead of expected message {2}"),
          'rawMidSentenceFailureMessage ("{0} had message {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had message {1} instead of expected message {2}"),
          'failureMessageArgs(Vector(lhs, "Message from Mars!")),
          'negatedFailureMessageArgs(Vector(lhs, "Message from Mars!", "Message from Mars!")),
          'midSentenceFailureMessageArgs(Vector(lhs, "Message from Mars!")),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, "Message from Mars!", "Message from Mars!"))    
        )
      }
    }
    
    object `apply(ResultOfLengthWordApplication) method returns MatcherFactory1` {
      
      val mtf = have (new ResultOfLengthWordApplication(3))
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("have length 3")
        mt.toString should be ("have length 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had length 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had length 3"),
          'rawFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawNegatedFailureMessage ("{0} had length {1}"),
          'rawMidSentenceFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had length {1}"),
          'failureMessageArgs(Vector(lhs, 3, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) had length 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had length 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had length 3 instead of expected length 3"),
          'rawFailureMessage ("{0} had length {1}"),
          'rawNegatedFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'rawMidSentenceFailureMessage ("{0} had length {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had length {1} instead of expected length {2}"),
          'failureMessageArgs(Vector(lhs, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3, 3))    
        )
      }
    }
    
    object `size(ResultOfSizeWordApplication) method returns MatcherFactory1` {
      
      val mtf = have (new ResultOfSizeWordApplication(3))
      val mt = mtf.matcher[Array[Int]]
      
      def `should have pretty toString` {
        mtf.toString should be ("have size 3")
        mt.toString should be ("have size 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      def `should have correct MatcherResult` {
        mr should have (
          'matches (true),
          'failureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had size 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had size 3"),
          'rawFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawNegatedFailureMessage ("{0} had size {1}"),
          'rawMidSentenceFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had size {1}"),
          'failureMessageArgs(Vector(lhs, 3, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3))    
        )
      }
      
      val nmr = mr.negated
      
      def `should have correct negated MatcherResult` {
        nmr should have (
          'matches (false),
          'failureMessage ("Array(1, 2, 3) had size 3"),
          'negatedFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'midSentenceFailureMessage ("Array(1, 2, 3) had size 3"),
          'midSentenceNegatedFailureMessage ("Array(1, 2, 3) had size 3 instead of expected size 3"),
          'rawFailureMessage ("{0} had size {1}"),
          'rawNegatedFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'rawMidSentenceFailureMessage ("{0} had size {1}"),
          'rawMidSentenceNegatedFailureMessage ("{0} had size {1} instead of expected size {2}"),
          'failureMessageArgs(Vector(lhs, 3)),
          'negatedFailureMessageArgs(Vector(lhs, 3, 3)),
          'midSentenceFailureMessageArgs(Vector(lhs, 3)),
          'midSentenceNegatedFailureMessageArgs(Vector(lhs, 3, 3))    
        )
      }
    }
    
    object `apply(HavePropertyMatcher) method returns MatcherFactory1` {
      
      def name(expectedName: String) = {
        HavePropertyMatcher {
          (person: Person) => HavePropertyMatchResult(
            person.name == expectedName,
            "name",
            expectedName,
            person.name
          )
        }
      }
      
      val nameBob = name("Bob")
      val mt = have (nameBob)
      
      case class Person(name: String)
      
      def `should have pretty toString` {
        mt.toString should be ("have (" + nameBob + ")")
      }
      
      object `when evaluate to true` {
        
        val lhs = Person("Bob")
        val mr = mt(lhs)
      
        def `should have correct MatcherResult` {
          mr should have (
            'matches (true),
            'failureMessage ("The name property had its expected value \"Bob\", on object " + lhs),
            'negatedFailureMessage ("The name property had its expected value \"Bob\", on object " + lhs),
            'midSentenceFailureMessage ("the name property had its expected value \"Bob\", on object " + lhs),
            'midSentenceNegatedFailureMessage ("the name property had its expected value \"Bob\", on object " + lhs),
            'rawFailureMessage ("The {0} property had its expected value {1}, on object {2}"),
            'rawNegatedFailureMessage ("The {0} property had its expected value {1}, on object {2}"),
            'rawMidSentenceFailureMessage ("the {0} property had its expected value {1}, on object {2}"),
            'rawMidSentenceNegatedFailureMessage ("the {0} property had its expected value {1}, on object {2}"),
            'failureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'negatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'midSentenceFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'midSentenceNegatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs))    
          )
        }
      
        val nmr = mr.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (false),
            'failureMessage ("The name property had its expected value \"Bob\", on object " + lhs),
            'negatedFailureMessage ("The name property had its expected value \"Bob\", on object " + lhs),
            'midSentenceFailureMessage ("the name property had its expected value \"Bob\", on object " + lhs),
            'midSentenceNegatedFailureMessage ("the name property had its expected value \"Bob\", on object " + lhs),
            'rawFailureMessage ("The {0} property had its expected value {1}, on object {2}"),
            'rawNegatedFailureMessage ("The {0} property had its expected value {1}, on object {2}"),
            'rawMidSentenceFailureMessage ("the {0} property had its expected value {1}, on object {2}"),
            'rawMidSentenceNegatedFailureMessage ("the {0} property had its expected value {1}, on object {2}"),
            'failureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'negatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'midSentenceFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs)),
            'midSentenceNegatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", lhs))    
          )
        }
        
      }
      
      object `when evaluate to false` {
        
        val lhs = Person("Alice")
        val mr = mt(lhs)
      
        def `should have correct MatcherResult` {
          mr should have (
            'matches (false),
            'failureMessage ("The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'negatedFailureMessage ("The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'midSentenceFailureMessage ("the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'midSentenceNegatedFailureMessage ("the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'rawFailureMessage ("The {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawNegatedFailureMessage ("The {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawMidSentenceFailureMessage ("the {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawMidSentenceNegatedFailureMessage ("the {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'failureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'negatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'midSentenceFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'midSentenceNegatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs))    
          )
        }
      
        val nmr = mr.negated
      
        def `should have correct negated MatcherResult` {
          nmr should have (
            'matches (true),
            'failureMessage ("The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'negatedFailureMessage ("The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'midSentenceFailureMessage ("the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'midSentenceNegatedFailureMessage ("the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs),
            'rawFailureMessage ("The {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawNegatedFailureMessage ("The {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawMidSentenceFailureMessage ("the {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'rawMidSentenceNegatedFailureMessage ("the {0} property had value {2}, instead of its expected value {1}, on object {3}"),
            'failureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'negatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'midSentenceFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs)),
            'midSentenceNegatedFailureMessageArgs(Vector(UnquotedString("name"), "Bob", "Alice", lhs))    
          )
        }
        
      }
    }
  }
  
}