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

class HaveWordSpec extends FunSpec with Matchers {
  
  describe("HaveWord ") {
    
    it("should have pretty toString") {
      have.toString should be ("have")
    }
    
    describe("length(Long) method returns MatcherFactory1") {
      
      val mtf = have length 3
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("have length 3")
        mt.toString should be ("have length 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        mr.rawFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} had length {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had length {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "Array(1, 2, 3) had length 3"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        nmr.rawFailureMessage shouldBe "{0} had length {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} had length {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)

      }
    }
    
    describe("size(Long) method returns MatcherFactory1") {
      
      val mtf = have size 3
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("have size 3")
        mt.toString should be ("have size 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        mr.rawFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} had size {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had size {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "Array(1, 2, 3) had size 3"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        nmr.rawFailureMessage shouldBe "{0} had size {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} had size {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)

      }
    }
    
    describe("message(String) method returns MatcherFactory1") {
      
      val mtf = have message "Message from Mars!"
      val mt = mtf.matcher[RuntimeException]
      
      it("should have pretty toString") {
        mtf.toString should be ("have message \"Message from Mars!\"")
        mt.toString should be ("have message \"Message from Mars!\"")
      }
      
      val lhs = new RuntimeException("Message from Mars!")
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""
        mr.negatedFailureMessage shouldBe lhs + " had message \"Message from Mars!\""
        mr.midSentenceFailureMessage shouldBe lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""
        mr.midSentenceNegatedFailureMessage shouldBe lhs + " had message \"Message from Mars!\""
        mr.rawFailureMessage shouldBe "{0} had message {1} instead of expected message {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} had message {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} had message {1} instead of expected message {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had message {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, "Message from Mars!", "Message from Mars!")
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!")
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!", "Message from Mars!")
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!")

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe lhs + " had message \"Message from Mars!\""
        nmr.negatedFailureMessage shouldBe lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""
        nmr.midSentenceFailureMessage shouldBe lhs + " had message \"Message from Mars!\""
        nmr.midSentenceNegatedFailureMessage shouldBe lhs + " had message \"Message from Mars!\" instead of expected message \"Message from Mars!\""
        nmr.rawFailureMessage shouldBe "{0} had message {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} had message {1} instead of expected message {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} had message {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had message {1} instead of expected message {2}"
        nmr.failureMessageArgs shouldBe Vector(lhs, "Message from Mars!")
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!", "Message from Mars!")
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!")
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, "Message from Mars!", "Message from Mars!")

      }
    }
    
    describe("apply(ResultOfLengthWordApplication) method returns MatcherFactory1") {
      
      val mtf = have (new ResultOfLengthWordApplication(3))
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("have length 3")
        mt.toString should be ("have length 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        mr.rawFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} had length {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had length {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "Array(1, 2, 3) had length 3"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had length 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had length 3 instead of expected length 3"
        nmr.rawFailureMessage shouldBe "{0} had length {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} had length {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had length {1} instead of expected length {2}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)

      }
    }
    
    describe("size(ResultOfSizeWordApplication) method returns MatcherFactory1") {
      
      val mtf = have (new ResultOfSizeWordApplication(3))
      val mt = mtf.matcher[Array[Int]]
      
      it("should have pretty toString") {
        mtf.toString should be ("have size 3")
        mt.toString should be ("have size 3")
      }
      
      val lhs = Array(1, 2, 3)
      val mr = mt(lhs)
      
      it("should have correct MatcherResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        mr.negatedFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        mr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        mr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        mr.rawFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} had size {1}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had size {1}"
        mr.failureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.negatedFailureMessageArgs shouldBe Vector(lhs, 3)
        mr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3)

      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatcherResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "Array(1, 2, 3) had size 3"
        nmr.negatedFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        nmr.midSentenceFailureMessage shouldBe "Array(1, 2, 3) had size 3"
        nmr.midSentenceNegatedFailureMessage shouldBe "Array(1, 2, 3) had size 3 instead of expected size 3"
        nmr.rawFailureMessage shouldBe "{0} had size {1}"
        nmr.rawNegatedFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} had size {1}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} had size {1} instead of expected size {2}"
        nmr.failureMessageArgs shouldBe Vector(lhs, 3)
        nmr.negatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(lhs, 3)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(lhs, 3, 3)

      }
    }
    
    describe("apply(HavePropertyMatcher) method returns MatcherFactory1") {
      
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
      
      it("should have pretty toString") {
        mt.toString should be ("have (" + nameBob + ")")
      }
      
      describe("when evaluate to true") {
        
        val lhs = Person("Bob")
        val mr = mt(lhs)
      
        it("should have correct MatcherResult") {
          mr.matches shouldBe true
          mr.failureMessage shouldBe "The name property had its expected value \"Bob\", on object " + lhs
          mr.negatedFailureMessage shouldBe "The name property had its expected value \"Bob\", on object " + lhs
          mr.midSentenceFailureMessage shouldBe "the name property had its expected value \"Bob\", on object " + lhs
          mr.midSentenceNegatedFailureMessage shouldBe "the name property had its expected value \"Bob\", on object " + lhs
          mr.rawFailureMessage shouldBe "The {0} property had its expected value {1}, on object {2}"
          mr.rawNegatedFailureMessage shouldBe "The {0} property had its expected value {1}, on object {2}"
          mr.rawMidSentenceFailureMessage shouldBe "the {0} property had its expected value {1}, on object {2}"
          mr.rawMidSentenceNegatedFailureMessage shouldBe "the {0} property had its expected value {1}, on object {2}"
          mr.failureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          mr.negatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          mr.midSentenceFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)

        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe false
          nmr.failureMessage shouldBe "The name property had its expected value \"Bob\", on object " + lhs
          nmr.negatedFailureMessage shouldBe "The name property had its expected value \"Bob\", on object " + lhs
          nmr.midSentenceFailureMessage shouldBe "the name property had its expected value \"Bob\", on object " + lhs
          nmr.midSentenceNegatedFailureMessage shouldBe "the name property had its expected value \"Bob\", on object " + lhs
          nmr.rawFailureMessage shouldBe "The {0} property had its expected value {1}, on object {2}"
          nmr.rawNegatedFailureMessage shouldBe "The {0} property had its expected value {1}, on object {2}"
          nmr.rawMidSentenceFailureMessage shouldBe "the {0} property had its expected value {1}, on object {2}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "the {0} property had its expected value {1}, on object {2}"
          nmr.failureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          nmr.negatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          nmr.midSentenceFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", lhs)

        }
        
      }
      
      describe("when evaluate to false") {
        
        val lhs = Person("Alice")
        val mr = mt(lhs)
      
        it("should have correct MatcherResult") {
          mr.matches shouldBe false
          mr.failureMessage shouldBe "The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          mr.negatedFailureMessage shouldBe "The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          mr.midSentenceFailureMessage shouldBe "the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          mr.midSentenceNegatedFailureMessage shouldBe "the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          mr.rawFailureMessage shouldBe "The {0} property had value {2}, instead of its expected value {1}, on object {3}"
          mr.rawNegatedFailureMessage shouldBe "The {0} property had value {2}, instead of its expected value {1}, on object {3}"
          mr.rawMidSentenceFailureMessage shouldBe "the {0} property had value {2}, instead of its expected value {1}, on object {3}"
          mr.rawMidSentenceNegatedFailureMessage shouldBe "the {0} property had value {2}, instead of its expected value {1}, on object {3}"
          mr.failureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          mr.negatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          mr.midSentenceFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)

        }
      
        val nmr = mr.negated
      
        it("should have correct negated MatcherResult") {
          nmr.matches shouldBe true
          nmr.failureMessage shouldBe "The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          nmr.negatedFailureMessage shouldBe "The name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          nmr.midSentenceFailureMessage shouldBe "the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          nmr.midSentenceNegatedFailureMessage shouldBe "the name property had value \"Alice\", instead of its expected value \"Bob\", on object " + lhs
          nmr.rawFailureMessage shouldBe "The {0} property had value {2}, instead of its expected value {1}, on object {3}"
          nmr.rawNegatedFailureMessage shouldBe "The {0} property had value {2}, instead of its expected value {1}, on object {3}"
          nmr.rawMidSentenceFailureMessage shouldBe "the {0} property had value {2}, instead of its expected value {1}, on object {3}"
          nmr.rawMidSentenceNegatedFailureMessage shouldBe "the {0} property had value {2}, instead of its expected value {1}, on object {3}"
          nmr.failureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          nmr.negatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          nmr.midSentenceFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)
          nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(UnquotedString("name"), "Bob", "Alice", lhs)

        }
        
      }
    }
  }
  
}