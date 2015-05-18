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
import FailureMessages.decorateToStringValue
import org.scalatest.matchers.MatchResult
import org.scalactic.Prettifier

class MatchersSpec extends FunSpec {
  
  describe("Matchers ") {
    
    describe("equal(Spread) method returns Matcher") {
      
      val mt = equal (8 +- 1)
      
      it("should have pretty toString") {
        mt.toString should be ("equal (8 +- 1)")
      }
      
      val mr = mt(9)
      
      it("should have correct MatchResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "9 did not equal 8 plus or minus 1"
        mr.negatedFailureMessage shouldBe "9 equaled 8 plus or minus 1"
        mr.midSentenceFailureMessage shouldBe "9 did not equal 8 plus or minus 1"
        mr.midSentenceNegatedFailureMessage shouldBe "9 equaled 8 plus or minus 1"
        mr.rawFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        mr.rawNegatedFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        mr.failureMessageArgs shouldBe Vector(9, 8, 1)
        mr.negatedFailureMessageArgs shouldBe Vector(9, 8, 1)
        mr.midSentenceFailureMessageArgs shouldBe Vector(9, 8, 1)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector(9, 8, 1)
      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatchResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "9 equaled 8 plus or minus 1"
        nmr.negatedFailureMessage shouldBe "9 did not equal 8 plus or minus 1"
        nmr.midSentenceFailureMessage shouldBe "9 equaled 8 plus or minus 1"
        nmr.midSentenceNegatedFailureMessage shouldBe "9 did not equal 8 plus or minus 1"
        nmr.rawFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        nmr.rawMidSentenceFailureMessage shouldBe "{0} equaled {1} plus or minus {2}"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal {1} plus or minus {2}"
        nmr.failureMessageArgs shouldBe Vector(9, 8, 1)
        nmr.negatedFailureMessageArgs shouldBe Vector(9, 8, 1)
        nmr.midSentenceFailureMessageArgs shouldBe Vector(9, 8, 1)
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(9, 8, 1)
      }
      
    }
    
    describe("equal(Null) method returns Matcher") {
      
      val mt = equal (null)
      
      it("should have pretty toString") {
        mt.toString should be ("equal (null)")
      }
      
      val mr = mt(null)
      
      it("should have correct MatchResult") {
        mr.matches shouldBe true
        mr.failureMessage shouldBe "null did not equal null"
        mr.negatedFailureMessage shouldBe "The reference equaled null"
        mr.midSentenceFailureMessage shouldBe "null did not equal null"
        mr.midSentenceNegatedFailureMessage shouldBe "the reference equaled null"
        mr.rawFailureMessage shouldBe "{0} did not equal null"
        mr.rawNegatedFailureMessage shouldBe "The reference equaled null"
        mr.rawMidSentenceFailureMessage shouldBe "{0} did not equal null"
        mr.rawMidSentenceNegatedFailureMessage shouldBe "the reference equaled null"
        mr.failureMessageArgs shouldBe Vector(null)
        mr.negatedFailureMessageArgs shouldBe Vector.empty
        mr.midSentenceFailureMessageArgs shouldBe Vector(null)
        mr.midSentenceNegatedFailureMessageArgs shouldBe Vector.empty
      }
      
      val nmr = mr.negated
      
      it("should have correct negated MatchResult") {
        nmr.matches shouldBe false
        nmr.failureMessage shouldBe "The reference equaled null"
        nmr.negatedFailureMessage shouldBe "null did not equal null"
        nmr.midSentenceFailureMessage shouldBe "the reference equaled null"
        nmr.midSentenceNegatedFailureMessage shouldBe "null did not equal null"
        nmr.rawFailureMessage shouldBe "The reference equaled null"
        nmr.rawNegatedFailureMessage shouldBe "{0} did not equal null"
        nmr.rawMidSentenceFailureMessage shouldBe "the reference equaled null"
        nmr.rawMidSentenceNegatedFailureMessage shouldBe "{0} did not equal null"
        nmr.failureMessageArgs shouldBe Vector.empty
        nmr.negatedFailureMessageArgs shouldBe Vector(null)
        nmr.midSentenceFailureMessageArgs shouldBe Vector.empty
        nmr.midSentenceNegatedFailureMessageArgs shouldBe Vector(null)
      }
      
    }

    // SKIP-SCALATESTJS-START
    describe("HavePropertyMatcherGenerator ") {
      
      describe("apply(Any) returns HavePropertyMatcher") {
        
        val generator = new HavePropertyMatcherGenerator('name)
        val havePropMatcher = generator("test")
        
        it("should have pretty toString") {
          havePropMatcher.toString should be ("HavePropertyMatcher[AnyRef, Any](expectedValue = \"test\")")
        }
        
      }
      
    }
    // SKIP-SCALATESTJS-END

    describe("ResultOfBeWordForAny ") {
      val word = 1 should be
      it("should have pretty toString") {
        word.toString should be ("ResultOfBeWordForAny(1, true)")
      }
    }

    describe("ResultOfIncludeWordForString ") {
      val word = "Bob" should include
      it("should have pretty toString") {
        word.toString should be ("ResultOfIncludeWordForString(\"Bob\", true)")
      }
    }

    describe("ResultOfStartWithWordForString ") {
      val word = "Bob" should startWith
      it("should have pretty toString") {
        word.toString should be ("ResultOfStartWithWordForString(\"Bob\", true)")
      }
    }

    describe("ResultOfEndWithWordForString ") {
      val word = "Bob" should endWith
      it("should have pretty toString") {
        word.toString should be ("ResultOfEndWithWordForString(\"Bob\", true)")
      }
    }

    describe("ResultOfFullyMatchWordForString ") {
      val word = "Bob" should fullyMatch
      it("should have pretty toString") {
        word.toString should be ("ResultOfFullyMatchWordForString(\"Bob\", true)")
      }
    }
    
    describe("RegexWord ") {
      
      it("should have pretty toString") {
        regex.toString should be ("regex")
      }
      
    }

    describe("KeyWord ") {
      it("should have pretty toString") {
        key.toString should be ("key")
      }
    }

    describe("ValueWord ") {
      it("should have pretty toString") {
        value.toString should be ("value")
      }
    }

    describe("AWord ") {
      it("should have pretty toString") {
        val aWord = new AWord
        aWord.toString should be ("a")
      }
    }

    describe("AnWord ") {
      it("should have pretty toString") {
        val anWord = new AnWord
        anWord.toString should be ("an")
      }
    }

    describe("TheSameInstanceAsPhrase ") {
      it("should have pretty toString") {
        theSameInstanceAs.toString should be ("theSameInstanceAs")
      }
    }

    describe("ResultOfHaveWordForExtent ") {
      it("should have pretty toString") {
        val word = "Bob" should have
        word.toString should be ("ResultOfHaveWordForExtent(\"Bob\", true)")
      }
    }

    describe("ResultOfProduceInvocation ") {
      it("should have pretty toString") {
        val word = produce [StringIndexOutOfBoundsException]
        word.toString should be ("ResultOfProduceInvocation(classOf[java.lang.StringIndexOutOfBoundsException])")
      }
    }

    describe("ResultOfNotWordForCollectedAny ") {
      it("should have pretty toString") {
        val word = all(List(1, 2, 3)) should not
        word.toString should be ("ResultOfNotWordForCollectedAny(AllCollected, List(1, 2, 3), false)")
      }
    }

    describe("ResultOfContainWordForCollectedAny ") {
      it("should have pretty toString") {
        val word = all(List(List("1"), List("2"), List("3"))) should contain
        word.toString should be ("ResultOfContainWordForCollectedAny(AllCollected, List(List(\"1\"), List(\"2\"), List(\"3\")), true)")
      }
    }

    describe("ResultOfBeWordForCollectedAny ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should be
        word.toString should be ("ResultOfBeWordForCollectedAny(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }

    describe("ResultOfBeWordForCollectedArray ") {
      it("should have pretty toString") {
        val a1 = Array("1")
        val a2 = Array("2")
        val a3 = Array("3")
        val word = all(List(a1, a2, a3)) should be
          word.toString should be ("ResultOfBeWordForCollectedAny(AllCollected, List(" + decorateToStringValue(a1) + ", " + decorateToStringValue(a2) + ", " + decorateToStringValue(a3) + "), true)")
      }
    }

    describe("ResultOfCollectedAny ") {
      it("should have pretty toString") {
        val word = all(List(1, 2, 3))
        word.toString should be ("ResultOfCollectedAny(AllCollected, List(1, 2, 3))")
      }
    }

    describe("ResultOfHaveWordForCollectedExtent ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should have
        word.toString should be ("ResultOfHaveWordForCollectedExtent(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }

    describe("ResultOfStartWithWordForCollectedString ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should startWith
        word.toString should be ("ResultOfStartWithWordForCollectedString(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }

    describe("ResultOfIncludeWordForCollectedString ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should include
        word.toString should be ("ResultOfIncludeWordForCollectedString(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }

    describe("ResultOfEndWithWordForCollectedString ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should endWith
        word.toString should be ("ResultOfEndWithWordForCollectedString(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }

    describe("ResultOfFullyMatchWordForCollectedString ") {
      it("should have pretty toString") {
        val word = all(List("1", "2", "3")) should fullyMatch
        word.toString should be ("ResultOfFullyMatchWordForCollectedString(AllCollected, List(\"1\", \"2\", \"3\"), true)")
      }
    }
  }
  
}
