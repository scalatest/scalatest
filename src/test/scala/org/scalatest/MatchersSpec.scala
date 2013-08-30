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

    object `ResultOfBeWordForAny ` {
      val word = 1 should be
      def `should have pretty toString` {
        word.toString should be ("ResultOfBeWordForAny(1, true)")
      }
    }

    object `ResultOfIncludeWordForString ` {
      val word = "Bob" should include
      def `should have pretty toString` {
        word.toString should be ("ResultOfIncludeWordForString(\"Bob\", true)")
      }
    }

    object `ResultOfStartWithWordForString ` {
      val word = "Bob" should startWith
      def `should have pretty toString` {
        word.toString should be ("ResultOfStartWithWordForString(\"Bob\", true)")
      }
    }

    object `ResultOfEndWithWordForString ` {
      val word = "Bob" should endWith
      def `should have pretty toString` {
        word.toString should be ("ResultOfEndWithWordForString(\"Bob\", true)")
      }
    }

    object `ResultOfFullyMatchWordForString ` {
      val word = "Bob" should fullyMatch
      def `should have pretty toString` {
        word.toString should be ("ResultOfFullyMatchWordForString(\"Bob\", true)")
      }
    }
    
    object `RegexWord ` {
      
      def `should have pretty toString` {
        regex.toString should be ("regex")
      }
      
    }

    object `KeyWord ` {
      def `should have pretty toString` {
        key.toString should be ("key")
      }
    }

    object `ValueWord ` {
      def `should have pretty toString` {
        value.toString should be ("value")
      }
    }

    object `AWord ` {
      def `should have pretty toString` {
        val aWord = new AWord
        aWord.toString should be ("a")
      }
    }

    object `AnWord ` {
      def `should have pretty toString` {
        val anWord = new AnWord
        anWord.toString should be ("an")
      }
    }

    object `TheSameInstanceAsPhrase ` {
      def `should have pretty toString` {
        theSameInstanceAs.toString should be ("theSameInstanceAs")
      }
    }

    object `ResultOfHaveWordForExtent ` {
      def `should have pretty toString` {
        val word = "Bob" should have
        word.toString should be ("ResultOfHaveWordForExtent(\"Bob\", true)")
      }
    }

    object `ResultOfEvaluatingApplication ` {
      def `should have pretty toString` {
        val word = evaluating { "hi".charAt(-1) }
        word.toString should be ("evaluating { ... }")
      }
    }

    object `ResultOfProduceInvocation ` {
      def `should have pretty toString` {
        val word = produce [StringIndexOutOfBoundsException]
        word.toString should be ("ResultOfProduceInvocation(classOf[java.lang.StringIndexOutOfBoundsException])")
      }
    }

    object `ResultOfNotWordForCollectedAny ` {
      def `should have pretty toString` {
        val word = all(List(1, 2, 3)) should not
        word.toString should be ("ResultOfNotWordForCollectedAny(AllCollected, List(1, 2, 3), false)")
      }
    }

    object `ResultOfNotWordForCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should not
        word.toString should be ("ResultOfNotWordForCollectedString(AllCollected, List(1, 2, 3), false)")
      }
    }

    object `ResultOfContainWordForCollectedAny ` {
      def `should have pretty toString` {
        val word = all(List(List("1"), List("2"), List("3"))) should contain
        word.toString should be ("ResultOfContainWordForCollectedAny(AllCollected, List(List(1), List(2), List(3)), true)")
      }
    }

    object `ResultOfBeWordForCollectedAny ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should be
        word.toString should be ("ResultOfBeWordForCollectedAny(AllCollected, List(1, 2, 3), true)")
      }
    }

    object `ResultOfBeWordForCollectedArray ` {
      def `should have pretty toString` {
        val a1 = Array("1")
        val a2 = Array("2")
        val a3 = Array("3")
        val word = all(List(a1, a2, a3)) should be
          word.toString should be ("ResultOfBeWordForCollectedAny(AllCollected, List(" + a1 + ", " + a2 + ", " + a3 + "), true)")
      }
    }

    object `ResultOfCollectedAny ` {
      def `should have pretty toString` {
        val word = all(List(1, 2, 3))
        word.toString should be ("ResultOfCollectedAny(AllCollected, List(1, 2, 3))")
      }
    }

    object `ResultOfHaveWordForCollectedExtent ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should have
        word.toString should be ("ResultOfHaveWordForCollectedExtent(AllCollected, List(1, 2, 3), true)")
      }
    }

    object `ResultOfCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3"))
        word.toString should be ("ResultOfCollectedString(AllCollected, List(1, 2, 3))")
      }
    }

    object `ResultOfStartWithWordForCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should startWith
        word.toString should be ("ResultOfStartWithWordForCollectedString(AllCollected, List(1, 2, 3), true)")
      }
    }

    object `ResultOfIncludeWordForCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should include
        word.toString should be ("ResultOfIncludeWordForCollectedString(AllCollected, List(1, 2, 3), true)")
      }
    }

    object `ResultOfEndWithWordForCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should endWith
        word.toString should be ("ResultOfEndWithWordForCollectedString(AllCollected, List(1, 2, 3), true)")
      }
    }

    object `ResultOfFullyMatchWordForCollectedString ` {
      def `should have pretty toString` {
        val word = all(List("1", "2", "3")) should fullyMatch
        word.toString should be ("ResultOfFullyMatchWordForCollectedString(AllCollected, List(1, 2, 3), true)")
      }
    }
  }
  
}