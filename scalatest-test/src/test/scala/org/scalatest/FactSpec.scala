/*
 * Copyright 2001-2015 Artima, Inc.
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

import org.scalactic.PrettyMethods
import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import Fact._

class FactSpec extends FreeSpec with Matchers with PrettyMethods with ExpectationHavePropertyMatchers {
  "A Fact" - {
    val falseFact = False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    val trueFact = True("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "should have isTrue and isFalse methods" in {
      falseFact.isTrue shouldBe false
      falseFact.isFalse shouldBe true
      trueFact.isTrue shouldBe true
      trueFact.isFalse shouldBe false
    }
    "should have a toAssertion method that either returns Succeeded or throws TestFailedException with the correct error message and stack depth" in {
      trueFact.toAssertion shouldBe Succeeded
      val caught = the [TestFailedException] thrownBy falseFact.toAssertion
      caught should have message "1 did not equal 2"
      caught.failedCodeLineNumber shouldEqual Some(thisLineNumber - 2)
      caught.failedCodeFileName shouldBe Some("FactSpec.scala")
    }
    "should offer a toBoolean method, even though it is redundant with isTrue" in {
      falseFact.toBoolean shouldBe false
      trueFact.toBoolean shouldBe true
    }
    "should construct localized strings from the raw strings and args" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      fact should have (
        failureMessage ("1 did not equal 2"),
        negatedFailureMessage ("1 equaled 2"),
        midSentenceFailureMessage ("1 did not equal 2"),
        midSentenceNegatedFailureMessage ("1 equaled 2"),
        rawFailureMessage ("{0} did not equal {1}"),
        rawNegatedFailureMessage ("{0} equaled {1}"),
        rawMidSentenceFailureMessage ("{0} did not equal {1}"),
        rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
        failureMessageArgs(Vector(1, 2)),
        negatedFailureMessageArgs(Vector(1, 2)),
        midSentenceFailureMessageArgs(Vector(1, 2)),
        midSentenceNegatedFailureMessageArgs(Vector(1, 2)),
        composite(false)
      )
    }

    "should use midSentenceFailureMessageArgs to construct midSentenceFailureMessage" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector(1, 2), Vector.empty)
      fact.midSentenceFailureMessage should be ("1 did not equal 2")
    }

    "should use midSentenceNegatedFailureMessageArgs to construct midSentenceNegatedFailureMessage" in {
      val fact = False("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector.empty, Vector(1, 2))
      fact.midSentenceNegatedFailureMessage should be ("1 equaled 2")
    }
    "when negated" - {
      "swaps failure and negated failure messages" in {
        falseFact should equal (False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
        !falseFact should equal (Unary_!(False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")))
        val fact2 = True("{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)
        fact2 should have (
          failureMessage ("\"howdy\" did not equal null"),
          negatedFailureMessage ("The reference equaled null"),
          midSentenceFailureMessage ("\"howdy\" did not equal null"),
          midSentenceNegatedFailureMessage ("the reference equaled null"),
          rawFailureMessage ("{0} did not equal null"),
          rawNegatedFailureMessage ("The reference equaled null"),
          rawMidSentenceFailureMessage ("{0} did not equal null"),
          rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
          failureMessageArgs(Vector("howdy")),
          negatedFailureMessageArgs(Vector.empty),
          midSentenceFailureMessageArgs(Vector("howdy")),
          midSentenceNegatedFailureMessageArgs(Vector.empty),
          composite(false)
        )
        val fact2Negated = !fact2
         fact2Negated should equal (Unary_!(True("{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)))
        fact2Negated should have (
          failureMessage ("The reference equaled null"),
          negatedFailureMessage ("\"howdy\" did not equal null"),
          midSentenceFailureMessage ("the reference equaled null"),
          midSentenceNegatedFailureMessage ("\"howdy\" did not equal null"),
          rawFailureMessage ("The reference equaled null"),
          rawNegatedFailureMessage ("{0} did not equal null"),
          rawMidSentenceFailureMessage ("the reference equaled null"),
          rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
          failureMessageArgs(Vector.empty),
          negatedFailureMessageArgs(Vector("howdy")),
          midSentenceFailureMessageArgs(Vector.empty),
          midSentenceNegatedFailureMessageArgs(Vector("howdy")),
          composite(false)
        )
      }
      "should maintain the same composite state" in {
        !falseFact should have (composite(false))
        
        val factCopy = falseFact.copy(composite = true)
        !factCopy should have (composite(true))
      }
      "should return the original object when negated yet again" in {
        val notTrueFact = !trueFact
        !notTrueFact should be theSameInstanceAs trueFact
      }
    }
  }
  "The True and False companion objects factory methods" - {
    "that takes two strings should work correctly" in {
      val fact = True("one", "two")
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("one"),
        midSentenceNegatedFailureMessage ("two"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("one"),
        rawMidSentenceNegatedFailureMessage ("two"),
        failureMessageArgs(Vector.empty),
        negatedFailureMessageArgs(Vector.empty),
        midSentenceFailureMessageArgs(Vector.empty),
        midSentenceNegatedFailureMessageArgs(Vector.empty),
        composite(false)
      )
      val ms = False("aaa", "bbb")
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("aaa"),
        midSentenceNegatedFailureMessage ("bbb"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("aaa"),
        rawMidSentenceNegatedFailureMessage ("bbb"),
        failureMessageArgs(Vector.empty),
        negatedFailureMessageArgs(Vector.empty),
        midSentenceFailureMessageArgs(Vector.empty),
        midSentenceNegatedFailureMessageArgs(Vector.empty),
        composite(false)
      )
    }
    "that takes four strings should work correctly" in {
      val fact = True("one", "two", "three", "four")
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("three"),
        midSentenceNegatedFailureMessage ("four"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("three"),
        rawMidSentenceNegatedFailureMessage ("four"),
        failureMessageArgs(Vector.empty),
        negatedFailureMessageArgs(Vector.empty),
        midSentenceFailureMessageArgs(Vector.empty),
        midSentenceNegatedFailureMessageArgs(Vector.empty),
        composite(false)
      )
      val ms = False("aaa", "bbb", "ccc", "ddd")
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("ccc"),
        midSentenceNegatedFailureMessage ("ddd"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("ccc"),
        rawMidSentenceNegatedFailureMessage ("ddd"),
        failureMessageArgs(Vector.empty),
        negatedFailureMessageArgs(Vector.empty),
        midSentenceFailureMessageArgs(Vector.empty),
        midSentenceNegatedFailureMessageArgs(Vector.empty),
        composite(false)
      )
    }
    "that takes four strings and two IndexedSeqs should work correctly" in {
      val fact = True("one", "two", "three", "four", Vector(42), Vector(42.0))
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("three"),
        midSentenceNegatedFailureMessage ("four"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("three"),
        rawMidSentenceNegatedFailureMessage ("four"),
        failureMessageArgs(Vector(42)),
        negatedFailureMessageArgs(Vector(42.0)),
        midSentenceFailureMessageArgs(Vector(42)),
        midSentenceNegatedFailureMessageArgs(Vector(42.0)),
        composite(false)
      )
      val ms = False("aaa", "bbb", "ccc", "ddd", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("ccc"),
        midSentenceNegatedFailureMessage ("ddd"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("ccc"),
        rawMidSentenceNegatedFailureMessage ("ddd"),
        failureMessageArgs(Vector("ho", "he")),
        negatedFailureMessageArgs(Vector("foo", "fie")),
        midSentenceFailureMessageArgs(Vector("ho", "he")),
        midSentenceNegatedFailureMessageArgs(Vector("foo", "fie")),
        composite(false)
      )
    }
    "that takes two strings and one IndexedSeq should work correctly" in {
      val fact = True("one", "two", Vector(42))
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("one"),
        midSentenceNegatedFailureMessage ("two"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("one"),
        rawMidSentenceNegatedFailureMessage ("two"),
        failureMessageArgs(Vector(42)),
        negatedFailureMessageArgs(Vector(42)),
        midSentenceFailureMessageArgs(Vector(42)),
        midSentenceNegatedFailureMessageArgs(Vector(42)),
        composite(false)
      )
      val ms = False("aaa", "bbb", Vector("ho", "he"))
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("aaa"),
        midSentenceNegatedFailureMessage ("bbb"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("aaa"),
        rawMidSentenceNegatedFailureMessage ("bbb"),
        failureMessageArgs(Vector("ho", "he")),
        negatedFailureMessageArgs(Vector("ho", "he")),
        midSentenceFailureMessageArgs(Vector("ho", "he")),
        midSentenceNegatedFailureMessageArgs(Vector("ho", "he")),
        composite(false)
      )
    }
    "that takes two strings and two IndexedSeqs should work correctly" in {
      val fact = True("one", "two", Vector(42), Vector(42.0))
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("one"),
        midSentenceNegatedFailureMessage ("two"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("one"),
        rawMidSentenceNegatedFailureMessage ("two"),
        failureMessageArgs(Vector(42)),
        negatedFailureMessageArgs(Vector(42.0)),
        midSentenceFailureMessageArgs(Vector(42)),
        midSentenceNegatedFailureMessageArgs(Vector(42.0)),
        composite(false)
      )
      val ms = False("aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("aaa"),
        midSentenceNegatedFailureMessage ("bbb"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("aaa"),
        rawMidSentenceNegatedFailureMessage ("bbb"),
        failureMessageArgs(Vector("ho", "he")),
        negatedFailureMessageArgs(Vector("foo", "fie")),
        midSentenceFailureMessageArgs(Vector("ho", "he")),
        midSentenceNegatedFailureMessageArgs(Vector("foo", "fie")),
        composite(false)
      )
    }
    "that takes four strings and four IndexedSeqs should work correctly" in {
      val fact = True("one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4))
      fact should have (
        failureMessage ("one"),
        negatedFailureMessage ("two"),
        midSentenceFailureMessage ("three"),
        midSentenceNegatedFailureMessage ("four"),
        rawFailureMessage ("one"),
        rawNegatedFailureMessage ("two"),
        rawMidSentenceFailureMessage ("three"),
        rawMidSentenceNegatedFailureMessage ("four"),
        failureMessageArgs(Vector(1)),
        negatedFailureMessageArgs(Vector(2)),
        midSentenceFailureMessageArgs(Vector(3)),
        midSentenceNegatedFailureMessageArgs(Vector(4)),
        composite(false)
      )
      val ms = False("aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'))
      ms should have (
        failureMessage ("aaa"),
        negatedFailureMessage ("bbb"),
        midSentenceFailureMessage ("ccc"),
        midSentenceNegatedFailureMessage ("ddd"),
        rawFailureMessage ("aaa"),
        rawNegatedFailureMessage ("bbb"),
        rawMidSentenceFailureMessage ("ccc"),
        rawMidSentenceNegatedFailureMessage ("ddd"),
        failureMessageArgs(Vector('A')),
        negatedFailureMessageArgs(Vector('B')),
        midSentenceFailureMessageArgs(Vector('C')),
        midSentenceNegatedFailureMessageArgs(Vector('D')),
        composite(false)
      )
    }
    "that takes four strings, four IndexedSeqs and composite should work correctly" in {
      val fact = True("one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4), true)
          fact should have (
              failureMessage ("one"),
              negatedFailureMessage ("two"),
              midSentenceFailureMessage ("three"),
              midSentenceNegatedFailureMessage ("four"),
              rawFailureMessage ("one"),
              rawNegatedFailureMessage ("two"),
              rawMidSentenceFailureMessage ("three"),
              rawMidSentenceNegatedFailureMessage ("four"),
              failureMessageArgs(Vector(1)),
              negatedFailureMessageArgs(Vector(2)),
              midSentenceFailureMessageArgs(Vector(3)),
              midSentenceNegatedFailureMessageArgs(Vector(4)),
              composite(true)
              )
      val ms = False("aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'), true)
      ms should have (
          failureMessage ("aaa"),
          negatedFailureMessage ("bbb"),
          midSentenceFailureMessage ("ccc"),
          midSentenceNegatedFailureMessage ("ddd"),
          rawFailureMessage ("aaa"),
          rawNegatedFailureMessage ("bbb"),
          rawMidSentenceFailureMessage ("ccc"),
          rawMidSentenceNegatedFailureMessage ("ddd"),
          failureMessageArgs(Vector('A')),
          negatedFailureMessageArgs(Vector('B')),
          midSentenceFailureMessageArgs(Vector('C')),
          midSentenceNegatedFailureMessageArgs(Vector('D')),
          composite(true)
          )
    }
  }
}
