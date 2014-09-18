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
package org.scalatest.matchers

import org.scalatest._
import Inside._
import org.scalactic.{PrettyMethods, Prettifier}

/*
class FactResultSpec extends FreeSpec with Matchers with PrettyMethods {

  "A Fact" - {
    val fact = No("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "can be negated" in {
      fact should equal (No("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
      !fact should equal (Yes("1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
      val fact2 = No("{0} did not equal null", "The reference equaled null", "{0} did not equal null", "the reference equaled null", Vector("howdy"), Vector.empty)
      fact2 should have (
        'failureMessage ("\"howdy\" did not equal null"),
        'negatedFailureMessage ("The reference equaled null"),
        'midSentenceFailureMessage ("\"howdy\" did not equal null"),
        'midSentenceNegatedFailureMessage ("the reference equaled null"),
        'rawFailureMessage ("{0} did not equal null"),
        'rawNegatedFailureMessage ("The reference equaled null"),
        'rawMidSentenceFailureMessage ("{0} did not equal null"),
        'rawMidSentenceNegatedFailureMessage ("the reference equaled null"),
        'failureMessageArgs(Vector("howdy")),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector("howdy")),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val fact2Negated = !fact2
       fact2Negated should equal (Yes("The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
      fact2Negated should have (
        'failureMessage ("The reference equaled null"),
        'negatedFailureMessage ("\"howdy\" did not equal null"),
        'midSentenceFailureMessage ("the reference equaled null"),
        'midSentenceNegatedFailureMessage ("\"howdy\" did not equal null"),
        'rawFailureMessage ("The reference equaled null"),
        'rawNegatedFailureMessage ("{0} did not equal null"),
        'rawMidSentenceFailureMessage ("the reference equaled null"),
        'rawMidSentenceNegatedFailureMessage ("{0} did not equal null"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector("howdy")),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector("howdy"))
      )
    }
    "should construct localized strings from the raw strings and args" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      fact should have (
        'failureMessage ("1 did not equal 2"),
        'negatedFailureMessage ("1 equaled 2"),
        'midSentenceFailureMessage ("1 did not equal 2"),
        'midSentenceNegatedFailureMessage ("1 equaled 2"),
        'rawFailureMessage ("{0} did not equal {1}"),
        'rawNegatedFailureMessage ("{0} equaled {1}"),
        'rawMidSentenceFailureMessage ("{0} did not equal {1}"),
        'rawMidSentenceNegatedFailureMessage ("{0} equaled {1}"),
        'failureMessageArgs(Vector(1, 2)),
        'negatedFailureMessageArgs(Vector(1, 2)),
        'midSentenceFailureMessageArgs(Vector(1, 2)),
        'midSentenceNegatedFailureMessageArgs(Vector(1, 2))
      )
    }

    "should use midSentenceFailureMessageArgs to construct midSentenceFailureMessage" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector(1, 2), Vector.empty, Prettifier.default)
      fact.midSentenceFailureMessage should be ("1 did not equal 2")
    }

    "should use midSentenceNegatedFailureMessageArgs to construct midSentenceNegatedFailureMessage" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", "{0} did not equal {1}", "{0} equaled {1}", Vector.empty, Vector.empty, Vector.empty, Vector(1, 2), Prettifier.default)
      fact.midSentenceNegatedFailureMessage should be ("1 equaled 2")
    }
  }

  "The MatchResult companion object factory method" - {
    "that takes two strings should work correctly" in {
      val fact = Yes("one", "two")
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val ms = No("aaa", "bbb")
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
    }
    "that takes four strings should work correctly" in {
      val fact = Yes("one", "two", "three", "four")
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
      val ms = No("aaa", "bbb", "ccc", "ddd")
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector.empty),
        'negatedFailureMessageArgs(Vector.empty),
        'midSentenceFailureMessageArgs(Vector.empty),
        'midSentenceNegatedFailureMessageArgs(Vector.empty)
      )
    }
    "that takes four strings and two IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", "three", "four", Vector(42), Vector(42.0))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42.0)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42.0))
      )
      val ms = No("aaa", "bbb", "ccc", "ddd", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("foo", "fie")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
    "that takes two strings and one IndexedSeq should work correctly" in {
      val fact = Yes("one", "two", Vector(42))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42))
      )
      val ms = No("aaa", "bbb", Vector("ho", "he"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("ho", "he")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("ho", "he"))
      )
    }
    "that takes two strings and two IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", Vector(42), Vector(42.0))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two"),
        'failureMessageArgs(Vector(42)),
        'negatedFailureMessageArgs(Vector(42.0)),
        'midSentenceFailureMessageArgs(Vector(42)),
        'midSentenceNegatedFailureMessageArgs(Vector(42.0))
      )
      val ms = No("aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb"),
        'failureMessageArgs(Vector("ho", "he")),
        'negatedFailureMessageArgs(Vector("foo", "fie")),
        'midSentenceFailureMessageArgs(Vector("ho", "he")),
        'midSentenceNegatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
    "that takes four strings and four IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", "three", "four", Vector(1), Vector(2), Vector(3), Vector(4))
      fact should have (
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four"),
        'failureMessageArgs(Vector(1)),
        'negatedFailureMessageArgs(Vector(2)),
        'midSentenceFailureMessageArgs(Vector(3)),
        'midSentenceNegatedFailureMessageArgs(Vector(4))
      )
      val ms = No("aaa", "bbb", "ccc", "ddd", Vector('A'), Vector('B'), Vector('C'), Vector('D'))
      ms should have (
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd"),
        'failureMessageArgs(Vector('A')),
        'negatedFailureMessageArgs(Vector('B')),
        'midSentenceFailureMessageArgs(Vector('C')),
        'midSentenceNegatedFailureMessageArgs(Vector('D'))
      )
    }
  }
}
*/
