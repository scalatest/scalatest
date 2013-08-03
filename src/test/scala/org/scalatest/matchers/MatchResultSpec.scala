/*
 * Copyright 2001-2008 Artima, Inc.
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

class MatchResultSpec extends FreeSpec with Matchers {

  /*
  These are now actually the raw ones.
  */

  "A MatchResult" - {
    val mr = MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "can be negated" in {
      mr should equal (MatchResult(false, "1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
      mr.negated should equal (MatchResult(true, "1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
    }
    "can be pattern matched via an extractor for the failureMessage if it doesn't match" in {
      inside (mr) { case MatchFailed(failureMessage) => 
          failureMessage should be ("1 did not equal 2")
      }
    }
    "can be pattern matched via an extractor for the negatedFailureMessage if it does match" in {
      inside (mr.negated) { case MatchSucceeded(negatedFailureMessage) => 
          negatedFailureMessage should be ("1 did not equal 2")
      }
    }
  }

  "The MatchResult companion object factory method" - {
    "that takes two strings works correctly" in {
      val mr = MatchResult(true, "one", "two")
      mr should have (
        'matches (true),
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("one"),
        'rawMidSentenceNegatedFailureMessage ("two")// ,
        // 'failureMessageArgs(Seq.empty),
        // 'negatedFailureMessageArgs(Seq.empty)
      )
      val ms = MatchResult(false, "aaa", "bbb")
      ms should have (
        'matches (false),
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("aaa"),
        'rawMidSentenceNegatedFailureMessage ("bbb")// ,
        // 'failureMessageArgs(Seq.empty),
        // 'negatedFailureMessageArgs(Seq.empty)
      )
    }
    "that takes four strings works correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four")
      mr should have (
        'matches (true),
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four")// ,
        // 'failureMessageArgs(Seq.empty),
        // 'negatedFailureMessageArgs(Seq.empty)
      )
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd")
      ms should have (
        'matches (false),
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd")// ,
        // 'failureMessageArgs(Seq.empty),
        // 'negatedFailureMessageArgs(Seq.empty)
      )
    }
    "that takes six strings works correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four"/*, Vector(42), Vector(42.0) */)
      mr should have (
        'matches (true),
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four"),
        'rawFailureMessage ("one"),
        'rawNegatedFailureMessage ("two"),
        'rawMidSentenceFailureMessage ("three"),
        'rawMidSentenceNegatedFailureMessage ("four")// ,
        // 'failureMessageArgs(Vector(42)),
        // 'negatedFailureMessageArgs(Vector(42.0))
      )
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd"/*, Vector("ho", "he"), Vector("foo", "fie")*/)
      ms should have (
        'matches (false),
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd"),
        'rawFailureMessage ("aaa"),
        'rawNegatedFailureMessage ("bbb"),
        'rawMidSentenceFailureMessage ("ccc"),
        'rawMidSentenceNegatedFailureMessage ("ddd")// ,
        // 'failureMessageArgs(Vector("ho", "he")),
        // 'negatedFailureMessageArgs(Vector("foo", "fie"))
      )
    }
  }
}
 
