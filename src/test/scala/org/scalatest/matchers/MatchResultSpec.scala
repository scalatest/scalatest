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

class MatchResultSpec extends FreeSpec with ShouldMatchers {

  "The MatchResult companion object factory method" - {
    "that takes two strings works correctly" in {
      val mr = MatchResult(true, "one", "two")
      mr should have (
        'matches (true),
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("one"),
        'midSentenceNegatedFailureMessage ("two")
      )
      val ms = MatchResult(false, "aaa", "bbb")
      ms should have (
        'matches (false),
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("aaa"),
        'midSentenceNegatedFailureMessage ("bbb")
      )
    }
    "that takes four strings works correctly" in {
      val mr = MatchResult(true, "one", "two", "three", "four")
      mr should have (
        'matches (true),
        'failureMessage ("one"),
        'negatedFailureMessage ("two"),
        'midSentenceFailureMessage ("three"),
        'midSentenceNegatedFailureMessage ("four")
      )
      val ms = MatchResult(false, "aaa", "bbb", "ccc", "ddd")
      ms should have (
        'matches (false),
        'failureMessage ("aaa"),
        'negatedFailureMessage ("bbb"),
        'midSentenceFailureMessage ("ccc"),
        'midSentenceNegatedFailureMessage ("ddd")
      )
    }
  }
}
 
