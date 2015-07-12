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

import org.scalactic.PrettyMethods
import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber

class ExpectationSpec extends FreeSpec with Matchers with PrettyMethods with ExpectationHavePropertyMatchers {

/*
  "A Expectation" - {
    val fact = False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    "when negated" - {
      "swaps failure and negated failure messages" in {
        fact should equal (False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2"))
        !fact should equal (True("1 equaled 2", "1 did not equal 2", "1 equaled 2", "1 did not equal 2"))
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
         fact2Negated should equal (False("The reference equaled null", "{0} did not equal null", "the reference equaled null", "{0} did not equal null", Vector.empty, Vector("howdy")))
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
        !fact should have (composite(false))
        
        val factCopy = fact.copy(composite = true)
        !factCopy should have (composite(true))
      }
    }
  }


*/
}
