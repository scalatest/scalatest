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


  "The Expectation obtained from or-ing two Expectations" - {
    "should be lazy about constructing strings" - {

      "for False || False" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'))
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideFalse || rightSideFalse
        fact shouldBe a [False]
        fact.rawFailureMessage should be (Resources.rawCommaAnd)
        fact.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.failureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(FailureMessage(leftSideFalse), MidSentenceFailureMessage(rightSideFalse)))
        fact.negatedFailureMessageArgs should be (Vector(FailureMessage(leftSideFalse), MidSentenceNegatedFailureMessage(rightSideFalse)))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(leftSideFalse), MidSentenceFailureMessage(rightSideFalse)))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(leftSideFalse), MidSentenceNegatedFailureMessage(rightSideFalse)))
        fact.composite should be (true)
       }

      "for False || True" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'))
        val rightSideTrue = True(Resources.rawWasNotLessThan, Resources.rawWasLessThan, Resources.rawWasNotLessThan, Resources.rawWasLessThan, Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideFalse || rightSideTrue
        fact shouldBe a [True]
        fact.rawFailureMessage should be (Resources.rawCommaAnd)
        fact.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.failureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(FailureMessage(leftSideFalse), MidSentenceFailureMessage(rightSideTrue)))
        fact.negatedFailureMessageArgs should be (Vector(FailureMessage(leftSideFalse), MidSentenceNegatedFailureMessage(rightSideTrue)))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceFailureMessage(leftSideFalse), MidSentenceFailureMessage(rightSideTrue)))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceFailureMessage(leftSideFalse), MidSentenceNegatedFailureMessage(rightSideTrue)))
        fact.composite should be (true)
      }

      "for True || False" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'))
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideTrue || rightSideFalse
        fact shouldBe a [True]
        fact.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.failureMessage should be (Resources.wasNotGreaterThan('c'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources.wasGreaterThan('c'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('c'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('c'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('c', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('c', 'b'))
        fact.composite should be (false)
      }

      "for True || True" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'))
        val rightSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideTrue || rightSideTrue
        fact shouldBe a [True]
        fact.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.failureMessage should be (Resources.wasNotGreaterThan('e'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources.wasGreaterThan('e'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('e'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('e'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('e', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('e', 'b'))
        fact.composite should be (false)
      }
    }

    "should be parenthesize composite facts" - {
      "for non-composite || composite" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), false)
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), true)
        val fact = leftSideFalse || rightSideFalse
        fact.rawFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.rawNegatedFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.composite should be (true)
      }

      "for composite || non-composite" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), true)
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), false)
        val fact = leftSideFalse || rightSideFalse
        fact.rawFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.rawNegatedFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.composite should be (true)
      }

      "for composite || composite" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), true)
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), true)
        val fact = leftSideFalse || rightSideFalse
        fact.rawFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.rawNegatedFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.composite should be (true)
      }
    }
  }
*/
}
