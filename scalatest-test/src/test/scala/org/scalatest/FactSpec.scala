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

/*
import org.scalactic.PrettyMethods
import org.scalatest.exceptions.TestFailedException
import SharedHelpers.thisLineNumber
import Fact._
import prop.TableDrivenPropertyChecks._
import prop.TableFor1

class FactSpec extends FreeSpec with Matchers with PrettyMethods with ExpectationHavePropertyMatchers {

  "A Fact" - {
    val falseFact: Expectation = False("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
    val trueFact: Expectation = True("1 did not equal 2", "1 equaled 2", "1 did not equal 2", "1 equaled 2")
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
        
        val factCopy = falseFact.asInstanceOf[False].copy(composite = true)
        !factCopy should have (composite(true))
      }
      "should return the original object when negated yet again" in {
        val notTrueFact = !trueFact
        !notTrueFact should be theSameInstanceAs trueFact
      }
      "should return the opposite of underlying from its isTrue method" in {
        val notTrueFact = !trueFact
        notTrueFact.isTrue should equal (!(trueFact.isTrue))
      }
      "should return the opposite of underlying from its isFalse method" in {
        val notTrueFact = !trueFact
        notTrueFact.isFalse should equal (!(trueFact.isFalse))
      }
    }
  }
  "The Fact obtained from and-ing two Facts" - {
    "should be lazy about constructing strings" - {
      "for False && False" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'))
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideFalse && rightSideFalse
        fact shouldBe a [False]
        fact.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.failureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('a', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('a', 'b'))
        fact.composite should be (false)
      }

      "for False && True" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'))
        val rightSideTrue = True(Resources.rawWasNotLessThan, Resources.rawWasLessThan, Resources.rawWasNotLessThan, Resources.rawWasLessThan, Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideFalse && rightSideTrue
        fact shouldBe a [False]
        fact.rawFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.rawMidSentenceFailureMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawWasGreaterThan)
        fact.failureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.negatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.midSentenceFailureMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.midSentenceNegatedFailureMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.failureMessageArgs should be (Vector('a', 'b'))
        fact.negatedFailureMessageArgs should be (Vector('a', 'b'))
        fact.composite should be (false)
      }

      "for True && False" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'),Vector('c', 'b'))
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideTrue && rightSideFalse
        fact shouldBe a [Binary_&&]
        fact.isFalse shouldBe true
        fact.rawFailureMessage should be (Resources.rawCommaBut)
        fact.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawCommaBut)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.failureMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('c'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('c'.pretty, 'd'.pretty)))
        fact.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(NegatedFailureMessage(leftSideTrue), MidSentenceFailureMessage(rightSideFalse)))
        fact.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(leftSideTrue), MidSentenceNegatedFailureMessage(rightSideFalse)))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(leftSideTrue), MidSentenceFailureMessage(rightSideFalse)))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(leftSideTrue), MidSentenceNegatedFailureMessage(rightSideFalse)))
        fact.composite should be (true)
      }

      "for True && True" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'))
        val rightSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideTrue && rightSideTrue
        fact shouldBe a [Binary_&&]
        fact.isTrue shouldBe true
        fact.rawFailureMessage should be (Resources.rawCommaBut)
        fact.rawNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawCommaBut)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawCommaAnd)
        fact.failureMessage should be (Resources.commaBut(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('e'.pretty, 'd'.pretty)))
        fact.negatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.midSentenceFailureMessage should be (Resources.commaBut(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('e'.pretty, 'd'.pretty))) 
        fact.midSentenceNegatedFailureMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.failureMessageArgs should be (Vector(NegatedFailureMessage(leftSideTrue), MidSentenceFailureMessage(rightSideTrue)))
        fact.negatedFailureMessageArgs should be (Vector(NegatedFailureMessage(leftSideTrue), MidSentenceNegatedFailureMessage(rightSideTrue)))
        fact.midSentenceFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(leftSideTrue), MidSentenceFailureMessage(rightSideTrue)))
        fact.midSentenceNegatedFailureMessageArgs should be (Vector(MidSentenceNegatedFailureMessage(leftSideTrue), MidSentenceNegatedFailureMessage(rightSideTrue)))
        fact.composite should be (true)
      }
    }

    "should be parenthesize composite facts" - {
      "for non-composite && composite" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), false)
        val rightSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), true)
        val fact = leftSideTrue && rightSideTrue
        fact.rawFailureMessage should be (Resources.rawRightParensCommaBut)
        fact.rawNegatedFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawRightParensCommaBut)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawRightParensCommaAnd)
        fact.composite should be (true)
      }

      "for composite && non-composite" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), true)
        val rightSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), false)
        val fact = leftSideTrue && rightSideTrue
        fact.rawFailureMessage should be (Resources.rawLeftParensCommaBut)
        fact.rawNegatedFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawLeftParensCommaBut)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawLeftParensCommaAnd)
        fact.composite should be (true)
      }

      "for composite && composite" in {
        val leftSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'),Vector('e', 'b'), true)
        val rightSideTrue = True(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'),Vector('e', 'd'), true)
        val fact = leftSideTrue && rightSideTrue
        fact.rawFailureMessage should be (Resources.rawBothParensCommaBut)
        fact.rawNegatedFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.rawMidSentenceFailureMessage should be (Resources.rawBothParensCommaBut)
        fact.rawMidSentenceNegatedFailureMessage should be (Resources.rawBothParensCommaAnd)
        fact.composite should be (true)
      }
    }
  }
  "The Expectation obtained from or-ing two Expectations" - {
    "should be lazy about constructing strings" - {

      "for False || False" in {
        val leftSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'),Vector('a', 'b'))
        val rightSideFalse = False(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideFalse || rightSideFalse
        fact shouldBe a [Binary_||]
        fact.isFalse shouldBe true
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
        fact shouldBe a [Binary_||]
        fact.isTrue shouldBe true
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

  def examples: TableFor1[Expectation] =
    Table(
      "fact",
      False("message", "negated message"),
      True("message", "negated message"),
      !(False("message", "negated message")),
      !(True("message", "negated message")),
      False("message", "negated message") && True("message", "negated message"),
      True("message", "negated message") && True("message", "negated message"),
      True("message", "negated message") && False("message", "negated message"),
      True("message", "negated message") || False("message", "negated message"),
      False("message", "negated message") || True("message", "negated message"),
      False("message", "negated message") || False("message", "negated message")
    )
}
*/
