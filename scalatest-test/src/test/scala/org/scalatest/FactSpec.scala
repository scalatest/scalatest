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


import Fact._
import prop.TableDrivenPropertyChecks._
import org.scalactic.PrettyMethods
import SharedHelpers.thisLineNumber
import matchers.{ FailureMessage, NegatedFailureMessage }
import org.scalatest.exceptions.TestCanceledException
import org.scalatest.exceptions.TestFailedException
import prop.TableFor1


/*
No(
  No(!Yes(3 equaled 3)) &&
  No(4 did not equal 3)
)

No(4 did not equal 3)
*/
class FactSpec extends FreeSpec with Matchers with PrettyMethods with ExpectationHavePropertyMatchers {

  val NEWLINE = scala.compat.Platform.EOL

  "A Fact" - {
    // As if we said expectResult(3) { 1 + 1 }
    val noFact: Expectation = No("Expected 3, but got 2", "3 did not equal 2", Vector.empty, Vector.empty)
    // As if we said expectResult(3) { 1 + 2 }
    val yesFact: Expectation = Yes("Expected 3, and got 3", "3 equaled 3", Vector.empty, Vector.empty)
    "should have isYes and isNo methods" in {
      noFact.isYes shouldBe false
      noFact.isNo shouldBe true
      yesFact.isYes shouldBe true
      yesFact.isNo shouldBe false
    }
    "should give the correct string prefix" in {
      noFact.stringPrefix shouldBe "No"
      yesFact.stringPrefix shouldBe "Yes"
    }
    "should have a toAssertion method that" - {
      "returns Succeeded if the Fact is a non-vacuous Yes" in {
        yesFact.toAssertion shouldBe Succeeded
      }
      "throws TestFailedException with the correct error message and stack depth if the Fact is a No" in {
        val caught = the [TestFailedException] thrownBy noFact.toAssertion
        caught should have message "Expected 3, but got 2"
        caught.failedCodeLineNumber shouldEqual Some(thisLineNumber - 2)
        caught.failedCodeFileName shouldBe Some("FactSpec.scala")
      }
      "throws TestCanceledException with the correct error message and stack depth if the Fact is a vacuous Yes" in {
        import Expectations._
        val x = 1
        val vacuousYes = (expect(x == 2) implies expect(x > 0))
        val caught = the [TestCanceledException] thrownBy vacuousYes.toAssertion
        caught should have message "1 did not equal 2"
        caught.failedCodeLineNumber shouldEqual Some(thisLineNumber - 2)
        caught.failedCodeFileName shouldBe Some("FactSpec.scala")
      }
    }
    "should offer a toBoolean method, even though it is redundant with isYes" in {
      noFact.toBoolean shouldBe false
      yesFact.toBoolean shouldBe true
    }
    "should construct localized strings from the raw strings and args" in {
      val fact = No("{0} did not equal {1}", "{0} equaled {1}", Vector(1, 2), Vector(1, 2))
      fact.factMessage shouldBe ("1 did not equal 2")
      fact.composableFactMessage shouldBe ("1 equaled 2")
      fact.rawFactMessage shouldBe ("{0} did not equal {1}")
      fact.rawComposableFactMessage shouldBe ("{0} equaled {1}")
      fact.factMessageArgs shouldBe (Vector(1, 2))
      fact.composableFactMessageArgs shouldBe (Vector(1, 2))
      fact.isLeaf shouldBe (true)
      fact.isYes shouldBe (false)
      fact.isVacuousYes shouldBe (false)
    }

    "when inverted with !" - {
      val fact2 = No("Expected {0}, but got {1}", "{0} did not equal {1}", Vector(3, 2), Vector(3, 2))
      val notFact2 = !fact2
      "should use the composable message, changing Yes to No or No to Yes" in {
        !noFact should equal (Unary_!(noFact))
        fact2.factMessage shouldBe ("Expected 3, but got 2")
        fact2.composableFactMessage shouldBe ("3 did not equal 2")
        fact2.rawFactMessage shouldBe ("Expected {0}, but got {1}")
        fact2.rawComposableFactMessage shouldBe ("{0} did not equal {1}")
        fact2.factMessageArgs shouldBe (Vector(3, 2))
        fact2.composableFactMessageArgs shouldBe (Vector(3, 2))
        fact2.isLeaf shouldBe (true)
        fact2.isYes shouldBe (false)
        fact2.isVacuousYes shouldBe (false)

        notFact2 should equal (Unary_!(No("Expected {0}, but got {1}", "{0} did not equal {1}", Vector(3, 2), Vector(3, 2))))
        notFact2.factMessage shouldBe ("3 did not equal 2")
        notFact2.composableFactMessage shouldBe ("3 did not equal 2")
        notFact2.rawFactMessage shouldBe ("{0} did not equal {1}")
        notFact2.rawComposableFactMessage shouldBe ("{0} did not equal {1}")
        notFact2.factMessageArgs shouldBe (Vector(3, 2))
        notFact2.composableFactMessageArgs shouldBe (Vector(3, 2))
        notFact2.isLeaf shouldBe (true)
        notFact2.isYes shouldBe (true)
        notFact2.isVacuousYes shouldBe (false)
      }
      "should give back the original Fact instance if inverted twice" in {
        val notNotFact2 = !notFact2
        notNotFact2 should be theSameInstanceAs fact2
      }
      "should maintain the same isLeaf state" in {
        !noFact should have (isLeaf(true))

        !(noFact || yesFact) should have (isLeaf(false))
        !(yesFact && yesFact) should have (isLeaf(false))
      }
      "should return the original object when composable yet again" in {
        val notYesFact = !yesFact
        !notYesFact should be theSameInstanceAs yesFact
      }
      "should return the opposite of underlying from its isYes method" in {
        val notYesFact = !yesFact
        notYesFact.isYes should equal (!(yesFact.isYes))
      }
      "should return the opposite of underlying from its isNo method" in {
        val notYesFact = !yesFact
        notYesFact.isNo should equal (!(yesFact.isNo))
      }
    }
  }

  "The Fact obtained from and-ing two Facts with &&" - {
    "should be lazy about constructing strings" - {
      "for No && No" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo && rightSideNo
        fact shouldBe a [Leaf]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('a', 'b'))
        fact.composableFactMessageArgs should be (Vector('a', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for No && Yes" in {
        // This looks like a bug Both should be was not greater than probably?
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasLessThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo && rightSideYes
        fact shouldBe a [Leaf]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('a', 'b'))
        fact.composableFactMessageArgs should be (Vector('a', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes && No" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideYes && rightSideNo
        fact shouldBe a [Binary_&&]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaBut)
        fact.rawComposableFactMessage should be (Resources.rawCommaBut)
        fact.factMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes && Yes" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideYes && rightSideYes
        fact shouldBe a [Binary_&&]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }
    }

    "should short-circuit correctly" - {
      "for Yes && Yes" in {
        var evaluated = false
        Yes("yes1") && { evaluated = true; Yes("yes2") }
        assert(evaluated)
      }
      "for Yes && No" in {
        var evaluated = false
        Yes("yes1") && { evaluated = true; No("no1") }
        assert(evaluated)
      }
      "for No && Yes" in {
        var evaluated = false
        No("no1") && { evaluated = true; Yes("yes1") }
        assert(!evaluated)
      }
      "for No && No" in {
        var evaluated = false
        No("no1") && { evaluated = true; No("no2") }
        assert(!evaluated)
      }
    }

    "should propagate VacuousYes correctly" - {
      import Expectations._
      val x = 1
      val vacuousYes = (expect(x == 2) implies expect(x > 0))
      "for VacuousYes && VacuousYes" in {
        val result = vacuousYes && vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes && Yes" in {
        val result = vacuousYes && Yes("yes")
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for Yes && VacuousYes" in {
        val result = Yes("yes") && vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes && No" in {
        val result = vacuousYes && No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No && VacuousYes" in {
        val result = No("no") && vacuousYes
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes && Yes" in {
        val result = Yes("yes1") && Yes("yes2")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes && No" in {
        val result = Yes("yes") && No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No && Yes" in {
        val result = No("no") && Yes("yes")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No && No" in {
        val result = No("no1") && No("no2")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
    }
  }

  "The Fact obtained from and-ing two Facts with &" - {
    "should be lazy about constructing strings" - {
      "for No & No" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo & rightSideNo
        fact shouldBe a [Binary_&]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('a'.pretty, 'b'.pretty), Resources.wasGreaterThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for No & Yes" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasLessThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo & rightSideYes
        fact shouldBe a [Binary_&]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('a'.pretty, 'b'.pretty), Resources.wasLessThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes & No" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideYes & rightSideNo
        fact shouldBe a [Binary_&]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaBut)
        fact.rawComposableFactMessage should be (Resources.rawCommaBut)
        fact.factMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes & Yes" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideYes & rightSideYes
        fact shouldBe a [Binary_&]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }
    }

    "should not short-circuit" - {
      "for Yes & Yes" in {
        var evaluated = false
        Yes("yes1") & { evaluated = true; Yes("yes2") }
        assert(evaluated)
      }
      "for Yes & No" in {
        var evaluated = false
        Yes("yes1") & { evaluated = true; No("no1") }
        assert(evaluated)
      }
      "for No & Yes" in {
        var evaluated = false
        No("no1") & { evaluated = true; Yes("yes1") }
        assert(evaluated)
      }
      "for No & No" in {
        var evaluated = false
        No("no1") & { evaluated = true; No("no2") }
        assert(evaluated)
      }
    }

    "should propagate VacuousYes correctly" - {
      import Expectations._
      val x = 1
      val vacuousYes = (expect(x == 2) implies expect(x > 0))
      "for VacuousYes & VacuousYes" in {
        val result = vacuousYes & vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes & Yes" in {
        val result = vacuousYes & Yes("yes")
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for Yes & VacuousYes" in {
        val result = Yes("yes") & vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes & No" in {
        val result = vacuousYes & No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No & VacuousYes" in {
        val result = No("no") & vacuousYes
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes & Yes" in {
        val result = Yes("yes1") & Yes("yes2")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes & No" in {
        val result = Yes("yes") & No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No & Yes" in {
        val result = No("no") & Yes("yes")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No & No" in {
        val result = No("no1") & No("no2")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
    }
  }

  "The Fact obtained from or-ing two Facts with ||" - {
    "should be lazy about constructing strings" - {

      "for No || No" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo || rightSideNo
        fact shouldBe a [Binary_||]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
       }


      "for No || Yes" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasNotLessThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo || rightSideYes
        fact shouldBe a [Binary_||]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes || No" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideYes || rightSideNo
        fact shouldBe a [Leaf]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('c'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('c'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('c', 'b'))
        fact.composableFactMessageArgs should be (Vector('c', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes || Yes" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideYes || rightSideYes
        fact shouldBe a [Leaf]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('e'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('e'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('e', 'b'))
        fact.composableFactMessageArgs should be (Vector('e', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }
    }

    "should short-circuit correctly" - {
      "for Yes || Yes" in {
        var evaluated = false
        Yes("yes1") || { evaluated = true; Yes("yes2") }
        assert(!evaluated)
      }
      "for Yes || No" in {
        var evaluated = false
        Yes("yes1") || { evaluated = true; No("no1") }
        assert(!evaluated)
      }
      "for No || Yes" in {
        var evaluated = false
        No("no1") || { evaluated = true; Yes("yes1") }
        assert(evaluated)
      }
      "for No || No" in {
        var evaluated = false
        No("no1") || { evaluated = true; No("no2") }
        assert(evaluated)
      }
    }

    "should propagate VacuousYes correctly" - {
      import Expectations._
      val x = 1
      val vacuousYes = (expect(x == 2) implies expect(x > 0))
      "for VacuousYes || VacuousYes" in {
        val result = vacuousYes || vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes || Yes" in { // Because it short circuits
        val result = vacuousYes || Yes("yes")
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for Yes || VacuousYes" in {
        val result = Yes("yes") || vacuousYes
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for VacuousYes || No" in {
        val result = vacuousYes || No("no")
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for No || VacuousYes" in {
        val result = No("no") || vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for Yes || Yes" in {
        val result = Yes("yes1") || Yes("yes2")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes || No" in {
        val result = Yes("yes") || No("no")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No || Yes" in {
        val result = No("no") || Yes("yes")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No || No" in {
        val result = No("no1") || No("no2")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
    }

    "The Fact obtained from or-ing two Facts with |" - {
      "should be lazy about constructing strings" - {

        "for No | No" in {
          val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
          val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
          val fact = leftSideNo | rightSideNo
          fact shouldBe a [Binary_|]
          fact.isNo shouldBe true
          fact.rawFactMessage should be (Resources.rawCommaAnd)
          fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
          fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
          fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
          fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
          fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
          fact.isLeaf should be (false)
          fact.isYes shouldBe (false)
          fact.isVacuousYes shouldBe (false)
        }


        "for No | Yes" in {
          val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
          val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasNotLessThan, Vector('a', 'd'),Vector('a', 'd'))
          val fact = leftSideNo | rightSideYes
          fact shouldBe a [Binary_|]
          fact.isYes shouldBe true
          fact.rawFactMessage should be (Resources.rawCommaAnd)
          fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
          fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
          fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
          fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
          fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
          fact.isLeaf should be (false)
          fact.isYes shouldBe (true)
          fact.isVacuousYes shouldBe (false)
        }

        "for Yes | No" in {
          val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
          val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
          val fact = leftSideYes | rightSideNo
          fact shouldBe a [Binary_|]
          fact.rawFactMessage should be (Resources.rawCommaAnd)
          fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
          fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
          fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
          fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
          fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
          fact.isLeaf should be (false)
          fact.isYes shouldBe (true)
          fact.isVacuousYes shouldBe (false)
        }

        "for Yes | Yes" in {
          val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
          val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
          val fact = leftSideYes | rightSideYes
          fact shouldBe a [Binary_|]
          fact.rawFactMessage should be (Resources.rawCommaAnd)
          fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
          fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
          fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
          fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
          fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
          fact.isLeaf should be (false)
          fact.isYes shouldBe (true)
          fact.isVacuousYes shouldBe (false)
        }
      }

      "should short-circuit correctly" - {
        "for Yes | Yes" in {
          var evaluated = false
          Yes("yes1") | { evaluated = true; Yes("yes2") }
          assert(evaluated)
        }
        "for Yes | No" in {
          var evaluated = false
          Yes("yes1") | { evaluated = true; No("no1") }
          assert(evaluated)
        }
        "for No | Yes" in {
          var evaluated = false
          No("no1") | { evaluated = true; Yes("yes1") }
          assert(evaluated)
        }
        "for No | No" in {
          var evaluated = false
          No("no1") | { evaluated = true; No("no2") }
          assert(evaluated)
        }
      }


      "should propagate VacuousYes correctly" - {
        import Expectations._
        val x = 1
        val vacuousYes = (expect(x == 2) implies expect(x > 0))
        "for VacuousYes | VacuousYes" in {
          val result = vacuousYes | vacuousYes
          assert(result.isYes)
          assert(result.isVacuousYes)
        }
        "for VacuousYes | Yes" in { // Because it DOES NOT short circuit
          val result = vacuousYes | Yes("yes")
          assert(result.isYes)
          assert(!result.isVacuousYes)
        }
        "for Yes | VacuousYes" in {
          val result = Yes("yes") | vacuousYes
          assert(result.isYes)
          assert(!result.isVacuousYes)
        }
        "for VacuousYes | No" in {
          val result = vacuousYes | No("no")
          assert(result.isYes)
          assert(result.isVacuousYes)
        }
        "for No | VacuousYes" in {
          val result = No("no") | vacuousYes
          assert(result.isYes)
          assert(result.isVacuousYes)
        }
        "for Yes | Yes" in {
          val result = Yes("yes1") | Yes("yes2")
          assert(result.isYes)
          assert(!result.isVacuousYes)
        }
        "for Yes | No" in {
          val result = Yes("yes") | No("no")
          assert(result.isYes)
          assert(!result.isVacuousYes)
        }
        "for No | Yes" in {
          val result = No("no") | Yes("yes")
          assert(result.isYes)
          assert(!result.isVacuousYes)
        }
        "for No | No" in {
          val result = No("no1") | No("no2")
          assert(!result.isYes)
          assert(!result.isVacuousYes)
        }
      }
    }

    "toString method" - {

      "should use implicit prettifier in scope during construction" in {

        import org.scalactic.Prettifier
        import org.scalatest.Expectations._

        case class Yell(secret: String)
        val y = Yell("howdy")
        val z = Yell("hodee")
        implicit val myLittlePretty =
          new Prettifier {
            def apply(o: Any) =
              o match {
                case Yell(secret) => secret.toUpperCase + "!!!"
                case _ => Prettifier.default(o)
              }
          }
        val fact = expect(y == z)
        fact.toString shouldBe "No(HOWDY!!! did not equal HODEE!!!)"
      }

      "should display factMessage enclosed with opening and closing parentheses" in {
        val yes = Yes("fact message")
        yes.toString shouldBe "Yes(fact message)"

        val no = No("fact message")
        no.toString shouldBe "No(fact message)"
      }

      "should prefix new line to factMessage in toString when the factMessage contains \\n" in {
        val fact = Yes("line 1\nline 2\nline 3", "composable fact message")
        fact.toString shouldBe "Yes(" + NEWLINE + "  line 1\n  line 2\n  line 3" + NEWLINE + ")"
      }

      "should pad multiline error message correctly" in {
        import Expectations._
        import Inspectors._
        val x = 0
        val y = 1
        val zs = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
        val fact =
          (expect(x == 1) || expect(y == 1)) && forAll (zs) { z => expect(z <= 0) }
        fact.toString shouldBe
          "No(" + NEWLINE +
          "  Yes(" + NEWLINE +
          "    No(0 did not equal 1) ||" + NEWLINE +
          "    Yes(1 equaled 1)" + NEWLINE +
          "  ) &&" + NEWLINE +
          "  No(" + NEWLINE +
          "    forAll failed, because: " + NEWLINE +
          "      at index 0, 1 was not less than or equal to 0 " + NEWLINE +
          "    in List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)" + NEWLINE +
          "  )" + NEWLINE +
          ")"
      }
    }
  }

  "The Yes and No companion objects factory methods" - {
    "that takes two strings should work correctly" in {
      val fact = Yes("one", "two")
      fact.factMessage shouldBe ("one")
      fact.composableFactMessage shouldBe ("two")
      fact.rawFactMessage shouldBe ("one")
      fact.rawComposableFactMessage shouldBe ("two")
      fact.factMessageArgs shouldBe (Vector.empty)
      fact.composableFactMessageArgs shouldBe (Vector.empty)
      fact.isLeaf shouldBe (true)
      fact.isYes shouldBe (true)
      fact.isVacuousYes shouldBe (false)

      val ms = No("aaa", "bbb")
      ms.factMessage shouldBe ("aaa")
      ms.composableFactMessage shouldBe ("bbb")
      ms.rawFactMessage shouldBe ("aaa")
      ms.rawComposableFactMessage shouldBe ("bbb")
      ms.factMessageArgs shouldBe (Vector.empty)
      ms.composableFactMessageArgs shouldBe (Vector.empty)
      ms.isLeaf shouldBe (true)
      ms.isYes shouldBe (false)
      ms.isVacuousYes shouldBe (false)
    }
    "that takes two strings and two IndexedSeqs should work correctly" in {
      val fact = Yes("one", "two", Vector(42), Vector(43.0))
      fact.factMessage shouldBe ("one")
      fact.composableFactMessage shouldBe ("two")
      fact.rawFactMessage shouldBe ("one")
      fact.rawComposableFactMessage shouldBe ("two")
      fact.factMessageArgs shouldBe (Vector(42))
      fact.composableFactMessageArgs shouldBe (Vector(43.0))
      fact.isLeaf shouldBe (true)
      fact.isYes shouldBe (true)
      fact.isVacuousYes shouldBe (false)
      val ms = No("aaa", "bbb", Vector("ho", "he"), Vector("foo", "fie"))
      ms.factMessage shouldBe ("aaa")
      ms.composableFactMessage shouldBe ("bbb")
      ms.rawFactMessage shouldBe ("aaa")
      ms.rawComposableFactMessage shouldBe ("bbb")
      ms.factMessageArgs shouldBe (Vector("ho", "he"))
      ms.composableFactMessageArgs shouldBe (Vector("foo", "fie"))
      ms.isLeaf shouldBe (true)
      ms.isYes shouldBe (false)
      ms.isVacuousYes shouldBe (false)
    }
  }

  "The Fact obtained from combining two Facts with implies" - {
    "should be lazy about constructing strings" - {
      "for No implies No" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo implies rightSideNo
        fact shouldBe a [VacuousYes]
        fact.isNo shouldBe false
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('a', 'b'))
        fact.composableFactMessageArgs should be (Vector('a', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (true)
        fact.toString should startWith ("VacuousYes")
      }

      "for No implies Yes" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasLessThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo implies rightSideYes
        fact shouldBe a [VacuousYes]
        fact.isNo shouldBe false
        fact.rawFactMessage should be (Resources.rawWasNotGreaterThan)
        fact.rawComposableFactMessage should be (Resources.rawWasGreaterThan)
        fact.factMessage should be (Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty))
        fact.composableFactMessage should be (Resources.wasGreaterThan('a'.pretty, 'b'.pretty))
        fact.factMessageArgs should be (Vector('a', 'b'))
        fact.composableFactMessageArgs should be (Vector('a', 'b'))
        fact.isLeaf should be (true)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (true)
        fact.toString should startWith ("VacuousYes")
      }

      "for Yes implies No" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideYes implies rightSideNo
        fact shouldBe a [Implies]
        fact.isNo shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaBut)
        fact.rawComposableFactMessage should be (Resources.rawCommaBut)
        fact.factMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaBut(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes implies Yes" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideYes implies rightSideYes
        fact shouldBe a [Implies]
        fact.isYes shouldBe true
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }

      "for Boolean implies Fact" in {
        import org.scalatest.Expectations._
        val x = 10
        val fact = (x > 0) implies expect(x > -1)
        fact.isYes shouldBe true
        fact.toString shouldBe
        """Yes(
          |  Yes(10 was greater than 0) implies
          |  Yes(10 was greater than -1)
          |)""".stripMargin
      }
    }

    "should short-circuit correctly" - {
      "for Yes implies Yes" in {
        var evaluated = false
        Yes("yes1") && { evaluated = true; Yes("yes2") }
        assert(evaluated)
      }
      "for Yes implies No" in {
        var evaluated = false
        Yes("yes1") && { evaluated = true; No("no1") }
        assert(evaluated)
      }
      "for No implies Yes" in {
        var evaluated = false
        No("no1") && { evaluated = true; Yes("yes1") }
        assert(!evaluated)
      }
      "for No implies No" in {
        var evaluated = false
        No("no1") && { evaluated = true; No("no2") }
        assert(!evaluated)
      }
    }
  }

  "The Fact obtained from combining two Facts with isEqvTo" - {
    "should be lazy about constructing strings" - {

      "for No isEqvTo No" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo isEqvTo rightSideNo
        fact shouldBe a [IsEqvTo]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotGreaterThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }


      "for No isEqvTo Yes" in {
        val leftSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasNotGreaterThan, Vector('a', 'b'),Vector('a', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotLessThan, Resources.rawWasNotLessThan, Vector('a', 'd'),Vector('a', 'd'))
        val fact = leftSideNo isEqvTo rightSideYes
        fact shouldBe a [IsEqvTo]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasNotGreaterThan('a'.pretty, 'b'.pretty), Resources.wasNotLessThan('a'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideNo), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes isEqvTo No" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'b'),Vector('c', 'b'))
        val rightSideNo = No(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('c', 'd'),Vector('c', 'd'))
        val fact = leftSideYes isEqvTo rightSideNo
        fact shouldBe a [IsEqvTo]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('c'.pretty, 'b'.pretty), Resources.wasGreaterThan('c'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideNo)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (false)
        fact.isVacuousYes shouldBe (false)
      }

      "for Yes isEqvTo Yes" in {
        val leftSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'b'),Vector('e', 'b'))
        val rightSideYes = Yes(Resources.rawWasNotGreaterThan, Resources.rawWasGreaterThan, Vector('e', 'd'),Vector('e', 'd'))
        val fact = leftSideYes isEqvTo rightSideYes
        fact shouldBe a [IsEqvTo]
        fact.rawFactMessage should be (Resources.rawCommaAnd)
        fact.rawComposableFactMessage should be (Resources.rawCommaAnd)
        fact.factMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.composableFactMessage should be (Resources.commaAnd(Resources.wasGreaterThan('e'.pretty, 'b'.pretty), Resources.wasGreaterThan('e'.pretty, 'd'.pretty)))
        fact.factMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.composableFactMessageArgs should be (Vector(ComposableFactMessage(leftSideYes), ComposableFactMessage(rightSideYes)))
        fact.isLeaf should be (false)
        fact.isYes shouldBe (true)
        fact.isVacuousYes shouldBe (false)
      }
    }

    "should short-circuit correctly" - {
      "for Yes isEqvTo Yes" in {
        var evaluated = false
        Yes("yes1") isEqvTo { evaluated = true; Yes("yes2") }
        assert(evaluated)
      }
      "for Yes isEqvTo No" in {
        var evaluated = false
        Yes("yes1") isEqvTo { evaluated = true; No("no1") }
        assert(evaluated)
      }
      "for No isEqvTo Yes" in {
        var evaluated = false
        No("no1") isEqvTo { evaluated = true; Yes("yes1") }
        assert(evaluated)
      }
      "for No isEqvTo No" in {
        var evaluated = false
        No("no1") isEqvTo { evaluated = true; No("no2") }
        assert(evaluated)
      }
    }

    "should propagate VacuousYes correctly" - {
      import Expectations._
      val x = 1
      val vacuousYes = (expect(x == 2) implies expect(x > 0))
      "for VacuousYes isEqvTo VacuousYes" in {
        val result = vacuousYes isEqvTo vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes isEqvTo Yes" in {
        val result = vacuousYes isEqvTo Yes("yes")
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for Yes isEqvTo VacuousYes" in {
        val result = Yes("yes") isEqvTo vacuousYes
        assert(result.isYes)
        assert(result.isVacuousYes)
      }
      "for VacuousYes isEqvTo No" in {
        val result = vacuousYes isEqvTo No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No isEqvTo VacuousYes" in {
        val result = No("no") isEqvTo vacuousYes
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes isEqvTo Yes" in {
        val result = Yes("yes1") isEqvTo Yes("yes2")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
      "for Yes isEqvTo No" in {
        val result = Yes("yes") isEqvTo No("no")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No isEqvTo Yes" in {
        val result = No("no") isEqvTo Yes("yes")
        assert(!result.isYes)
        assert(!result.isVacuousYes)
      }
      "for No isEqvTo No" in {
        val result = No("no1") isEqvTo No("no2")
        assert(result.isYes)
        assert(!result.isVacuousYes)
      }
    }
  }

  def examples: TableFor1[Expectation] =
    Table(
      "fact",
      No("message", "composable message"),
      Yes("message", "composable message"),
      !(No("message", "composable message")),
      !(Yes("message", "composable message")),
      No("message", "composable message") && Yes("message", "composable message"),
      Yes("message", "composable message") && Yes("message", "composable message"),
      Yes("message", "composable message") && No("message", "composable message"),
      Yes("message", "composable message") || No("message", "composable message"),
      No("message", "composable message") || Yes("message", "composable message"),
      No("message", "composable message") || No("message", "composable message")
    )
}
