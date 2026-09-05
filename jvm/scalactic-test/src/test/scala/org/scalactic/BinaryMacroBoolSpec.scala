/*
 * Copyright 2001-2025 Artima, Inc.
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
package org.scalactic

import org.scalatest.funspec.AnyFunSpec

class BinaryMacroBoolSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  // The raw message constants used by BinaryMacroBool.
  private val exprWasFalse = Resources.rawExpressionWasFalse
  private val exprWasTrue = Resources.rawExpressionWasTrue
  private val didNotEqual = Resources.rawDidNotEqual
  private val equaled = Resources.rawEqualed
  private val wasNotGreaterThan = Resources.rawWasNotGreaterThan
  private val wasGreaterThan = Resources.rawWasGreaterThan
  private val wasNotGreaterThanOrEqualTo = Resources.rawWasNotGreaterThanOrEqualTo
  private val wasGreaterThanOrEqualTo = Resources.rawWasGreaterThanOrEqualTo
  private val wasNotLessThan = Resources.rawWasNotLessThan
  private val wasLessThan = Resources.rawWasLessThan
  private val wasNotLessThanOrEqualTo = Resources.rawWasNotLessThanOrEqualTo
  private val wasLessThanOrEqualTo = Resources.rawWasLessThanOrEqualTo
  private val didNotStartWith = Resources.rawDidNotStartWith
  private val startedWith = Resources.rawStartedWith
  private val didNotEndWith = Resources.rawDidNotEndWith
  private val endedWith = Resources.rawEndedWith
  private val didNotContainKey = Resources.rawDidNotContainKey
  private val containedKey = Resources.rawContainedKey
  private val didNotContain = Resources.rawDidNotContain
  private val contained = Resources.rawContained
  private val wasNotTheSameInstanceAs = Resources.rawWasNotTheSameInstanceAs
  private val wasTheSameInstanceAs = Resources.rawWasTheSameInstanceAs
  private val commaBut = Resources.rawCommaBut
  private val commaAnd = Resources.rawCommaAnd

  private def mb(left: Any, op: String, right: Any, expr: Boolean): BinaryMacroBool =
    new BinaryMacroBool(left, op, right, expr, prettifier)

  private def mbBool(left: Any, op: String, right: Any, bool: Bool): BinaryMacroBool =
    new BinaryMacroBool(left, op, right, bool, prettifier)

  private def sb(v: Boolean): SimpleBool = new SimpleBool(v, prettifier)

  private def eqInt(v: Int): TripleEqualsSupport#Equalizer[Int] = TripleEquals.convertToEqualizer(v)
  private def checkInt(v: Int): TripleEqualsSupport#CheckingEqualizer[Int] = TypeCheckedTripleEquals.convertToCheckingEqualizer(v)

  // Assert every public value-producing method of BinaryMacroBool.
  // When assertNegatedRawMessages is false the negated raw message methods are skipped,
  // because rawNegatedFailureMessage requires Bool operands for the || and | operators
  // (it has no catch-all match).
  private def assertBool(
    b: BinaryMacroBool,
    expectedValue: Boolean,
    expectedRawFailureMessage: String,
    expectedRawNegatedFailureMessage: String,
    expectedFailureArgs: IndexedSeq[Any],
    expectedNegatedArgs: IndexedSeq[Any],
    expectedAnalysis: IndexedSeq[String] = Vector.empty,
    assertNegatedRawMessages: Boolean = true
  ): Unit = {
    assert(b.value === expectedValue)
    assert(b.rawFailureMessage === expectedRawFailureMessage)
    assert(b.rawMidSentenceFailureMessage === expectedRawFailureMessage)
    assert(b.failureMessageArgs === expectedFailureArgs)
    assert(b.negatedFailureMessageArgs === expectedNegatedArgs)
    assert(b.midSentenceFailureMessageArgs === expectedFailureArgs)
    assert(b.midSentenceNegatedFailureMessageArgs === expectedNegatedArgs)
    assert(b.analysis === expectedAnalysis)
    if (assertNegatedRawMessages) {
      assert(b.rawNegatedFailureMessage === expectedRawNegatedFailureMessage)
      assert(b.rawMidSentenceNegatedFailureMessage === expectedRawNegatedFailureMessage)
    }
  }

  describe("BinaryMacroBool") {

    it("should support the == operator") {
      assertBool(mb(eqInt(1), "==", 1, true), true, didNotEqual, equaled, Vector(1, 1), Vector(1, 1))
      assertBool(mb(eqInt(1), "==", 2, false), false, didNotEqual, equaled, Vector(1, 2), Vector(1, 2))
      assertBool(mb("abc", "==", "abc", true), true, didNotEqual, equaled, Vector("abc", "abc"), Vector("abc", "abc"), Vector("\"abc\" -> \"abc\""))
    }

    it("should support the === operator") {
      assertBool(mb(1, "===", 1, true), true, didNotEqual, equaled, Vector(1, 1), Vector(1, 1))
      assertBool(mb(checkInt(1), "===", 1, true), true, didNotEqual, equaled, Vector(1, 1), Vector(1, 1))
      assertBool(mb(checkInt(2), "===", 2, true), true, didNotEqual, equaled, Vector(2, 2), Vector(2, 2))
      assertBool(mb("x", "===", "x", true), true, didNotEqual, equaled, Vector("x", "x"), Vector("x", "x"), Vector("\"x\" -> \"x\""))
    }

    it("should support the != and !== operators") {
      assertBool(mb(1, "!=", 2, true), true, equaled, didNotEqual, Vector(1, 2), Vector(1, 2))
      assertBool(mb(1, "!==", 2, true), true, equaled, didNotEqual, Vector(1, 2), Vector(1, 2))
    }

    it("should support the comparison operators") {
      assertBool(mb(1, ">", 0, true), true, wasNotGreaterThan, wasGreaterThan, Vector(1, 0), Vector(1, 0))
      assertBool(mb(1, ">=", 0, true), true, wasNotGreaterThanOrEqualTo, wasGreaterThanOrEqualTo, Vector(1, 0), Vector(1, 0))
      assertBool(mb(1, "<", 2, true), true, wasNotLessThan, wasLessThan, Vector(1, 2), Vector(1, 2))
      assertBool(mb(1, "<=", 2, true), true, wasNotLessThanOrEqualTo, wasLessThanOrEqualTo, Vector(1, 2), Vector(1, 2))
    }

    it("should support startsWith, endsWith and contains") {
      assertBool(mb("abc", "startsWith", "a", true), true, didNotStartWith, startedWith, Vector("abc", "a"), Vector("abc", "a"))
      assertBool(mb("abc", "endsWith", "c", true), true, didNotEndWith, endedWith, Vector("abc", "c"), Vector("abc", "c"))
      assertBool(mb("abc", "contains", "b", true), true, didNotContain, contained, Vector("abc", "b"), Vector("abc", "b"))
      val aMap = Map(1 -> "a")
      assertBool(mb(aMap, "contains", 1, true), true, didNotContainKey, containedKey, Vector(aMap, 1), Vector(aMap, 1))
    }

    it("should support eq and ne") {
      val o1 = new Object
      val o2 = new Object
      assertBool(mb(o1, "eq", o2, false), false, wasNotTheSameInstanceAs, wasTheSameInstanceAs, Vector(o1, o2), Vector(o1, o2))
      assertBool(mb(o1, "ne", o2, true), true, wasTheSameInstanceAs, wasNotTheSameInstanceAs, Vector(o1, o2), Vector(o1, o2))
    }

    it("should support the && operator") {
      // (Bool, Bool) with leftBool.value true and false
      assertBool(mb(sb(true), "&&", sb(true), true), true, commaBut, commaAnd, Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(false), "&&", sb(true), false), false, exprWasFalse, commaAnd, Vector.empty, Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      // (Bool, Any) with leftBool.value true and false
      assertBool(mb(sb(true), "&&", 0, true), true, commaBut, commaAnd, Vector(UnquotedString(exprWasTrue), 0), Vector(UnquotedString(exprWasTrue), 0))
      assertBool(mb(sb(false), "&&", 0, false), false, exprWasFalse, commaAnd, Vector.empty, Vector(UnquotedString(exprWasTrue), 0))
      // (Any, Bool) with rightBool.value true and false
      assertBool(mb(1, "&&", sb(true), true), true, commaBut, commaAnd, Vector(1, UnquotedString(exprWasTrue)), Vector(1, UnquotedString(exprWasTrue)))
      assertBool(mb(1, "&&", sb(false), true), true, commaBut, commaAnd, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasTrue)))
      // (Any, Any)
      assertBool(mb(1, "&&", 1, true), true, commaBut, commaAnd, Vector(1, 1), Vector(1, 1))
    }

    it("should support the & operator") {
      // (Bool, Bool) with all value combinations
      assertBool(mb(sb(true), "&", sb(true), true), true, commaAnd, commaAnd, Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(true), "&", sb(false), false), false, commaBut, commaAnd, Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(false), "&", sb(true), true), true, commaBut, commaAnd, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasTrue)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(false), "&", sb(false), false), false, commaAnd, commaAnd, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      // (Bool, Any) with leftBool.value true and false
      assertBool(mb(sb(true), "&", 1, true), true, commaBut, commaAnd, Vector(UnquotedString(exprWasTrue), 1), Vector(UnquotedString(exprWasTrue), 1))
      assertBool(mb(sb(false), "&", 1, true), true, commaBut, commaAnd, Vector(UnquotedString(exprWasFalse), 1), Vector(UnquotedString(exprWasTrue), 1))
      // (Any, Bool) with rightBool.value true and false
      assertBool(mb(1, "&", sb(true), true), true, commaBut, commaAnd, Vector(1, UnquotedString(exprWasTrue)), Vector(1, UnquotedString(exprWasTrue)))
      assertBool(mb(1, "&", sb(false), true), true, commaBut, commaAnd, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasTrue)))
      // (Any, Any)
      assertBool(mb(1, "&", 1, true), true, commaBut, commaAnd, Vector(1, 1), Vector(1, 1))
    }

    it("should support the || operator") {
      // (Bool, Bool) with all value combinations
      assertBool(mb(sb(true), "||", sb(true), true), true, exprWasTrue, exprWasTrue, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector.empty)
      assertBool(mb(sb(false), "||", sb(true), false), false, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(false), "||", sb(false), false), false, commaAnd, commaBut, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)))
      // (Bool, Any) with leftBool.value true and false
      assertBool(mb(sb(true), "||", 1, true), true, exprWasTrue, exprWasTrue, Vector(UnquotedString(exprWasFalse), 1), Vector.empty)
      assertBool(mb(sb(false), "||", 1, false), false, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), 1), Vector(UnquotedString(exprWasFalse), 1))
      // (Any, Bool) with rightBool.value true and false
      assertBool(mb(1, "||", sb(true), true), true, commaBut, commaBut, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasTrue)), assertNegatedRawMessages = false)
      assertBool(mb(1, "||", sb(false), true), true, commaBut, commaBut, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasFalse)), assertNegatedRawMessages = false)
      // (Any, Any)
      assertBool(mb(1, "||", 1, true), true, commaBut, commaBut, Vector(1, 1), Vector(1, 1), assertNegatedRawMessages = false)
    }

    it("should support the | operator") {
      // (Bool, Bool) with all value combinations
      assertBool(mb(sb(true), "|", sb(true), true), true, commaAnd, commaAnd, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(true), "|", sb(false), false), false, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasFalse)))
      assertBool(mb(sb(false), "|", sb(true), true), true, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasTrue)))
      assertBool(mb(sb(false), "|", sb(false), false), false, commaAnd, commaAnd, Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasFalse), UnquotedString(exprWasFalse)))
      // (Bool, Any) with leftBool.value true and false
      assertBool(mb(sb(true), "|", 1, true), true, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), 1), Vector(UnquotedString(exprWasTrue), 1), assertNegatedRawMessages = false)
      assertBool(mb(sb(false), "|", 1, true), true, commaBut, commaBut, Vector(UnquotedString(exprWasFalse), 1), Vector(UnquotedString(exprWasFalse), 1), assertNegatedRawMessages = false)
      // (Any, Bool) with rightBool.value true and false
      assertBool(mb(1, "|", sb(true), true), true, commaBut, commaBut, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasTrue)), assertNegatedRawMessages = false)
      assertBool(mb(1, "|", sb(false), true), true, commaBut, commaBut, Vector(1, UnquotedString(exprWasFalse)), Vector(1, UnquotedString(exprWasFalse)), assertNegatedRawMessages = false)
      // (Any, Any)
      assertBool(mb(1, "|", 1, true), true, commaBut, commaBut, Vector(1, 1), Vector(1, 1), assertNegatedRawMessages = false)
    }

    it("should handle an unknown operator") {
      assertBool(mb(1, "???", 2, false), false, exprWasFalse, exprWasTrue, Vector.empty, Vector.empty)
    }

    it("should support the overloaded Bool constructor") {
      assertBool(mbBool(1, "==", 2, sb(true)), true, didNotEqual, equaled, Vector(1, 2), Vector(1, 2))
      assertBool(mbBool(sb(true), "&&", sb(true), sb(true)), true, commaBut, commaAnd, Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasFalse)), Vector(UnquotedString(exprWasTrue), UnquotedString(exprWasTrue)))
    }
  }
}