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

import org.scalatest._
import org.scalatest.funspec.AnyFunSpec

class BoolSpec extends AnyFunSpec {

  private val prettifier = Prettifier.default

  private def simpleBool(v: Boolean): Bool = new SimpleBool(v, prettifier)

  private def expressionWasFalse: String = Resources.rawExpressionWasFalse
  private def expressionWasTrue: String = Resources.rawExpressionWasTrue
  private def rawCommaBut: String = Resources.rawCommaBut
  private def rawCommaAnd: String = Resources.rawCommaAnd

  private def andBoolMessage(shortCircuit: Boolean, v1: Boolean, v2: Boolean): String = {
    if (shortCircuit && !v1) expressionWasFalse
    else if (v1 == v2) rawCommaAnd
    else rawCommaBut
  }

  describe("AndBool") {

    it("should compute value as the logical AND of its two Bool operands") {
      assert(new AndBool(simpleBool(true), simpleBool(true), false, prettifier).value)
      assert(!new AndBool(simpleBool(true), simpleBool(false), false, prettifier).value)
      assert(!new AndBool(simpleBool(false), simpleBool(true), false, prettifier).value)
      assert(!new AndBool(simpleBool(false), simpleBool(false), false, prettifier).value)
      assert(new AndBool(simpleBool(true), simpleBool(true), true, prettifier).value)
      assert(!new AndBool(simpleBool(false), simpleBool(true), true, prettifier).value)
    }

    it("should compute rawFailureMessage correctly for all operand value combinations") {
      val nonShort = new AndBool(simpleBool(true), simpleBool(true), false, prettifier)
      assert(nonShort.rawFailureMessage == rawCommaAnd)
      assert(nonShort.failureMessage == "Expression was true, and Expression was true")

      val butShort = new AndBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(butShort.rawFailureMessage == rawCommaBut)
      assert(butShort.failureMessage == "Expression was true, but Expression was false")

      val butNonShort = new AndBool(simpleBool(false), simpleBool(true), false, prettifier)
      assert(butNonShort.rawFailureMessage == rawCommaBut)
      assert(butNonShort.failureMessage == "Expression was false, but Expression was true")

      val falseShort = new AndBool(simpleBool(false), simpleBool(false), false, prettifier)
      assert(falseShort.rawFailureMessage == rawCommaAnd)
      assert(falseShort.failureMessage == "Expression was false, and Expression was false")

      val shortCircuitFalse = new AndBool(simpleBool(false), simpleBool(true), true, prettifier)
      assert(shortCircuitFalse.rawFailureMessage == expressionWasFalse)
      assert(shortCircuitFalse.failureMessage == "Expression was false")
    }

    it("should return the first Bool's raw failure message when short-circuiting") {
      val short = new AndBool(simpleBool(false), simpleBool(true), true, prettifier)
      assert(short.rawFailureMessage == expressionWasFalse)
      assert(short.rawMidSentenceFailureMessage == rawCommaBut)
    }

    it("should produce the negated raw messages from commaAnd") {
      val b = new AndBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(b.rawNegatedFailureMessage == rawCommaAnd)
      assert(b.rawMidSentenceNegatedFailureMessage == rawCommaAnd)
      assert(b.rawMidSentenceFailureMessage == rawCommaBut)
    }

    it("should compute failureMessageArgs for each combination") {
      val bothTrue = new AndBool(simpleBool(true), simpleBool(true), false, prettifier)
      assert(bothTrue.failureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasTrue)))

      val trueFalse = new AndBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(trueFalse.failureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasFalse)))

      val falseTrue = new AndBool(simpleBool(false), simpleBool(true), false, prettifier)
      assert(falseTrue.failureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasTrue)))

      val bothFalse = new AndBool(simpleBool(false), simpleBool(false), false, prettifier)
      assert(bothFalse.failureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasFalse)))

      val short = new AndBool(simpleBool(false), simpleBool(true), true, prettifier)
      assert(short.failureMessageArgs == Vector.empty)
    }

    it("should compute negatedFailureMessageArgs and mid-sentence args") {
      val b = new AndBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(b.negatedFailureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasTrue)))
      assert(b.midSentenceFailureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasFalse)))
      assert(b.midSentenceNegatedFailureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasTrue)))
    }

    it("should be produced by the && operator") {
      val result = simpleBool(true) && simpleBool(false)
      assert(result.isInstanceOf[AndBool])
      assert(!result.value)
    }

    it("should be produced by the & operator") {
      val result = simpleBool(true) & simpleBool(false)
      assert(result.isInstanceOf[AndBool])
      assert(!result.value)
    }
  }

  describe("OrBool") {

    it("should compute value as the logical OR of its two Bool operands") {
      assert(new OrBool(simpleBool(true), simpleBool(true), false, prettifier).value)
      assert(new OrBool(simpleBool(true), simpleBool(false), false, prettifier).value)
      assert(new OrBool(simpleBool(false), simpleBool(true), false, prettifier).value)
      assert(!new OrBool(simpleBool(false), simpleBool(false), false, prettifier).value)
    }

    it("should compute rawFailureMessage correctly for all operand value combinations") {
      val bothTrue = new OrBool(simpleBool(true), simpleBool(true), false, prettifier)
      assert(bothTrue.rawFailureMessage == rawCommaAnd)
      assert(bothTrue.failureMessage == "Expression was true, and Expression was true")

      val trueFalse = new OrBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(trueFalse.rawFailureMessage == rawCommaBut)
      assert(trueFalse.failureMessage == "Expression was true, but Expression was false")

      val falseTrue = new OrBool(simpleBool(false), simpleBool(true), false, prettifier)
      assert(falseTrue.rawFailureMessage == rawCommaBut)
      assert(falseTrue.failureMessage == "Expression was false, but Expression was true")

      val bothFalse = new OrBool(simpleBool(false), simpleBool(false), false, prettifier)
      assert(bothFalse.rawFailureMessage == rawCommaAnd)
      assert(bothFalse.failureMessage == "Expression was false, and Expression was false")
    }

    it("should return the first Bool's raw failure message when short-circuiting on true") {
      val short = new OrBool(simpleBool(true), simpleBool(false), true, prettifier)
      assert(short.rawFailureMessage == expressionWasFalse)
      assert(short.rawNegatedFailureMessage == expressionWasTrue)
      assert(short.negatedFailureMessageArgs == Vector.empty)
    }

    it("should produce the mid sentence raw messages") {
      val b = new OrBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(b.rawMidSentenceFailureMessage == rawCommaAnd)
      assert(b.rawMidSentenceNegatedFailureMessage == rawCommaAnd)
    }

    it("should compute rawNegatedFailureMessage for non-short-circuit operands") {
      val bothTrue = new OrBool(simpleBool(true), simpleBool(true), false, prettifier)
      assert(bothTrue.rawNegatedFailureMessage == rawCommaAnd)

      val trueFalse = new OrBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(trueFalse.rawNegatedFailureMessage == rawCommaBut)

      val bothFalse = new OrBool(simpleBool(false), simpleBool(false), false, prettifier)
      assert(bothFalse.rawNegatedFailureMessage == rawCommaAnd)
    }

    it("should compute the message args") {
      val b = new OrBool(simpleBool(true), simpleBool(false), false, prettifier)
      assert(b.failureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasFalse)))
      assert(b.negatedFailureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasFalse)))
      assert(b.midSentenceFailureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasFalse)))
      assert(b.midSentenceNegatedFailureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasTrue)))

      val bothTrue = new OrBool(simpleBool(true), simpleBool(true), false, prettifier)
      assert(bothTrue.negatedFailureMessageArgs == Vector(UnquotedString(expressionWasTrue), UnquotedString(expressionWasTrue)))

      val bothFalse = new OrBool(simpleBool(false), simpleBool(false), false, prettifier)
      assert(bothFalse.negatedFailureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasFalse)))

      val falseTrue = new OrBool(simpleBool(false), simpleBool(true), false, prettifier)
      assert(falseTrue.negatedFailureMessageArgs == Vector(UnquotedString(expressionWasFalse), UnquotedString(expressionWasTrue)))
    }

    it("should be produced by the || and | operators") {
      val orResult = simpleBool(false) || simpleBool(true)
      assert(orResult.isInstanceOf[OrBool])
      assert(orResult.value)

      val pipeResult = simpleBool(false) | simpleBool(true)
      assert(pipeResult.isInstanceOf[OrBool])
      assert(pipeResult.value)
    }
  }
}
