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
package org.scalatest
package expectations

import org.scalatest.funspec.AnyFunSpec

/**
 * Tests to verify that Fact.toString produces properly capitalized messages
 * that align with the messages thrown by toAssertion.
 *
 * These tests currently FAIL because toString uses midSentenceFactMessage
 * (lowercase) instead of factMessage (capitalized).
 */
class FactToStringSpec extends AnyFunSpec with Expectations {

  describe("Fact.toString for expectResult") {
    it("should use capitalized message when expectation fails") {
      val a = 5
      val b = 2
      val fact = expectResult(2) { a - b }

      // Should show "Expected 2, but got 3" with capital E
      assert(fact.toString == "No(Expected 2, but got 3)")
    }

    it("should use capitalized message when expectation succeeds") {
      val a = 3
      val b = 1
      val fact = expectResult(2) { a - b }

      // Yes facts don't typically show error messages in toString, but should be consistent
      // The message would be shown in negation or other contexts
      assert(fact.isYes)
    }

    it("should use capitalized message with clue when expectation fails") {
      val fact = expectResult(5, "calculation was wrong") { 3 }

      // Should show "Expected 5, but got 3 calculation was wrong" with capital E
      assert(fact.toString == "No(Expected 5, but got 3 calculation was wrong)")
    }
  }

  describe("Fact.toString for expectThrows") {
    it("should use capitalized message when exception is thrown (success case)") {
      val s = "hi"
      // SKIP-SCALATESTJS-START
      val fact = expectThrows[IndexOutOfBoundsException] {
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val fact = expectThrows[Error] {  
        s.charAt(-1)
      }

      // Should show "Exception ... was thrown" with capital E
      // The class name gets quoted by the prettifier
      // SKIP-SCALATESTJS-START
      assert(fact.toString == "Yes(Exception \"java.lang.IndexOutOfBoundsException\" was thrown)")
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(fact.toString == "Yes(Exception \"java.lang.Error\" was thrown)")
      
    }

    it("should use capitalized message when no exception is thrown (failure case)") {
      val s = "hi"
      val fact = expectThrows[IndexOutOfBoundsException] {
        s.charAt(0)
      }

      // Should show "Expected exception ... to be thrown, but no exception was thrown" with capital E
      // The class name gets quoted by the prettifier
      assert(fact.toString == "No(Expected exception \"java.lang.IndexOutOfBoundsException\" to be thrown, but no exception was thrown)")
    }

    it("should use capitalized message when wrong exception is thrown (failure case)") {
      // SKIP-SCALATESTJS-START
      val fact = expectThrows[IllegalArgumentException] {
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY val fact = expectThrows[IllegalArgumentException] {
        "test".charAt(100) // throws IndexOutOfBoundsException, not IllegalArgumentException
      }

      // Should show "Expected exception ... to be thrown, but ... was thrown" with capital E
      // Class names get quoted by the prettifier
      // SKIP-SCALATESTJS-START
      assert(fact.toString.startsWith("No(Expected exception \"java.lang.IllegalArgumentException\" to be thrown, but \"java.lang.StringIndexOutOfBoundsException\" was thrown"))
      // SKIP-SCALATESTJS-END
      //SCALATESTJS-ONLY assert(fact.toString.startsWith("No(Expected exception \"java.lang.IllegalArgumentException\" to be thrown, but \"org.scalajs.linker.runtime.UndefinedBehaviorError\" was thrown"))
    }
  }

  describe("Fact.toString for composite facts") {
    it("should use capitalized messages in leaf nodes of && composition") {
      val fact1 = expectResult(2) { 3 }
      val fact2 = expectResult(5) { 6 }
      val combined = fact1 & fact2  // Use & not && to avoid short-circuit

      val msg = combined.toString
      // The diagram should show:
      // No(
      //   No(Expected 2, but got 3) &
      //   No(Expected 5, but got 6)
      // )
      // Each leaf fact should have capital E
      assert(msg.contains("Expected 2, but got 3"))
      assert(msg.contains("Expected 5, but got 6"))
      assert(!msg.contains("expected 2, but got 3")) // lowercase should not appear
      assert(!msg.contains("expected 5, but got 6")) // lowercase should not appear
    }

    it("should use capitalized messages in leaf nodes of || composition") {
      val fact1 = expectResult(2) { 3 }
      val fact2 = expectResult(5) { 6 }
      val combined = fact1 || fact2

      val msg = combined.toString
      // Each leaf fact in the diagram should have capital E
      assert(msg.contains("Expected 2, but got 3"))
      assert(msg.contains("Expected 5, but got 6"))
    }

    it("should use capitalized message in negation") {
      val fact = expectResult(2) { 3 }
      val negated = !fact

      val msg = negated.toString
      // The underlying fact should show capitalized message
      assert(msg.contains("Expected 2, but got 3"))
      assert(!msg.contains("expected 2, but got 3"))
    }
  }

  describe("Fact.toString alignment with toAssertion") {
    it("expectResult toString and toAssertion should use same base message") {
      val fact = expectResult(2) { 3 }

      val toStringMsg = fact.toString
      val exceptionMsg = try {
        fact.toAssertion
        "no exception"
      } catch {
        case e: exceptions.TestFailedException => e.message.get
      }

      // Both should say "Expected 2, but got 3" (with capital E)
      assert(toStringMsg == "No(Expected 2, but got 3)")
      assert(exceptionMsg == "Expected 2, but got 3")
    }

    it("expectThrows toString and toAssertion should use same base message for failure") {
      val s = "hi"
      val fact = expectThrows[IndexOutOfBoundsException] {
        s.charAt(0) // doesn't throw
      }

      val toStringMsg = fact.toString
      val exceptionMsg = try {
        fact.toAssertion
        "no exception"
      } catch {
        case e: exceptions.TestFailedException => e.message.get
      }

      // Both should say "Expected exception ... to be thrown, but no exception was thrown" (with capital E)
      // Note: toString includes quotes from prettifier, toAssertion does not
      assert(toStringMsg == "No(Expected exception \"java.lang.IndexOutOfBoundsException\" to be thrown, but no exception was thrown)")
      assert(exceptionMsg == "Expected exception \"java.lang.IndexOutOfBoundsException\" to be thrown, but no exception was thrown")
    }
  }
}
