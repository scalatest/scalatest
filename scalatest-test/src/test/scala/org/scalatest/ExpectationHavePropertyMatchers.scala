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


import matchers.HavePropertyMatcher
import matchers.HavePropertyMatchResult

trait ExpectationHavePropertyMatchers {

  def failureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.factMessage == expectedValue,
          "factMessage",
          expectedValue,
          exp.factMessage
        )
    }

  def negatedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.negatedFactMessage == expectedValue,
          "negatedFactMessage",
          expectedValue,
          exp.negatedFactMessage
        )
    }

  def midSentenceFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceFactMessage == expectedValue,
          "midSentenceFactMessage",
          expectedValue,
          exp.midSentenceFactMessage
        )
    }

  def midSentenceNegatedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceNegatedFactMessage == expectedValue,
          "midSentenceNegatedFactMessage",
          expectedValue,
          exp.midSentenceNegatedFactMessage
        )
    }

  def rawFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawFactMessage == expectedValue,
          "rawFailureMessage",
          expectedValue,
          exp.rawFactMessage
        )
    }

  def rawNegatedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawNegatedFactMessage == expectedValue,
          "rawNegatedFailureMessage",
          expectedValue,
          exp.rawNegatedFactMessage
        )
    }

  def rawMidSentenceFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawMidSentenceFactMessage == expectedValue,
          "rawMidSentenceFactMessage",
          expectedValue,
          exp.rawMidSentenceFactMessage
        )
    }

  def rawMidSentenceNegatedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawMidSentenceNegatedFactMessage == expectedValue,
          "rawMidSentenceSimplifidFactMessage",
          expectedValue,
          exp.rawMidSentenceNegatedFactMessage
        )
    }

  def factMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.factMessageArgs == expectedValue,
          "factMessageArgs",
          expectedValue,
          exp.factMessageArgs
        )
    }

  def negatedFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.negatedFactMessageArgs == expectedValue,
          "negatedFactMessageArgs",
          expectedValue,
          exp.negatedFactMessageArgs
        )
    }

  def midSentenceFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceFactMessageArgs == expectedValue,
          "midSentenceFactMessageArgs",
          expectedValue,
          exp.midSentenceFactMessageArgs
        )
    }

  def midSentenceNegatedFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceNegatedFactMessageArgs == expectedValue,
          "midSentenceNegatedFactMessageArgs",
          expectedValue,
          exp.midSentenceNegatedFactMessageArgs
        )
    }

  def composite(expectedValue: Boolean) =
    new HavePropertyMatcher[Expectation, Boolean] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.composite == expectedValue,
          "composite",
          expectedValue,
          exp.composite
        )
    }
}
