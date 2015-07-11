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
          exp.failureMessage == expectedValue,
          "failureMessage",
          expectedValue,
          exp.failureMessage
        )
    }

  def negatedFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.negatedFailureMessage == expectedValue,
          "negatedFailureMessage",
          expectedValue,
          exp.negatedFailureMessage
        )
    }

  def midSentenceFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceFailureMessage == expectedValue,
          "midSentenceFailureMessage",
          expectedValue,
          exp.midSentenceFailureMessage
        )
    }

  def midSentenceNegatedFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceNegatedFailureMessage == expectedValue,
          "midSentenceNegatedFailureMessage",
          expectedValue,
          exp.midSentenceNegatedFailureMessage
        )
    }

  def rawFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawFailureMessage == expectedValue,
          "rawFailureMessage",
          expectedValue,
          exp.rawFailureMessage
        )
    }

  def rawNegatedFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawNegatedFailureMessage == expectedValue,
          "rawNegatedFailureMessage",
          expectedValue,
          exp.rawNegatedFailureMessage
        )
    }

  def rawMidSentenceFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawMidSentenceFailureMessage == expectedValue,
          "rawMidSentenceFailureMessage",
          expectedValue,
          exp.rawMidSentenceFailureMessage
        )
    }

  def rawMidSentenceNegatedFailureMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawMidSentenceNegatedFailureMessage == expectedValue,
          "rawMidSentenceNegatedFailureMessage",
          expectedValue,
          exp.rawMidSentenceNegatedFailureMessage
        )
    }

  def failureMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.failureMessageArgs == expectedValue,
          "failureMessageArgs",
          expectedValue,
          exp.failureMessageArgs
        )
    }

  def negatedFailureMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.negatedFailureMessageArgs == expectedValue,
          "negatedFailureMessageArgs",
          expectedValue,
          exp.negatedFailureMessageArgs
        )
    }

  def midSentenceFailureMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceFailureMessageArgs == expectedValue,
          "midSentenceFailureMessageArgs",
          expectedValue,
          exp.midSentenceFailureMessageArgs
        )
    }

  def midSentenceNegatedFailureMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceNegatedFailureMessageArgs == expectedValue,
          "midSentenceNegatedFailureMessageArgs",
          expectedValue,
          exp.midSentenceNegatedFailureMessageArgs
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
