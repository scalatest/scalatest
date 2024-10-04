/*
 * Copyright 2001-2024 Artima, Inc.
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

  def simplifiedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.simplifiedFactMessage == expectedValue,
          "simplifiedFactMessage",
          expectedValue,
          exp.simplifiedFactMessage
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

  def midSentenceSimplifiedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceSimplifiedFactMessage == expectedValue,
          "midSentenceSimplifiedFactMessage",
          expectedValue,
          exp.midSentenceSimplifiedFactMessage
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

  def rawSimplifiedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawSimplifiedFactMessage == expectedValue,
          "rawSimplifiedFailureMessage",
          expectedValue,
          exp.rawSimplifiedFactMessage
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

  def rawMidSentenceSimplifiedFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawMidSentenceSimplifiedFactMessage == expectedValue,
          "rawMidSentenceSimplifidFactMessage",
          expectedValue,
          exp.rawMidSentenceSimplifiedFactMessage
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

  def simplifiedFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.simplifiedFactMessageArgs == expectedValue,
          "simplifiedFactMessageArgs",
          expectedValue,
          exp.simplifiedFactMessageArgs
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

  def midSentenceSimplifiedFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.midSentenceSimplifiedFactMessageArgs == expectedValue,
          "midSentenceSimplifiedFactMessageArgs",
          expectedValue,
          exp.midSentenceSimplifiedFactMessageArgs
        )
    }

  def isLeaf(expectedValue: Boolean) =
    new HavePropertyMatcher[Expectation, Boolean] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.isLeaf == expectedValue,
          "isLeaf",
          expectedValue,
          exp.isLeaf
        )
    }
}
