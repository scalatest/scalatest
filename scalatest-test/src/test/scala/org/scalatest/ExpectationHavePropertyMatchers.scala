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

  def composableFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.composableFactMessage == expectedValue,
          "composableFactMessage",
          expectedValue,
          exp.composableFactMessage
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

  def rawComposableFactMessage(expectedValue: String) =
    new HavePropertyMatcher[Expectation, String] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.rawComposableFactMessage == expectedValue,
          "rawComposableFailureMessage",
          expectedValue,
          exp.rawComposableFactMessage
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

  def composableFactMessageArgs(expectedValue: IndexedSeq[Any]) =
    new HavePropertyMatcher[Expectation, IndexedSeq[Any]] {
      def apply(exp: Expectation) =
        HavePropertyMatchResult(
          exp.composableFactMessageArgs == expectedValue,
          "composableFactMessageArgs",
          expectedValue,
          exp.composableFactMessageArgs
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
