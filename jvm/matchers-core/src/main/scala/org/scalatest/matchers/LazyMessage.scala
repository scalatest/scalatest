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
package org.scalatest.matchers

import org.scalactic.Prettifier

// Idea is to override toString each time it is used.
sealed private[scalatest] abstract class LazyMessage extends Product with Serializable {
  val nestedArgs: IndexedSeq[Any]
  def message(prettifier: Prettifier): String
  override def toString: String = message(Prettifier.default)
}

private[scalatest] case class FailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.failureMessageArgs
  def message(prettifier: Prettifier): String = matchResult.failureMessage(prettifier)
}

private[scalatest] case class NegatedFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.negatedFailureMessageArgs
  def message(prettifier: Prettifier): String = matchResult.negatedFailureMessage(prettifier)
}

private[scalatest] case class MidSentenceFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.failureMessageArgs
  def message(prettifier: Prettifier): String = matchResult.midSentenceFailureMessage(prettifier)
}

private[scalatest] case class MidSentenceNegatedFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.negatedFailureMessageArgs
  def message(prettifier: Prettifier): String = matchResult.midSentenceNegatedFailureMessage(prettifier)
}
