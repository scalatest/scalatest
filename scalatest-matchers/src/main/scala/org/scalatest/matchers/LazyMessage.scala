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
package org.scalatest.matchers
 
// Idea is to override toString each time it is used.
sealed private[scalatest] abstract class LazyMessage {
  val nestedArgs: IndexedSeq[Any]
}

private[scalatest] case class FailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.failureMessageArgs
  override def toString: String = matchResult.failureMessage
}

private[scalatest] case class NegatedFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.negatedFailureMessageArgs
  override def toString: String = matchResult.negatedFailureMessage
}

private[scalatest] case class MidSentenceFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.failureMessageArgs
  override def toString: String = matchResult.midSentenceFailureMessage
}

private[scalatest] case class MidSentenceNegatedFailureMessage(matchResult: MatchResult) extends LazyMessage {
  val nestedArgs: IndexedSeq[Any] = matchResult.negatedFailureMessageArgs
  override def toString: String = matchResult.midSentenceNegatedFailureMessage
}
