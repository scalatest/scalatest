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

/**
 * Singleton object that provides <code>unapply</code> method to extract negated failure message from <code>MatchResult</code>
 * having <code>matches</code> property value of <code>true</code>.
 *
 * @author Bill Venners
 * @author Chee Seng
 */
object MatchSucceeded {
  /**
   * Extractor enabling patterns that match <code>MatchResult</code> having <code>matches</code> property value of <code>true</code>,
   * extracting the contained negated failure message.
   *
   * <p>
   * For example, you can use this extractor to get the negated failure message of a <code>MatchResult</code> like this:
   * </p>
   *
   * <pre>
   * matchResult match {
   *   case MatchSucceeded(negatedFailureMessage) => // do something with negatedFailureMessage
   *   case _ => // when matchResult.matches equal to <code>false</code>
   * }
   * </pre>
   *
   * @param matchResult the <code>MatchResult</code> to extract the negated failure message from.
   * @return a <code>Some</code> wrapping the contained negated failure message if <code>matchResult.matches</code> is equal to <code>true</code>, else <code>None</code>.
   */
  def unapply(matchResult: MatchResult)(implicit prettifier: Prettifier): Option[String] =
    if (matchResult.matches) Some(matchResult.negatedFailureMessage(prettifier)) else None
}
