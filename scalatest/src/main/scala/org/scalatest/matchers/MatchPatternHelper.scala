/*
 * Copyright 2001-2014 Artima, Inc.
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

import org.scalatest.{FailureMessages, Resources}
import org.scalactic.{Prettifier, SourceInfo}
import org.scalatest.MatchersHelper._
import org.scalatest.words.ResultOfNotWordForAny

/**
 * <code>MatchPatternHelper</code> is called by <code>MatchPatternMacro</code> to support <code>matchPattern</code> syntax.
 */
object MatchPatternHelper {

  /**
   * <code>MatchPatternHelper</code> that is called by <code>MatchPatternMacro</code> to support the following syntax:
   *
   * <pre class="stHighlight">
   * result should matchPattern { case Person("Bob", _) => }
   *               ^
   * </pre>
   */
  def matchPatternMatcher(right: PartialFunction[Any, _]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        MatchResult(
          right.isDefinedAt(left),
          Resources.rawDidNotMatchTheGivenPattern,
          Resources.rawMatchedTheGivenPattern,
          Vector(left)
        )
      }
      override def toString: String = "patternMatch " + Prettifier.default(right)
    }

  /**
   * <code>MatchPatternHelper</code> that is called by <code>MatchPatternMacro</code> to support the following syntax:
   *
   * <pre class="stHighlight">
   * result should (not matchPattern { case Person("Alice", _) => } and (equal (result)))
   *                    ^
   * </pre>
   */
  def notMatchPatternMatcher(right: PartialFunction[Any, _]): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        MatchResult(
          !right.isDefinedAt(left),
          Resources.rawMatchedTheGivenPattern,
          Resources.rawDidNotMatchTheGivenPattern,
          Vector(left)
        )
      }
      override def toString: String = "not patternMatch " + Prettifier.default(right)
    }

  /**
   * <code>MatchPatternHelper</code> that is called by <code>MatchPatternMacro</code> to support the following syntax:
   *
   * <pre class="stHighlight">
   * result should not matchPattern { case Person("Bob", _) => }
   *                   ^
   * </pre>
   */
  def checkMatchPattern(resultOfNoWordForAny: ResultOfNotWordForAny[_], right: PartialFunction[Any, _]) {
    if (right.isDefinedAt(resultOfNoWordForAny.left) != resultOfNoWordForAny.shouldBeTrue)
      throw newTestFailedException(
        if (resultOfNoWordForAny.shouldBeTrue)
          FailureMessages.didNotMatchTheGivenPattern(resultOfNoWordForAny.left)
        else
          FailureMessages.matchedTheGivenPattern(resultOfNoWordForAny.left),
        None,
        resultOfNoWordForAny.sourceInfo
      )
  }

}