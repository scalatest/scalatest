/*
 * Copyright 2001-2008 Artima, Inc.
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
package org.scalatest.words

import org.scalatest.matchers._
import org.scalautils._
import scala.util.matching.Regex
import org.scalatest.FailureMessages

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class EndWithWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7b" should (endWith ("1.7b") and endWith ("7b"))
   *                        ^
   * </pre>
   */
  def apply(right: String): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          left endsWith right,
          FailureMessages("didNotEndWith", left, right),
          FailureMessages("endedWith", left, right)
        )
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimal = """(-)?(\d+)(\.\d*)?"""
   * "b1.7" should (endWith regex (decimal) and endWith regex (decimal))
   *                        ^
   * </pre>
   */
  def regex[T <: String](right: T): Matcher[T] = regex(right.r)

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
   * "b1.7" should (endWith regex (decimalRegex) and endWith regex (decimalRegex))
   *                        ^
   * </pre>
   */
  def regex(rightRegex: Regex): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult = {
        val allMatches = rightRegex.findAllIn(left)
        MatchResult(
          allMatches.hasNext && (allMatches.end == left.length),
          FailureMessages("didNotEndWithRegex", left, rightRegex),
          FailureMessages("endedWithRegex", left, rightRegex)
        )
      }
    }
}
