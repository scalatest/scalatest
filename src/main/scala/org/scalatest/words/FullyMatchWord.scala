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
import org.scalatest.UnquotedString

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class FullyMatchWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimal = """(-)?(\d+)(\.\d*)?"""
   * "1.7" should (fullyMatch regex (decimal) and fullyMatch regex (decimal))
   *                          ^
   * </pre>
   */
  def regex(rightRegexString: String): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          java.util.regex.Pattern.matches(rightRegexString, left),
          FailureMessages("didNotFullyMatchRegex", left, UnquotedString(rightRegexString)),
          FailureMessages("fullyMatchedRegex", left, UnquotedString(rightRegexString))
        )
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
   * "1.7" should (fullyMatch regex (decimalRegex) and fullyMatch regex (decimalRegex))
   *                          ^
   * </pre>
   */
  def regex(rightRegex: Regex): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          rightRegex.pattern.matcher(left).matches,
          FailureMessages("didNotFullyMatchRegex", left, rightRegex),
          FailureMessages("fullyMatchedRegex", left, rightRegex)
        )
    }
}
