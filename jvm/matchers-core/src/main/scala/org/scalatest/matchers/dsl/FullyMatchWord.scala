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
package org.scalatest.matchers.dsl

import org.scalatest.matchers._
import org.scalactic._
import scala.util.matching.Regex
import org.scalatest.Resources
import org.scalatest.UnquotedString
import org.scalatest.matchers.MatchersHelper.fullyMatchRegexWithGroups

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
  //DOTTY-ONLY infix def regex(rightRegexString: String): Matcher[String] =
  // SKIP-DOTTY-START 
  def regex(rightRegexString: String): Matcher[String] =
  // SKIP-DOTTY-END
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          java.util.regex.Pattern.matches(rightRegexString, left),
          Resources.rawDidNotFullyMatchRegex,
          Resources.rawFullyMatchedRegex,
          Vector(left, UnquotedString(rightRegexString))
        )
      override def toString: String = "fullyMatch regex " + Prettifier.default(rightRegexString)
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * string should not { fullyMatch regex ("a(b*)c" withGroup "bb") } 
   *                          ^
   * </pre>
   */
  //DOTTY-ONLY infix def regex(regexWithGroups: RegexWithGroups) = 
  // SKIP-DOTTY-START 	
  def regex(regexWithGroups: RegexWithGroups) = 
  // SKIP-DOTTY-END
    new Matcher[String] {
      def apply(left: String): MatchResult = 
        fullyMatchRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      override def toString: String = "fullyMatch regex " + Prettifier.default(regexWithGroups)
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
  //DOTTY-ONLY infix def regex(rightRegex: Regex): Matcher[String] =
  // SKIP-DOTTY-START 
  def regex(rightRegex: Regex): Matcher[String] =
  // SKIP-DOTTY-END
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          rightRegex.pattern.matcher(left).matches,
          Resources.rawDidNotFullyMatchRegex,
          Resources.rawFullyMatchedRegex,
          Vector(left, UnquotedString(rightRegex.toString))
        )
      override def toString: String = "fullyMatch regex \"" + Prettifier.default(rightRegex) + "\""
    }
  
  /**
   * Overrides toString to return "fullyMatch"
   */
  override def toString: String = "fullyMatch"
}
