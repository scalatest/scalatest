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
package org.scalatest.words

import org.scalatest.matchers._
import org.scalautils._
import scala.util.matching.Regex
import org.scalatest.Resources
import org.scalatest.UnquotedString
import org.scalatest.MatchersHelper.startWithRegexWithGroups

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class StartWithWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "1.7b" should (startWith ("1.7") and startWith ("1.7b"))
   *                          ^
   * </pre>
   */
  def apply(right: String): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          left startsWith right,
          Resources("didNotStartWith"),
          Resources("startedWith"), 
          Vector(left, right)
        )
      override def toString: String = "startWith (" + Prettifier.default(right) + ")"
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimal = """(-)?(\d+)(\.\d*)?"""
   * "1.7b" should (startWith regex (decimal) and startWith regex (decimal))
   *                          ^
   * </pre>
   */
  def regex[T <: String](right: T): Matcher[T] = regex(right.r)
  
  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * string should not { startWith regex ("a(b*)c" withGroup "bb") } 
   *                               ^
   * </pre>
   */	
  def regex(regexWithGroups: RegexWithGroups) = 
    new Matcher[String] {
      def apply(left: String): MatchResult = 
        startWithRegexWithGroups(left, regexWithGroups.regex, regexWithGroups.groups)
      override def toString: String = "startWith regex " + regexWithGroups.regex.toString + (if (regexWithGroups.groups.size > 1) " withGroups " else " withGroup ") + regexWithGroups.groups.mkString(", ")
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * val decimalRegex = """(-)?(\d+)(\.\d*)?""".r
   * "1.7" should (startWith regex (decimalRegex) and startWith regex (decimalRegex))
   *                         ^
   * </pre>
   */
  def regex(rightRegex: Regex): Matcher[String] =
    new Matcher[String] {
      def apply(left: String): MatchResult =
        MatchResult(
          rightRegex.pattern.matcher(left).lookingAt,
          Resources("didNotStartWithRegex"),
          Resources("startedWithRegex"), 
          Vector(left, UnquotedString(rightRegex.toString))
        )
      override def toString: String = "startWith regex " + rightRegex.toString
    }
  
  /**
   * Overrides toString to return "startWith"
   */
  override def toString: String = "startWith"
}
