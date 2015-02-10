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
package org.scalatest.words

import org.scalactic.Prettifier
import org.scalatest.{Suite, Resources}
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.Assertions.areEqualComparingArraysStructurally
import org.scalatest.MatchersHelper.matchSymbolToPredicateMethod

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */

final class BeEqualEqualWord {

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * "hi" should not have length (3)
   *                             ^
   * </pre>
   */

  /**
   * This method enables <code>be_==</code> to be used for equality comparison (using Scala default ==):
   *
   * <pre class="stHighlight">
   * sum should be_== (19)
   *            ^
   * </pre>
   */
  def apply(right: Any): Matcher[Any] =
    new Matcher[Any] {
      def apply(left: Any): MatchResult = {
        val (leftee, rightee) = Suite.getObjectsForFailureMessage(left, right) // TODO: To move this to reporter
        MatchResult(
          areEqualComparingArraysStructurally(left, right),
          Resources("wasNotEqualTo"),
          Resources("wasEqualTo"),
          Vector(leftee, rightee),
          Vector(left, right)
        )
      }

      override def toString: String = "be_== (" + Prettifier.default(right) + ")"
    }

  /**
   * This method enables the following syntax:
   *
   * <pre class="stHighlight">
   * result should be_== (null)
   *               ^
   * </pre>
   */
  def apply(o: Null): Matcher[AnyRef] =
    new Matcher[AnyRef] {
      def apply(left: AnyRef): MatchResult = {
        MatchResult(
          left == null,
          Resources("wasNotNull"),
          Resources("wasNull"),
          Resources("wasNotNull"),
          Resources("midSentenceWasNull"),
          Vector(left),
          Vector.empty
        )
      }
      override def toString: String = "be_== (null)"
    }

  /**
   * Overrides toString to return "length"
   */
  override def toString: String = "be_=="
}
