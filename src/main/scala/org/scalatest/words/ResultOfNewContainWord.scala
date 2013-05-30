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

import org.scalatest.enablers.Containing
import org.scalatest.enablers.Aggregating
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString

/*
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
class ResultOfNewContainWord[L](left: L, shouldBeTrue: Boolean = true) {

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * option should contain oneOf (1, 2)
   *                       ^
   * </pre>
   */
  def newOneOf(right: Any*)(implicit holder: Containing[L]) {
    if (holder.containsOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }

  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * option should contain atLeastOneOf (1, 2)
   *                       ^
   * </pre>
   */
  def atLeastOneOf(right: Any*)(implicit aggregation: Aggregating[L]) {
    if (aggregation.containsAtLeastOneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "didNotContainAtLeastOneOf" else "containedAtLeastOneOf",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * option should contain noneOf (1, 2)
   *                       ^
   * </pre>
   */
  def newNoneOf(right: Any*)(implicit holder: Containing[L]) {
    if (holder.containsNoneOf(left, right) != shouldBeTrue)
      throw newTestFailedException(
        FailureMessages(
          if (shouldBeTrue) "containedOneOfElements" else "didNotContainOneOfElements",
          left,
          UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
        )
      )
  }
}

