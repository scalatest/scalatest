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

import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalatest.MatchersHelper.Collected
import org.scalatest.MatchersHelper.doCollected
import org.scalatest.MatchersHelper.containsOneOf
import org.scalatest.MatchersHelper.newTestFailedException
import org.scalatest.enablers.Holder

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="InspectorsMatchers.html"><code>InspectorsMatchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 * @author Chee Seng
 */
sealed class ResultOfNewContainWordForCollectedAny[T](collected: Collected, xs: scala.collection.GenTraversable[T], shouldBeTrue: Boolean) {
  
  /**
   * This method enables the following syntax: 
   *
   * <pre class="stHighlight">
   * option should contain oneOf (1, 2)
   *                       ^
   * </pre>
   */
  def newOneOf(right: Any*)(implicit holder: Holder[T]) {
    doCollected(collected, xs, "ResultOfNewContainWordForCollectedAny.scala", "newOneOf", 1) { e =>
      if (containsOneOf(e, right.toIterator) != shouldBeTrue)
        throw newTestFailedException(
          FailureMessages(
            if (shouldBeTrue) "didNotContainOneOfElements" else "containedOneOfElements",
            e,
            UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))
          ),
          None,
          6
      )
    }
  }
}