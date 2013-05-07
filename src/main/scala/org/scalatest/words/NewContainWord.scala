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

import org.scalatest.enablers.Holder
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.MatcherFactory1
import org.scalatest.matchers.MatchResult
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import scala.annotation.tailrec

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
final class NewContainWord {
  def newOneOf(right: Any*): MatcherFactory1[Any, Holder] = {
    new MatcherFactory1[Any, Holder] {
      def matcher[T](implicit holder: Holder[T]): Matcher[T] = {
        new Matcher[T] {
          def apply(left: T): MatchResult = {
        
            @tailrec
            def containsOneOf(left: T, rightItr: Iterator[Any], processedSet: Set[Any]): Boolean = {
              if (rightItr.hasNext) {
                val nextRight = rightItr.next
                if (holder.containsElement(left, nextRight)) // Found one of right in left, can succeed early
                  true
                else
                  containsOneOf(left, rightItr, processedSet + nextRight)
              }
              else // No more elements in right, left does not contain one of right.
                false
            }

            MatchResult(
              containsOneOf(left, right.toIterator, Set.empty),
              FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", "))),
              FailureMessages("containedOneOfElements", left, UnquotedString(right.map(FailureMessages.decorateToStringValue).mkString(", ")))
            )
          }
        }
      }
    }
  }
}

