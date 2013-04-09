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
import scala.collection.GenTraversable
import org.scalatest.FailureMessages
import org.scalatest.UnquotedString
import org.scalautils.Equality
import scala.annotation.tailrec

/**
 * This class is part of the ScalaTest matchers DSL. Please see the documentation for <a href="../Matchers.html"><code>Matchers</code></a> for an overview of
 * the matchers DSL.
 *
 * @author Bill Venners
 */
class OneOfContainMatcher[T](right: GenTraversable[T], equality: Equality[T]) extends ContainMatcher[T] {
  
  @tailrec
  private def checkEqual(left: GenTraversable[T], rightItr: Iterator[T], processedSet: Set[T]): Boolean = {
    
    if (rightItr.hasNext) {
      val nextRight = rightItr.next
      if (processedSet.find(equality.areEqual(_, nextRight)).isDefined)
          throw new IllegalArgumentException(FailureMessages("oneOfDuplicate", nextRight))
      if (left.exists(t => equality.areEqual(t, nextRight))) // Found one of right in left, can succeed early
        true
      else
        checkEqual(left, rightItr, processedSet + nextRight)
    }
    else // No more element in right, left does not contain one of right.
      false
  }
  
  /**
   * This method contains the matching code for oneOf.
   */
  def apply(left: GenTraversable[T]): MatchResult = 
    MatchResult(
      checkEqual(left, right.toIterator, Set.empty), 
      FailureMessages("didNotContainOneOfElements", left, UnquotedString(right.mkString(", "))),
      FailureMessages("containedOneOfElements", left, UnquotedString(right.mkString(", ")))
    )
}

