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
class TheSameIteratedElementsAsContainMatcher[T](right: GenTraversable[T], equality: Equality[T]) extends ContainMatcher[T] {
  @tailrec
  private def checkEqual(left: Iterator[T], right: Iterator[T]): Boolean = {
    if (left.hasNext && right.hasNext) {
      val nextLeft = left.next
      val nextRight = right.next
      if (!equality.areEqual(nextLeft, nextRight))
        false
      else
        checkEqual(left, right)
    }
    else
      left.isEmpty && right.isEmpty
  }
  
  /**
   * This method contains the matching code for theSameIteratedElementsAs.
   */
  def apply(left: GenTraversable[T]): MatchResult = 
    MatchResult(
      checkEqual(left.toIterator, right.toIterator), 
      FailureMessages("didNotContainSameIteratedElements", left, right), 
      FailureMessages("containedSameIteratedElements", left, right)
    )
  
}

