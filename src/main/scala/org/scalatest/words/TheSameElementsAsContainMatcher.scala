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
class TheSameElementsAsContainMatcher[T](right: GenTraversable[T], equality: Equality[T]) extends ContainMatcher[T] {
  @tailrec
  private def checkEqual(left: Iterator[T], right: Iterator[T], remains: IndexedSeq[T]): Boolean = {
    if (left.hasNext) {
      val nextLeft = left.next
      // Let's look from the remains first
      val idx = remains.indexWhere(equality.areEqual(_, nextLeft))
      if (idx >= 0) {
        // Found in remains, let's remove it from remains and continue
        val (first, second) = remains.splitAt(idx)
        checkEqual(left, right, first ++: second.tail)
      }
      else {
        // Not found in remains, let's try right iterator
        if (right.isEmpty) // right is empty, so the element not found
          false
        else {
          val (newRemains, found) = takeUntilFound(right, nextLeft, IndexedSeq.empty)
          if (found)
            checkEqual(left, right, remains ++: newRemains.toIndexedSeq)
          else // Not found in right iterator
            false
        }
      }
    }
    else
      left.isEmpty && right.isEmpty && remains.isEmpty
  }
  
  @tailrec
  private def takeUntilFound(itr: Iterator[T], target: T, taken: IndexedSeq[T]): (IndexedSeq[T], Boolean) = {
    if (itr.hasNext) {
      val next = itr.next
      if (equality.areEqual(next, target))
        (taken, true)
      else
        takeUntilFound(itr, target, taken :+ next)
    }
    else
      (taken, false)
  }
  
  /**
   * This method contains the matching code for theSameElementsAs.
   */
  def apply(left: GenTraversable[T]): MatchResult = 
    MatchResult(
      checkEqual(left.toIterator, right.toIterator, IndexedSeq.empty), 
      FailureMessages("didNotContainSameElements", left, right), 
      FailureMessages("containedSameElements", left, right)
    )
}

