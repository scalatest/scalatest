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
class InOrderOnlyContainMatcher[T](right: GenTraversable[T], equality: Equality[T]) extends ContainMatcher[T] {
  
  @tailrec
  private def findNext(value: T, rightItr: Iterator[T], processedList: IndexedSeq[T]): IndexedSeq[T] = 
    if (rightItr.hasNext) {
      val nextRight = rightItr.next
      if (processedList.find(equality.areEqual(_, nextRight)).isDefined)
          throw new IllegalArgumentException(FailureMessages("inOrderOnlyDuplicate", nextRight))
      if (equality.areEqual(value, nextRight))
        processedList :+ nextRight
      else
        findNext(value, rightItr, processedList :+ nextRight)
    }
    else
      processedList
  
  @tailrec
  private def checkEqual(leftItr: Iterator[T], rightItr: Iterator[T], currentRight: T, processedList: IndexedSeq[T]): Boolean = {
    
    if (leftItr.hasNext) {
      val nextLeft = leftItr.next
      if (equality.areEqual(nextLeft, currentRight)) // The nextLeft is contained in right, let's continue next
        checkEqual(leftItr, rightItr, currentRight, processedList)
      else {
        val newProcessedList = findNext(nextLeft, rightItr, processedList)
        if (equality.areEqual(nextLeft, newProcessedList.last)) // The nextLeft is contained in right, let's continue next
          checkEqual(leftItr, rightItr, nextLeft, newProcessedList) // nextLeft will be the new currentRight
        else // The nextLeft is not in right, let's fail early
          false
      }
    }
    else // No more element in left, left contains only elements of right.
      true
  }
  
  /**
   * This method contains the matching code for inOrderOnly.
   */
  def apply(left: GenTraversable[T]): MatchResult = {
    val rightItr = right.toIterator
    val rightFirst = rightItr.next
    MatchResult(
      if (rightItr.hasNext) checkEqual(left.toIterator, rightItr, rightFirst, IndexedSeq(rightFirst)) else left.isEmpty, 
      FailureMessages("didNotContainInOrderOnlyElements", left, UnquotedString(right.mkString(", "))),
      FailureMessages("containedInOrderOnlyElements", left, UnquotedString(right.mkString(", ")))
    )
  }
}

