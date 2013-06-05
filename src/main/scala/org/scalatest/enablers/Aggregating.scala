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
package org.scalatest.enablers

import org.scalautils.Equality
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import scala.collection.GenTraversableOnce
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._

trait Aggregating[A] {
  def containsAtLeastOneOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsTheSameElementsAs(aggregation: A, eles: GenTraversable[Any]): Boolean
  def containsTheSameElementsInOrderAs(aggregation: A, eles: GenTraversable[Any]): Boolean
  def containsOnly(aggregation: A, eles: Seq[Any]): Boolean
  def containsInOrderOnly(aggregation: A, eles: Seq[Any]): Boolean
/*  def containsAllOf(aggregation: A, eles: Seq[Any]): Boolean
  def containsAtMostOneOf(aggregation: A, eles: Seq[Any]): Boolean
*/
}

object Aggregating {
  
  private def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean = 
    try equality.areEqual(left.asInstanceOf[T], right)
      catch {
        case cce: ClassCastException => false
    }
  
  private def checkTheSameElementsAs[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    case class ElementCount(element: Any, leftCount: Int, rightCount: Int)
    object ZipNoMatch
    
    def leftNewCount(next: Any, count: IndexedSeq[ElementCount]): IndexedSeq[ElementCount] = {
      val idx = count.indexWhere(ec => tryEquality(next, ec.element, equality))
      if (idx >= 0) {
        val currentElementCount = count(idx)
        count.updated(idx, ElementCount(currentElementCount.element, currentElementCount.leftCount + 1, currentElementCount.rightCount))
      }
      else
        count :+ ElementCount(next, 1, 0)
    }
    
    def rightNewCount(next: Any, count: IndexedSeq[ElementCount]): IndexedSeq[ElementCount] = {
      val idx = count.indexWhere(ec => tryEquality(next, ec.element, equality))
      if (idx >= 0) {
        val currentElementCount = count(idx)
        count.updated(idx, ElementCount(currentElementCount.element, currentElementCount.leftCount, currentElementCount.rightCount + 1))
      }
      else
        count :+ ElementCount(next, 0, 1)
    }
    
    val counts = right.toIterable.zipAll(left.toIterable, ZipNoMatch, ZipNoMatch).aggregate(IndexedSeq.empty[ElementCount])( 
      { case (count, (nextLeft, nextRight)) => 
          if (nextLeft == ZipNoMatch || nextRight == ZipNoMatch)
            return false  // size not match, can fail early
          rightNewCount(nextRight, leftNewCount(nextLeft, count))
      }, 
      { case (count1, count2) =>
          count2.foldLeft(count1) { case (count, next) => 
            val idx = count.indexWhere(ec => tryEquality(next.element, ec.element, equality))
            if (idx >= 0) {
              val currentElementCount = count(idx)
              count.updated(idx, ElementCount(currentElementCount.element, currentElementCount.leftCount + next.leftCount, currentElementCount.rightCount + next.rightCount))
            }
            else
              count :+ next
          }
      }
    )
    
    !counts.exists(e => e.leftCount != e.rightCount)
  }
  
  private def checkTheSameElementsInOrderAs[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def checkEqual(left: Iterator[T], right: Iterator[Any]): Boolean = {
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
    checkEqual(left.toIterator, right.toIterator)
  }

  private def checkOnly[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def findNext(value: T, rightItr: Iterator[Any], processedSet: Set[Any]): Set[Any] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.find(tryEquality(_, nextRight, equality)).isDefined)
          throw new IllegalArgumentException(FailureMessages("onlyDuplicate", nextRight))
        if (tryEquality(nextRight, value, equality))
          processedSet + nextRight
        else
          findNext(value, rightItr, processedSet + nextRight)
      }
      else
        processedSet

    @tailrec
    def checkEqual(leftItr: Iterator[T], rightItr: Iterator[Any], processedSet: Set[Any]): Boolean = {
      if (leftItr.hasNext) {
        val nextLeft = leftItr.next
        if (processedSet.find(tryEquality(_, nextLeft, equality)).isDefined) // The nextLeft is contained in right, let's continue next
          checkEqual(leftItr, rightItr, processedSet)
        else {
          val newProcessedSet = findNext(nextLeft, rightItr, processedSet)
          if (newProcessedSet.find(tryEquality(_, nextLeft, equality)).isDefined) // The nextLeft is contained in right, let's continue next
            checkEqual(leftItr, rightItr, newProcessedSet)
          else // The nextLeft is not in right, let's fail early
            false
        }
      }
      else // No more element in left, left contains only elements of right.
        true
    }
    checkEqual(left.toIterator, right.toIterator, Set.empty)
  }
  
  private def checkInOrderOnly[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
  
    @tailrec
    def checkEqual(left: T, right: Any, leftItr: Iterator[T], rightItr: Iterator[Any]): Boolean = {
      if (equality.areEqual(left, right)) { // The first time in, left must equal right
        // Now need to iterate through the left while it is equal to the right
        @tailrec
        def checkNextLeftAgainstCurrentRight(): Option[T] = { // Returns first left that doesn't match the current right, or None, if all remaining lefts matched current right
          if (leftItr.hasNext) {
            val nextLeft = leftItr.next
            if (equality.areEqual(nextLeft, right))
              checkNextLeftAgainstCurrentRight()
            else
              Some(nextLeft)
          }
          else None // No more lefts
        }
        val nextLeftOption = checkNextLeftAgainstCurrentRight()
        nextLeftOption match {
          case Some(nextLeft) => 
            if (rightItr.hasNext) {
              checkEqual(nextLeft, rightItr.next, leftItr, rightItr)
            }
            else false
          case None => !rightItr.hasNext // No more lefts remaining, so we're good so long as no more rights remaining either.
        }
      }
      else false
    }

    val leftItr: Iterator[T] = left.toIterator
    val rightItr: Iterator[Any] = right.toIterator
    if (leftItr.hasNext && rightItr.hasNext)
      checkEqual(leftItr.next, rightItr.next, leftItr, rightItr)
    else left.isEmpty && right.isEmpty
  }

  implicit def withGenTraversableElementEquality[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Aggregating[TRAV[E]] = 
    new Aggregating[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
      def containsTheSameElementsAs(trav: TRAV[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
      def containsTheSameElementsInOrderAs(trav: TRAV[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
      def containsOnly(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
      def containsInOrderOnly(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregating[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Aggregating[TRAV[E]] = 
    withGenTraversableElementEquality(equality)
    
  implicit def withArrayElementEquality[E](implicit equality: Equality[E]): Aggregating[Array[E]] = 
    new Aggregating[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
      def containsTheSameElementsAs(array: Array[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs[E](new ArrayWrapper(array), elements, equality)
      }
      def containsTheSameElementsInOrderAs(array: Array[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](new ArrayWrapper(array), elements, equality)
      }
      def containsOnly(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(new ArrayWrapper(array), elements, equality)
      }
      def containsInOrderOnly(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        throw new UnsupportedOperationException("Not Implemented")
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] = 
    withArrayElementEquality(equality)
  
  implicit def withStringCharacterEquality(implicit equality: Equality[Char]): Aggregating[String] = 
    new Aggregating[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[Char], ele)))
      }
      def containsTheSameElementsAs(s: String, elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(s, elements, equality)
      }
      def containsTheSameElementsInOrderAs(s: String, elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(s, elements, equality)
      }
      def containsOnly(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(s, elements, equality)
      }
      def containsInOrderOnly(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        throw new UnsupportedOperationException("Not Implemented")
      }
    }

  implicit def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] = 
    withStringCharacterEquality(equality)
    
  implicit def withGenMapElementEquality[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    new Aggregating[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
      def containsTheSameElementsAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
      def containsTheSameElementsInOrderAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
      def containsOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
      def containsInOrderOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        throw new UnsupportedOperationException("Not Implemented")
      }
    }

  implicit def convertEqualityToGenMapAggregating[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    withGenMapElementEquality(equality)
    
  implicit def withJavaCollectionElementEquality[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Aggregating[JCOL[E]] = 
    new Aggregating[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        col.asInstanceOf[java.util.Collection[E]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
      def containsTheSameElementsAs(col: JCOL[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
      def containsTheSameElementsInOrderAs(col: JCOL[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
      def containsOnly(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
      def containsInOrderOnly(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        throw new UnsupportedOperationException("Not Implemented")
      }
    }

  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Aggregating[JCOL[E]] = 
    withJavaCollectionElementEquality(equality)
    
  implicit def withJavaMapElementEquality[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[(K, V)]): Aggregating[JMAP[K, V]] = 
    new Aggregating[JMAP[K, V]] {
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.asInstanceOf[java.util.Map[K, V]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
      def containsTheSameElementsAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(map.asInstanceOf[java.util.Map[K, V]].asScala, elements, equality)
      }
      def containsTheSameElementsInOrderAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map.asInstanceOf[java.util.Map[K, V]].asScala, elements, equality)
      }
      def containsOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(map.asInstanceOf[java.util.Map[K, V]].asScala, elements, equality)
      }
      def containsInOrderOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        throw new UnsupportedOperationException("Not Implemented")
      }
    }

  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[(K, V)]): Aggregating[JMAP[K, V]] = 
    withJavaMapElementEquality(equality)
}
