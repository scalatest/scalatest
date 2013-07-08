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
import org.scalatest.FailureMessages
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Typeclass that enables for aggregations certain <code>contain</code> syntax in the ScalaTest matchers DSL.
 *
 * <p>
 * An <code>Aggregating[A]</code> provides access to the "aggregating nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of <em>aggregation</em>&#8212;an object that in some way aggregates or brings together other objects. ScalaTest provides
 * implicit implementations for several types out of the box in the
 * <a href="Aggregating$.html"><code>Aggregating</code> companion object</a>:
 * </p>
 * 
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>scala.collection.Iterator</code> (coming soon)</li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * <li><code>java.util.Iterator</code> (coming soon)</li>
 * </ul>
 * 
 * <p>
 * The <code>contain</code> syntax enabled by this trait is:
 * <p>
 * 
 * <ul>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atLeastOneOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atMostOneOf</code> <code>(1, 2, 3)</code> (coming soon)</li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>only</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>allOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>theSameElementsAs</code> <code>(List(1, 2, 3))</code></li>
 * </ul>
 * 
 * <p>
 * You can enable the <code>contain</code> matcher syntax enabled by <code>Aggregating</code> on your own
 * type <code>U</code> by defining an <code>Aggregating[U]</code> for the type and making it available implicitly.
 * </p>
 *
 * <p>
 * Note, for an explanation of the difference between <code>Containing</code> and <code>Aggregating</code>, both of which
 * enable <code>contain</code> matcher syntax, see the <a href="Containing.html#containingVersusAggregating">Containing
 * versus Aggregating</a> section of the main documentation for trait <code>Containing</code>.
 * </p>
 */
trait Aggregating[A] {

// TODO: Write tests that a NotAllowedException is thrown when no elements are passed, maybe if only one element is passed, and 
// likely if an object is repeated in the list.
  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def containsAtLeastOneOf(aggregation: A, eles: Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>theSameElementsAs</code> syntax for aggregations of type <code>A</code>.
   *
   * @param leftAggregation an aggregation about which an assertion is being made
   * @param rightAggregation an aggregation that should contain the same elements as the passed <code>leftAggregation</code>
   * @return true if the passed <code>leftAggregation</code> contains the same elements as the passed <code>rightAggregation</code>
   */
  def containsTheSameElementsAs(leftAggregation: A, rightAggregation: GenTraversable[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>only</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles the only elements that should be contained in the passed aggregation
   * @return true if the passed aggregation contains only the passed elements
   */
  def containsOnly(aggregation: A, eles: Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>allOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements all of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains all of the passed elements
   */
  def containsAllOf(aggregation: A, eles: Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>atMostOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at most one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at most one of the passed elements
   */
  def containsAtMostOneOf(aggregation: A, eles: Seq[Any]): Boolean
}

object Aggregating {

  // TODO: Throwing exceptions is slow. Just do a pattern match and test the type before trying to cast it.
  private[scalatest] def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean = 
    try equality.areEqual(left.asInstanceOf[T], right)
      catch {
        case cce: ClassCastException => false
    }
  
  private[scalatest] def checkTheSameElementsAs[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
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
  
  private[scalatest] def checkOnly[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def findNext(value: T, rightItr: Iterator[Any], processedSet: Set[Any]): Set[Any] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
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
      else 
        !rightItr.hasNext // No more lefts remaining, so we're good so long as no more rights remaining either.
    }
    checkEqual(left.toIterator, right.toIterator, Set.empty)
  }
  
  private[scalatest] def checkAllOf[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def checkEqual(left: GenTraversable[T], rightItr: Iterator[Any]): Boolean = {
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (left.exists(t => equality.areEqual(t, nextRight))) 
          checkEqual(left, rightItr)
        else
          false // Element not found, let's fail early
      }
      else // No more element in right, left contains all of right.
        true
    }
    checkEqual(left, right.toIterator)
  }
  
  private[scalatest] def checkAtMostOneOf[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    
    def countElements: Int = 
      right.aggregate(0)(
        { case (count, nextRight) => 
            if (left.exists(l => equality.areEqual(l, nextRight))) {
              val newCount = count + 1
              if (newCount > 1)
                return newCount
              else
                newCount
            }
            else
              count
        }, 
        { case (count1, count2) => count1 + count2 }
      )
    val count = countElements
    count <= 1      
  }

  implicit def aggregatingNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e]](implicit equality: Equality[E]): Aggregating[TRAV[E]] = 
    new Aggregating[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(trav: TRAV[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs[E](trav, elements, equality)
      }
      def containsOnly(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly[E](trav, elements, equality)
      }
      def containsAllOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(trav, elements, equality)
      }
      def containsAtMostOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(trav, elements, equality)
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregating[E, TRAV[e] <: scala.collection.GenTraversable[e]](equality: Equality[E]): Aggregating[TRAV[E]] = 
    aggregatingNatureOfGenTraversable(equality)
    
  implicit def aggregatingNatureOfArray[E](implicit equality: Equality[E]): Aggregating[Array[E]] = 
    new Aggregating[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(array: Array[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs[E](new ArrayWrapper(array), elements, equality)
      }
      def containsOnly(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(new ArrayWrapper(array), elements, equality)
      }
      def containsAllOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(new ArrayWrapper(array), elements, equality)
      }
      def containsAtMostOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(new ArrayWrapper(array), elements, equality)
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] = 
    aggregatingNatureOfArray(equality)
  
  implicit def aggregatingNatureOfString(implicit equality: Equality[Char]): Aggregating[String] = 
    new Aggregating[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Char) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(s: String, elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(s, elements, equality)
      }
      def containsOnly(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(s, elements, equality)
      }
      def containsAllOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(s, elements, equality)
      }
      def containsAtMostOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(s, elements, equality)
      }
    }

  implicit def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] = 
    aggregatingNatureOfString(equality)
    
  implicit def aggregatingNatureOfGenMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](implicit equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    new Aggregating[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: (K, V)) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(map, elements, equality)
      }
      def containsOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(map, elements, equality)
      }
      def containsAllOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(map, elements, equality)
      }
      def containsAtMostOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(map, elements, equality)
      }
    }

  implicit def convertEqualityToGenMapAggregating[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    aggregatingNatureOfGenMap(equality)
    
  implicit def aggregatingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]](implicit equality: Equality[E]): Aggregating[JCOL[E]] = 
    new Aggregating[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        col.asScala.exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(col: JCOL[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(col.asScala, elements, equality)
      }
      def containsOnly(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(col.asScala, elements, equality)
      }
      def containsAllOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(col.asScala, elements, equality)
      }
      def containsAtMostOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(col.asScala, elements, equality)
      }
    }

  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[e] <: java.util.Collection[e]](equality: Equality[E]): Aggregating[JCOL[E]] = 
    aggregatingNatureOfJavaCollection(equality)
    
  implicit def aggregatingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = 
    new Aggregating[JMAP[K, V]] {
    
      import scala.collection.JavaConverters._
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(map.entrySet.asScala, elements, equality)
      }
      def containsOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(map.entrySet.asScala, elements, equality)
      }
      def containsAllOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(map.entrySet.asScala, elements, equality)
      }
      def containsAtMostOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(map.entrySet.asScala, elements, equality)
      }
    }

  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = 
    aggregatingNatureOfJavaMap(equality)
}
