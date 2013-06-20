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
 * Supertrait for typeclasses that enable <code>contain</code> matcher syntax for aggregations.
 *
 * <p>
 * An <code>Aggregating[A]</code> provides access to the "aggregating nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of "aggregation," a type that in some way aggregates or brings together other types. ScalaTest provides
 * implicit implementations for several types. You can enable the <code>contain</code> matcher syntax on your own
 * type <code>U</code> by defining an <code>Aggregating[U]</code> for the type and making it available implicitly.
 * 
 * <p>
 * ScalaTest provides implicit <code>Aggregating</code> instances for <code>scala.collection.GenTraversable</code>,
 * <code>java.util.Collection</code>, <code>java.util.Map</code>, <code>String</code>, and <code>Array</code> in the
 * <code>Aggregating</code> companion object.
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

/*  def containsAtMostOneOf(aggregation: A, eles: Seq[Any]): Boolean
*/
}

object Aggregating {

  // TODO: Throwing exceptions is slow. Just do a pattern match and test the type before trying to cast it.
  private[scalatest] def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean = 
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
      else 
        !rightItr.hasNext // No more lefts remaining, so we're good so long as no more rights remaining either.
    }
    checkEqual(left.toIterator, right.toIterator, Set.empty)
  }
  
  private def checkAllOf[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def checkEqual(left: GenTraversable[T], rightItr: Iterator[Any], processedSet: Set[Any]): Boolean = {
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (processedSet.contains(nextRight))
          throw new IllegalArgumentException(FailureMessages("allOfDuplicate", nextRight))
        if (left.exists(t => equality.areEqual(t, nextRight))) 
          checkEqual(left, rightItr, processedSet + nextRight)
        else
          false // Element not found, let's fail early
      }
      else // No more element in right, left contains all of right.
        true
    }
    checkEqual(left, right.toIterator, Set.empty)
  }
  
  implicit def aggregatingNatureOfGenTraversable[E, TRAV[_] <: scala.collection.GenTraversable[_]](implicit equality: Equality[E]): Aggregating[TRAV[E]] = 
    new Aggregating[TRAV[E]] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        trav.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
      def containsTheSameElementsAs(trav: TRAV[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
      def containsOnly(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly[E](trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
      def containsAllOf(trav: TRAV[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(trav.asInstanceOf[GenTraversable[E]], elements, equality)
      }
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToGenTraversableAggregating[E, TRAV[_] <: scala.collection.GenTraversable[_]](equality: Equality[E]): Aggregating[TRAV[E]] = 
    aggregatingNatureOfGenTraversable(equality)
    
  implicit def aggregatingNatureOfArray[E](implicit equality: Equality[E]): Aggregating[Array[E]] = 
    new Aggregating[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
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
    }

  // Enables (xs should contain ("HI")) (after being lowerCased)
  implicit def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] = 
    aggregatingNatureOfArray(equality)
  
  implicit def aggregatingNatureOfString(implicit equality: Equality[Char]): Aggregating[String] = 
    new Aggregating[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[Char], ele)))
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
    }

  implicit def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] = 
    aggregatingNatureOfString(equality)
    
  implicit def aggregatingNatureOfGenMap[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](implicit equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    new Aggregating[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
      }
      def containsTheSameElementsAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
      def containsOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
      def containsAllOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(map.asInstanceOf[scala.collection.GenMap[K, V]], elements, equality)
      }
    }

  implicit def convertEqualityToGenMapAggregating[K, V, MAP[_, _] <: scala.collection.GenMap[_, _]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = 
    aggregatingNatureOfGenMap(equality)
    
  implicit def aggregatingNatureOfJavaCollection[E, JCOL[_] <: java.util.Collection[_]](implicit equality: Equality[E]): Aggregating[JCOL[E]] = 
    new Aggregating[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        col.asInstanceOf[java.util.Collection[E]].asScala.exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[E], ele)))
      }
      def containsTheSameElementsAs(col: JCOL[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsAs(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
      def containsOnly(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
      def containsAllOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(col.asInstanceOf[java.util.Collection[E]].asScala, elements, equality)
      }
    }

  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[_] <: java.util.Collection[_]](equality: Equality[E]): Aggregating[JCOL[E]] = 
    aggregatingNatureOfJavaCollection(equality)
    
  implicit def aggregatingNatureOfJavaMap[K, V, JMAP[_, _] <: java.util.Map[_, _]](implicit equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = 
    new Aggregating[JMAP[K, V]] {
      // This is needed as asScala does not preserve the original iterated order
      /*private def getScalaMapInOrder(javaMap: JMAP[K, V]): scala.collection.GenMap[K, V] = {
        val map = new collection.mutable.LinkedHashMap[K, V]
        val itr = javaMap.entrySet.iterator
        while (itr.hasNext) {
          val entry = itr.next
          map += ((entry.getKey.asInstanceOf[K], entry.getValue.asInstanceOf[V]))
        }
        map
      }*/
    
      import scala.collection.JavaConverters._
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        //getScalaMapInOrder(map).exists((e: Any) => elements.exists((ele: Any) => equality.areEqual(e.asInstanceOf[(K, V)], ele)))
        map.asInstanceOf[java.util.Map[K, V]].entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        //checkTheSameElementsAs(getScalaMapInOrder(map), elements, equality)
        checkTheSameElementsAs(map.asInstanceOf[java.util.Map[K, V]].entrySet.asScala, elements, equality)
      }
      def containsOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        //checkOnly(getScalaMapInOrder(map), elements, equality)
        checkOnly(map.asInstanceOf[java.util.Map[K, V]].entrySet.asScala, elements, equality)
      }
      def containsAllOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        //checkAllOf(getScalaMapInOrder(map), elements, equality)
        checkAllOf(map.asInstanceOf[java.util.Map[K, V]].entrySet.asScala, elements, equality)
      }
    }

  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[_, _] <: java.util.Map[_, _]](equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = 
    aggregatingNatureOfJavaMap(equality)
}
