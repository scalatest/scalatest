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
package org.scalactic.enablers

import org.scalactic.{Equality, Every, EqualityConstraint, ArrayWrapper}
import org.scalactic.EqualityPolicy.BasicEqualityConstraint
import scala.collection.GenTraversable
import scala.annotation.tailrec
import scala.collection.JavaConverters._
import annotation.implicitNotFound

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
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 * 
 * <p>
 * The <code>contain</code> syntax enabled by this trait is:
 * <p>
 * 
 * <ul>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atLeastOneOf</code> <code>(1, 2, 3)</code></li>
 * <li><code>result</code> <code>should</code> <code>contain</code> <code>atMostOneOf</code> <code>(1, 2, 3)</code></li>
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
@implicitNotFound(msg = "Could not find evidence that ${R} can be contained in ${A}; the missing implicit parameter is of type org.scalactic.enablers.AggregatingConstraint[${A},${R}]")
trait AggregatingConstraint[-A, R] {

// TODO: Write tests that a NotAllowedException is thrown when no elements are passed, maybe if only one element is passed, and 
// likely if an object is repeated in the list.
  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def containsAtLeastOneOf(aggregation: A, eles: Seq[R]): Boolean

  /**
   * Implements <code>contain</code> <code>theSameElementsAs</code> syntax for aggregations of type <code>A</code>.
   *
   * @param leftAggregation an aggregation about which an assertion is being made
   * @param rightAggregation an aggregation that should contain the same elements as the passed <code>leftAggregation</code>
   * @return true if the passed <code>leftAggregation</code> contains the same elements as the passed <code>rightAggregation</code>
   */
  def containsTheSameElementsAs(leftAggregation: A, rightAggregation: GenTraversable[R]): Boolean

  /**
   * Implements <code>contain</code> <code>only</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles the only elements that should be contained in the passed aggregation
   * @return true if the passed aggregation contains only the passed elements
   */
  def containsOnly(aggregation: A, eles: Seq[R]): Boolean

  /**
   * Implements <code>contain</code> <code>allOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements all of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains all of the passed elements
   */
  def containsAllOf(aggregation: A, eles: Seq[R]): Boolean

  /**
   * Implements <code>contain</code> <code>atMostOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at most one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at most one of the passed elements
   */
  def containsAtMostOneOf(aggregation: A, eles: Seq[R]): Boolean
}

/**
 * Companion object for <code>Aggregating</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenTraversable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
object AggregatingConstraint {

  // TODO: Throwing exceptions is slow. Just do a pattern match and test the type before trying to cast it.
  private[scalactic] def tryEquality[L, R](left: Any, right: Any, constraint: EqualityConstraint[L, R]): Boolean = 
    try constraint.areEqual(left.asInstanceOf[L], right.asInstanceOf[R])
      catch {
        case cce: ClassCastException => false
    }
  
  
  private[scalactic] def checkTheSameElementsAs[L, R](left: GenTraversable[L], right: GenTraversable[R], constraint: EqualityConstraint[L, R]): Boolean = {
    case class ElementCount(element: Any, leftCount: Int, rightCount: Int)
    object ZipNoMatch
    
    def leftNewCount(next: Any, count: IndexedSeq[ElementCount]): IndexedSeq[ElementCount] = {
      val idx = count.indexWhere(ec => tryEquality(next, ec.element, constraint))
      if (idx >= 0) {
        val currentElementCount = count(idx)
        count.updated(idx, ElementCount(currentElementCount.element, currentElementCount.leftCount + 1, currentElementCount.rightCount))
      }
      else
        count :+ ElementCount(next, 1, 0)
    }
    
    def rightNewCount(next: Any, count: IndexedSeq[ElementCount]): IndexedSeq[ElementCount] = {
      val idx = count.indexWhere(ec => tryEquality(next, ec.element, constraint))
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
            val idx = count.indexWhere(ec => tryEquality(next.element, ec.element, constraint))
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
  
  private[scalactic] def checkOnly[L, R](left: GenTraversable[L], right: GenTraversable[R], constraint: EqualityConstraint[L, R]): Boolean = {
    @tailrec
    def findNext(value: L, rightItr: Iterator[R], processedSet: Set[R]): Set[R] = 
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (tryEquality(nextRight, value, constraint))
          processedSet + nextRight
        else
          findNext(value, rightItr, processedSet + nextRight)
      }
      else
        processedSet

    @tailrec
    def checkEqual(leftItr: Iterator[L], rightItr: Iterator[R], processedSet: Set[R]): Boolean = {
      if (leftItr.hasNext) {
        val nextLeft = leftItr.next
        if (processedSet.find(tryEquality(_, nextLeft, constraint)).isDefined) // The nextLeft is contained in right, let's continue next
          checkEqual(leftItr, rightItr, processedSet)
        else {
          val newProcessedSet = findNext(nextLeft, rightItr, processedSet)
          if (newProcessedSet.find(tryEquality(_, nextLeft, constraint)).isDefined) // The nextLeft is contained in right, let's continue next
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
  
  private[scalactic] def checkAllOf[L, R](left: GenTraversable[L], right: GenTraversable[R], constraint: EqualityConstraint[L, R]): Boolean = {
    @tailrec
    def checkEqual(left: GenTraversable[L], rightItr: Iterator[R]): Boolean = {
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        if (left.exists(t => constraint.areEqual(t, nextRight))) 
          checkEqual(left, rightItr)
        else
          false // Element not found, let's fail early
      }
      else // No more element in right, left contains all of right.
        true
    }
    checkEqual(left, right.toIterator)
  }
  
  private[scalactic] def checkAtMostOneOf[L, R](left: GenTraversable[L], right: GenTraversable[R], constraint: EqualityConstraint[L, R]): Boolean = {
    
    def countElements: Int = 
      right.aggregate(0)(
        { case (count, nextRight) => 
            if (left.exists(l => constraint.areEqual(l, nextRight))) {
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

  import scala.language.higherKinds

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>GenTraversable</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>GenTraversable</code>
   * @tparam E the type of the element in the <code>GenTraversable</code>
   * @tparam TRAV any subtype of <code>GenTraversable</code>
   * @return <code>Aggregating[TRAV[E]]</code> that supports <code>GenTraversable</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfGenTraversable[E, TRAV[e] <: scala.collection.GenTraversable[e], R](implicit constraint: EqualityConstraint[E, R]): AggregatingConstraint[TRAV[E], R] = 
    new AggregatingConstraint[TRAV[E], R] {
      def containsAtLeastOneOf(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        trav.exists((e: E) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(trav: TRAV[E], elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs[E, R](trav, elements, constraint)
      }
      def containsOnly(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        checkOnly[E, R](trav, elements, constraint)
      }
      def containsAllOf(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf[E, R](trav, elements, constraint)
      }
      def containsAtMostOneOf(trav: TRAV[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf[E, R](trav, elements, constraint)
      }
    }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Aggregating</code> of type <code>TRAV[E]</code>, where <code>TRAV</code> is a subtype of <code>GenTraversable</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (List("hi") should contain ("HI")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[List[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>GenTraversable</code>
   * @tparam TRAV subtype of <code>GenTraversable</code>
   * @return <code>Aggregating</code> of type <code>TRAV[E]</code>
   */
  implicit def convertEqualityToGenTraversableAggregating[E, TRAV[e] <: scala.collection.GenTraversable[e], R](equality: Equality[E]): AggregatingConstraint[TRAV[E], R] =
    aggregatingNatureOfGenTraversable(new BasicEqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>Array</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Aggregating[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfArray[E, R](implicit constraint: EqualityConstraint[E, R]): AggregatingConstraint[Array[E], R] = 
    new AggregatingConstraint[Array[E], R] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        new ArrayWrapper(array).exists((e: E) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(array: Array[E], elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs[E, R](new ArrayWrapper(array), elements, constraint)
      }
      def containsOnly(array: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        checkOnly(new ArrayWrapper(array), elements, constraint)
      }
      def containsAllOf(array: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf(new ArrayWrapper(array), elements, constraint)
      }
      def containsAtMostOneOf(array: Array[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf(new ArrayWrapper(array), elements, constraint)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Aggregating</code> of type <code>Array[E]</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Array("hi") should contain ("HI")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[Array[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>Array</code>
   * @return <code>Aggregating</code> of type <code>Array[E]</code>
   */
  implicit def convertEqualityToArrayAggregating[E, R](equality: Equality[E]): AggregatingConstraint[Array[E], R] = 
    aggregatingNatureOfArray(new BasicEqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>String</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
   * @return <code>Aggregating[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfString[R](implicit constraint: EqualityConstraint[Char, R]): AggregatingConstraint[String, R] = 
    new AggregatingConstraint[String, R] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[R]): Boolean = {
        s.exists((e: Char) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(s: String, elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs(s, elements, constraint)
      }
      def containsOnly(s: String, elements: scala.collection.Seq[R]): Boolean = {
        checkOnly(s, elements, constraint)
      }
      def containsAllOf(s: String, elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf(s, elements, constraint)
      }
      def containsAtMostOneOf(s: String, elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf(s, elements, constraint)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
   * into <code>Aggregating</code> of type <code>String</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * // lowerCased needs to be implemented as Normalization[Char]
   * ("hi hello" should contain ('E')) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[String]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
   * @return <code>Aggregating</code> of type <code>String</code>
   */
  implicit def convertEqualityToStringAggregating[R](equality: Equality[Char]): AggregatingConstraint[String, R] =
    aggregatingNatureOfString(new BasicEqualityConstraint[Char, R](equality))

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>java.util.Collection</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
   * @tparam E the type of the element in the <code>java.util.Collection</code>
   * @tparam JCOL any subtype of <code>java.util.Collection</code>
   * @return <code>Aggregating[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e], R](implicit constraint: EqualityConstraint[E, R]): AggregatingConstraint[JCOL[E], R] = 
    new AggregatingConstraint[JCOL[E], R] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        col.asScala.exists((e: E) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(col: JCOL[E], elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs(col.asScala, elements, constraint)
      }
      def containsOnly(col: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        checkOnly(col.asScala, elements, constraint)
      }
      def containsAllOf(col: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf(col.asScala, elements, constraint)
      }
      def containsAtMostOneOf(col: JCOL[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf(col.asScala, elements, constraint)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Aggregating</code> of type <code>JCOL[E]</code>, where <code>JCOL</code> is a subtype of <code>java.util.Collection</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaList = new java.util.ArrayList[String]()
   * javaList.add("hi")
   * (javaList should contain ("HI")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[java.util.ArrayList[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>java.util.Collection</code>
   * @tparam JCOL subtype of <code>java.util.Collection</code>
   * @return <code>Aggregating</code> of type <code>JCOL[E]</code>
   */
  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[e] <: java.util.Collection[e], R](equality: Equality[E]): AggregatingConstraint[JCOL[E], R] = 
    aggregatingNatureOfJavaCollection(new BasicEqualityConstraint[E, R](equality))

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>java.util.Map</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.Map</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Aggregating[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v], R](implicit constraint: EqualityConstraint[java.util.Map.Entry[K, V], R]): AggregatingConstraint[JMAP[K, V], R] = 
    new AggregatingConstraint[JMAP[K, V], R] {
    
      import scala.collection.JavaConverters._
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: JMAP[K, V], elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs(map.entrySet.asScala, elements, constraint)
      }
      def containsOnly(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        checkOnly(map.entrySet.asScala, elements, constraint)
      }
      def containsAllOf(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf(map.entrySet.asScala, elements, constraint)
      }
      def containsAtMostOneOf(map: JMAP[K, V], elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf(map.entrySet.asScala, elements, constraint)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
   * into <code>Aggregating</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaMap = new java.util.HashMap[Int, String]()
   * javaMap.put(1, "one")
   * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
   * (javaMap should contain (Entry(1, "ONE"))) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[java.util.HashMap[Int, String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
   * @tparam K the type of the key in the <code>java.util.Map</code>
   * @tparam V the type of the value in the <code>java.util.Map</code>
   * @tparam JMAP any subtype of <code>java.util.Map</code>
   * @return <code>Aggregating</code> of type <code>JMAP[K, V]</code>
   */
  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v], R](equality: Equality[java.util.Map.Entry[K, V]]): AggregatingConstraint[JMAP[K, V], R] = 
    aggregatingNatureOfJavaMap(new BasicEqualityConstraint[java.util.Map.Entry[K, V], R](equality))

  /**
   * Implicit to support <code>Aggregating</code> nature of <code>Every</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
   * @tparam E the type of the element in the <code>Every</code>
   * @return <code>Aggregating[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
   */
  implicit def aggregatingNatureOfEvery[E, R](implicit constraint: EqualityConstraint[E, R]): AggregatingConstraint[Every[E], R] =
    new AggregatingConstraint[Every[E], R] {
      def containsAtLeastOneOf(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        every.exists((e: E) => elements.exists((ele: R) => constraint.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(every: Every[E], elements: GenTraversable[R]): Boolean = {
        checkTheSameElementsAs[E, R](every, elements, constraint)
      }
      def containsOnly(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        checkOnly(every, elements, constraint)
      }
      def containsAllOf(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAllOf(every, elements, constraint)
      }
      def containsAtMostOneOf(every: Every[E], elements: scala.collection.Seq[R]): Boolean = {
        checkAtMostOneOf(every, elements, constraint)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Aggregating</code> of type <code>Every[E]</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Every("hi") should contain ("HI")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[Every[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>Every</code>
   * @return <code>Aggregating</code> of type <code>Every[E]</code>
   */
  implicit def convertEqualityToEveryAggregating[E, R](equality: Equality[E]): AggregatingConstraint[Every[E], R] =
    aggregatingNatureOfEvery(new BasicEqualityConstraint[E, R](equality))
}
