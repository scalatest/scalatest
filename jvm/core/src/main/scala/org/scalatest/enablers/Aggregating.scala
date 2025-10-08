/*
 * Copyright 2001-2025 Artima, Inc.
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

import scala.collection.JavaConverters._
import org.scalactic.{Equality, Every}
import org.scalactic.ColCompatHelper.Iterable
import org.scalatest.FailureMessages
import org.scalatest.verbs.ArrayWrapper
import org.scalactic.ColCompatHelper.aggregate
import scala.annotation.tailrec

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
 * <li><code>org.scalactic.ColCompatHelper.Iterable</code></li>
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
trait Aggregating[-A] {

// TODO: Write tests that a NotAllowedException is thrown when no elements are passed, maybe if only one element is passed, and 
// likely if an object is repeated in the list.
  /**
   * Implements <code>contain</code> <code>atLeastOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at least one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at least one of the passed elements
   */
  def containsAtLeastOneOf(aggregation: A, eles: scala.collection.Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>theSameElementsAs</code> syntax for aggregations of type <code>A</code>.
   *
   * @param leftAggregation an aggregation about which an assertion is being made
   * @param rightAggregation an aggregation that should contain the same elements as the passed <code>leftAggregation</code>
   * @return true if the passed <code>leftAggregation</code> contains the same elements as the passed <code>rightAggregation</code>
   */
  def containsTheSameElementsAs(leftAggregation: A, rightAggregation: Iterable[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>only</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles the only elements that should be contained in the passed aggregation
   * @return true if the passed aggregation contains only the passed elements
   */
  def containsOnly(aggregation: A, eles: scala.collection.Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>allOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements all of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains all of the passed elements
   */
  def containsAllOf(aggregation: A, eles: scala.collection.Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>atMostOneOf</code> syntax for aggregations of type <code>A</code>.
   *
   * @param aggregation an aggregation about which an assertion is being made
   * @param eles elements at most one of which should be contained in the passed aggregation
   * @return true if the passed aggregation contains at most one of the passed elements
   */
  def containsAtMostOneOf(aggregation: A, eles: scala.collection.Seq[Any]): Boolean
}

trait AggregatingImpls {
  // TODO: Throwing exceptions is slow. Just do a pattern match and test the type before trying to cast it.
  private[scalatest] def tryEquality[T](left: Any, right: Any, equality: Equality[T]): Boolean =
    try equality.areEqual(left.asInstanceOf[T], right)
    catch {
      case cce: ClassCastException => false
    }

  private[scalatest] def checkTheSameElementsAs[T](left: Iterable[T], right: Iterable[Any], equality: Equality[T]): Boolean = {
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

    val counts = aggregate(right.toIterable.zipAll(left.toIterable, ZipNoMatch, ZipNoMatch), IndexedSeq.empty[ElementCount])(
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

  private[scalatest] def checkOnly[T](left: Iterable[T], right: Iterable[Any], equality: Equality[T]): Boolean =
    left.forall(l => right.find(r => tryEquality(l, r, equality)).isDefined) &&
      right.forall(r => left.find(l => tryEquality(l, r, equality)).isDefined)

  private[scalatest] def checkAllOf[T](left: Iterable[T], right: Iterable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def checkEqual(left: Iterable[T], rightItr: Iterator[Any]): Boolean = {
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

  private[scalatest] def checkAtMostOneOf[T](left: Iterable[T], right: Iterable[Any], equality: Equality[T]): Boolean = {

    def countElements: Int =
      aggregate(right, 0)(
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
}

trait AggregatingJavaImplicits extends AggregatingImpls {

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>java.util.Collection</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY   * To support <code>Aggregating</code> nature of <code>java.util.Collection</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
    * @tparam E the type of the element in the <code>java.util.Collection</code>
    * @tparam JCOL any subtype of <code>java.util.Collection</code>
    * @return <code>Aggregating[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def aggregatingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]](implicit equality: Equality[E]): Aggregating[JCOL[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfJavaCollection[E, JCOL[e] <: java.util.Collection[e]](using equality: Equality[E]): Aggregating[JCOL[E]] =
    convertEqualityToJavaCollectionAggregating(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
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
  // SKIP-DOTTY-START
  implicit def convertEqualityToJavaCollectionAggregating[E, JCOL[e] <: java.util.Collection[e]](equality: Equality[E]): Aggregating[JCOL[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToJavaCollectionAggregating[E, JCOL[e] <: java.util.Collection[e]](equality: Equality[E]): Aggregating[JCOL[E]] =
    new Aggregating[JCOL[E]] {
      def containsAtLeastOneOf(col: JCOL[E], elements: scala.collection.Seq[Any]): Boolean = {
        col.asScala.exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(col: JCOL[E], elements: Iterable[Any]): Boolean = {
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

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>java.util.Collection</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam JCOL any subtype of <code>java.util.Collection</code>
  //DOTTY-ONLY   * @return <code>Aggregating[JCOL[E]]</code> that supports <code>java.util.Collection</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E, JCOL[e] <: java.util.Collection[e]] (using equality: Equality[E]): Aggregating[JCOL[E]] = convertEqualityToJavaCollectionAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>JCOL[E]</code>, where <code>JCOL</code> is a subtype of <code>java.util.Collection</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val javaList = new java.util.ArrayList[String]()
  //DOTTY-ONLY   * javaList.add("hi")
  //DOTTY-ONLY   * (javaList should contain ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[java.util.ArrayList[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>java.util.Collection</code>
  //DOTTY-ONLY   * @tparam JCOL subtype of <code>java.util.Collection</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>JCOL[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityJavaCollectionAggregating[E, JCOL[e] <: java.util.Collection[e]]: Conversion[Equality[E], Aggregating[JCOL[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Aggregating[JCOL[E]] = convertEqualityToJavaCollectionAggregating(equality)
  //DOTTY-ONLY }  

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>java.util.Map</code>.
    // SKIP-DOTTY-END
    //DOTTY-ONLY * To support <code>Aggregating</code> nature of <code>java.util.Map</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.Map</code>
    * @tparam K the type of the key in the <code>java.util.Map</code>
    * @tparam V the type of the value in the <code>java.util.Map</code>
    * @tparam JMAP any subtype of <code>java.util.Map</code>
    * @return <code>Aggregating[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START
  implicit def aggregatingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](implicit equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfJavaMap[K, V, JMAP[k, v] <: java.util.Map[k, v]](using equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] =
    convertEqualityToJavaMapAggregating(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
    // SKIP-DOTTY-END
    //DOTTY-ONLY * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
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
  // SKIP-DOTTY-START
  implicit def convertEqualityToJavaMapAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToJavaMapAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] =
    new Aggregating[JMAP[K, V]] {

      import scala.collection.JavaConverters._
      def containsAtLeastOneOf(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.entrySet.asScala.exists((e: java.util.Map.Entry[K, V]) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: JMAP[K, V], elements: Iterable[Any]): Boolean = {
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

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>java.util.Map</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY   * @return <code>Aggregating[JMAP[K, V]]</code> that supports <code>java.util.Map</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, JMAP[k, v] <: java.util.Map[k, v]] (using equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = convertEqualityToJavaMapAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.Map</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val javaMap = new java.util.HashMap[Int, String]()
  //DOTTY-ONLY   * javaMap.put(1, "one")
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
  //DOTTY-ONLY   * (javaMap should contain (Entry(1, "ONE"))) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[java.util.HashMap[Int, String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>java.util.Map</code>
  //DOTTY-ONLY   * @tparam JMAP any subtype of <code>java.util.Map</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>JMAP[K, V]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityJavaMapAggregating[K, V, JMAP[k, v] <: java.util.Map[k, v]]: Conversion[Equality[java.util.Map.Entry[K, V]], Aggregating[JMAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[java.util.Map.Entry[K, V]]): Aggregating[JMAP[K, V]] = convertEqualityToJavaMapAggregating(equality)
  //DOTTY-ONLY }  

}

trait AggregatingStandardImplicits extends AggregatingJavaImplicits {

  import scala.language.higherKinds

  // SKIP-DOTTY-START
  import scala.language.implicitConversions
  // SKIP-DOTTY-END

  /**
  // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>Array</code>.
  // SKIP-DOTTY-END
  //DOTTY-ONLY * <code>Aggregating</code> nature of <code>Array</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
    * @tparam E the type of the element in the <code>Array</code>
    * @return <code>Aggregating[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def aggregatingNatureOfArray[E](implicit equality: Equality[E]): Aggregating[Array[E]] = 
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfArray[E](using equality: Equality[E]): Aggregating[Array[E]] = 
    convertEqualityToArrayAggregating(equality)

  /**
  // SKIP-DOTTY-START 
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
  // SKIP-DOTTY-END   
  //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Array[E]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Array</code>
    * @return <code>Aggregating</code> of type <code>Array[E]</code>
    */
  // SKIP-DOTTY-START   
  implicit def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToArrayAggregating[E](equality: Equality[E]): Aggregating[Array[E]] =
    new Aggregating[Array[E]] {
      def containsAtLeastOneOf(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        new ArrayWrapper(array).exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(array: Array[E], elements: Iterable[Any]): Boolean = {
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

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>Array</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>Array</code>
  //DOTTY-ONLY   * @return <code>Aggregating[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E](using equality: Equality[E]): Aggregating[Array[E]] = convertEqualityToArrayAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Array[E]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (Array("hi") should contain ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[Array[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Array</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>Array[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityToArrayAggregating[E]: Conversion[Equality[E], Aggregating[Array[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Aggregating[Array[E]] = convertEqualityToArrayAggregating(equality)
  //DOTTY-ONLY }  

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>String</code>.
    // SKIP-DOTTY-END
  //DOTTY-ONLY * <code>Aggregating</code> nature of <code>String</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
    * @return <code>Aggregating[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def aggregatingNatureOfString(implicit equality: Equality[Char]): Aggregating[String] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfString(implicit equality: Equality[Char]): Aggregating[String] =
    convertEqualityToStringAggregating(equality)

  /**
    // SKIP-DOTTY-START
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
    // SKIP-DOTTY-END   
    //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>String</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
    * @return <code>Aggregating</code> of type <code>String</code>
    */
  // SKIP-DOTTY-START  
  implicit def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToStringAggregating(equality: Equality[Char]): Aggregating[String] =
    new Aggregating[String] {
      def containsAtLeastOneOf(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        s.exists((e: Char) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(s: String, elements: Iterable[Any]): Boolean = {
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

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>String</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>String</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>String</code>
  //DOTTY-ONLY   * @return <code>Aggregating[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given (using equality: Equality[Char]): Aggregating[String] = convertEqualityToStringAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>String</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[Char]
  //DOTTY-ONLY   * ("hi hello" should contain ('E')) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[String]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>String</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityToStringAggregating: Conversion[Equality[Char], Aggregating[String]] with {
  //DOTTY-ONLY   def apply(equality: Equality[Char]): Aggregating[String] = convertEqualityToStringAggregating(equality)
  //DOTTY-ONLY }  

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>Every</code>.
    // SKIP-DOTTY-END
  //DOTTY-ONLY * <code>Aggregating</code> nature of <code>Every</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
    * @tparam E the type of the element in the <code>Every</code>
    * @return <code>Aggregating[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def aggregatingNatureOfEvery[E](implicit equality: Equality[E]): Aggregating[Every[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfEvery[E](using equality: Equality[E]): Aggregating[Every[E]] =
    convertEqualityToEveryAggregating(equality)

  /**
    // SKIP-DOTTY-START
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
    // SKIP-DOTTY-END   
    //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Every[E]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Every</code>
    * @return <code>Aggregating</code> of type <code>Every[E]</code>
    */
  // SKIP-DOTTY-START  
  implicit def convertEqualityToEveryAggregating[E](equality: Equality[E]): Aggregating[Every[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToEveryAggregating[E](equality: Equality[E]): Aggregating[Every[E]] =
    new Aggregating[Every[E]] {
      def containsAtLeastOneOf(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        every.exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(every: Every[E], elements: Iterable[Any]): Boolean = {
        checkTheSameElementsAs[E](every, elements, equality)
      }
      def containsOnly(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly(every, elements, equality)
      }
      def containsAllOf(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(every, elements, equality)
      }
      def containsAtMostOneOf(every: Every[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(every, elements, equality)
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>Every</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>Every</code>
  //DOTTY-ONLY   * @return <code>Aggregating[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E](using equality: Equality[E]): Aggregating[Every[E]] = convertEqualityToEveryAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Every[E]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (Every("hi") should contain ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[Every[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Every</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>Every[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityToEveryAggregating[E]: Conversion[Equality[E], Aggregating[Every[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Aggregating[Every[E]] = convertEqualityToEveryAggregating(equality)
  //DOTTY-ONLY }

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>Iterable</code>.
    // SKIP-DOTTY-END
  //DOTTY-ONLY * <code>Aggregating</code> nature of <code>Iterable</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Iterable</code>
    * @tparam E the type of the element in the <code>Iterable</code>
    * @tparam ITR any subtype of <code>Iterable</code>
    * @return <code>Aggregating[ITR[E]]</code> that supports <code>Iterable</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def aggregatingNatureOfIterable[E, ITR[e] <: Iterable[e]](implicit equality: Equality[E]): Aggregating[ITR[E]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfIterable[E, ITR[e] <: Iterable[e]](using equality: Equality[E]): Aggregating[ITR[E]] =
    convertEqualityToIterableAggregating(equality)  

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * into <code>Aggregating</code> of type <code>TRAV[E]</code>, where <code>TRAV</code> is a subtype of <code>Iterable</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * (List("hi") should contain ("HI")) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
    * and this implicit conversion will convert it into <code>Aggregating[List[String]]</code>.
    // SKIP-DOTTY-END   
    //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Iterable[E]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
    * @tparam E type of elements in the <code>Iterable</code>
    * @tparam ITR subtype of <code>Iterable</code>
    * @return <code>Aggregating</code> of type <code>ITR[E]</code>
    */
  implicit def convertEqualityToIterableAggregating[E, ITR[e] <: Iterable[e]](equality: Equality[E]): Aggregating[ITR[E]] =
    new Aggregating[ITR[E]] {
      def containsAtLeastOneOf(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        itr.exists((e: E) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(itr: ITR[E], elements: Iterable[Any]): Boolean = {
        checkTheSameElementsAs[E](itr, elements, equality)
      }
      def containsOnly(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkOnly[E](itr, elements, equality)
      }
      def containsAllOf(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAllOf(itr, elements, equality)
      }
      def containsAtMostOneOf(itr: ITR[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkAtMostOneOf(itr, elements, equality)
      }
    }

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>Iterable</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Iterable</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>Iterable</code>
  //DOTTY-ONLY   * @return <code>Aggregating[Iterable[E]]</code> that supports <code>Iterable</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [E](using equality: Equality[E]): Aggregating[Iterable[E]] = convertEqualityToIterableAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>Iterable[E]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * (List("hi") should contain ("HI")) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[List[String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
  //DOTTY-ONLY   * @tparam E type of elements in the <code>Iterable/code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>Iterable[E]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityToIterableAggregating[E]: Conversion[Equality[E], Aggregating[Iterable[E]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[E]): Aggregating[Iterable[E]] = convertEqualityToIterableAggregating(equality)
  //DOTTY-ONLY }

}

trait AggregatingHighPriorityImplicits extends AggregatingStandardImplicits {

  /**
    // SKIP-DOTTY-START
    * Implicit to support <code>Aggregating</code> nature of <code>scala.collection.GenMap</code>.
    // SKIP-DOTTY-END
  //DOTTY-ONLY * <code>Aggregating</code> nature of <code>scala.collection.GenMap</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>scala.collection.GenMap</code>
    * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
    * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
    * @return <code>Aggregating[MAP[K, V]]</code> that supports <code>scala.collection.GenMap</code> in relevant <code>contain</code> syntax
    */
  // SKIP-DOTTY-START  
  implicit def aggregatingNatureOfMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](implicit equality: Equality[(K, V)]): Aggregating[MAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def aggregatingNatureOfMap[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](using equality: Equality[(K, V)]): Aggregating[MAP[K, V]] =
    convertEqualityToMapAggregating(equality)

  /**
    // SKIP-DOTTY-START
    * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    * into <code>Aggregating</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.GenMap</code>.
    * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
    *
    * <pre class="stHighlight">
    * val map = Map(1 -> "one")
    * // lowerCased needs to be implemented as Normalization[Tuple2[K, V]]
    * (map should contain ((1, "ONE"))) (after being lowerCased)
    * </pre>
    *
    * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Tuple2[Int, String]</code></a>
    * and this implicit conversion will convert it into <code>Aggregating[scala.collection.GenMap[Int, String]]</code>.
    // SKIP-DOTTY-END   
    //DOTTY-ONLY   * Converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>scala.collection.GenMap[K, V]</code>.
    *
    * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
    * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
    * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
    * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
    * @return <code>Aggregating</code> of type <code>MAP[K, V]</code>
    */
  // SKIP-DOTTY-START  
  implicit def convertEqualityToMapAggregating[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] =
  // SKIP-DOTTY-END
  //DOTTY-ONLY def convertEqualityToMapAggregating[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](equality: Equality[(K, V)]): Aggregating[MAP[K, V]] =
    new Aggregating[MAP[K, V]] {
      def containsAtLeastOneOf(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        map.exists((e: (K, V)) => elements.exists((ele: Any) => equality.areEqual(e, ele)))
      }
      def containsTheSameElementsAs(map: MAP[K, V], elements: Iterable[Any]): Boolean = {
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

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Given support <code>Aggregating</code> nature of <code>scala.collection.GenMap</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam E the type of the element in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @return <code>Aggregating[scala.collection.GenMap[E]]</code> that supports <code>scala.collection.GenMap</code> in relevant <code>contain</code> syntax
  //DOTTY-ONLY   */
  //DOTTY-ONLY given [K, V, MAP[k, v] <: scala.collection.GenMap[k, v]](using equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = convertEqualityToMapAggregating(equality)

  //DOTTY-ONLY /**
  //DOTTY-ONLY   * Conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
  //DOTTY-ONLY   * into <code>Aggregating</code> of type <code>MAP[K, V]</code>.
  //DOTTY-ONLY   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <pre class="stHighlight">
  //DOTTY-ONLY   * val map = Map(1 -> "one")
  //DOTTY-ONLY   * // lowerCased needs to be implemented as Normalization[Tuple2[K, V]]
  //DOTTY-ONLY   * (map should contain ((1, "ONE"))) (after being lowerCased)
  //DOTTY-ONLY   * </pre>
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Tuple2[Int, String]</code></a>
  //DOTTY-ONLY   * and this implicit conversion will convert it into <code>Aggregating[scala.collection.GenMap[Int, String]]</code>.
  //DOTTY-ONLY   *
  //DOTTY-ONLY   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Tuple2[K, V]</code>
  //DOTTY-ONLY   * @tparam K the type of the key in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam V the type of the value in the <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @tparam MAP any subtype of <code>scala.collection.GenMap</code>
  //DOTTY-ONLY   * @return <code>Aggregating</code> of type <code>MAP[K, V]</code>
  //DOTTY-ONLY   */
  //DOTTY-ONLY given equalityToMapAggregating[K, V, MAP[k, v] <: scala.collection.GenMap[k, v]]: Conversion[Equality[(K, V)], Aggregating[MAP[K, V]]] with {
  //DOTTY-ONLY   def apply(equality: Equality[(K, V)]): Aggregating[MAP[K, V]] = convertEqualityToMapAggregating(equality)
  //DOTTY-ONLY }  

}

/**
 * Companion object for <code>Aggregating</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>org.scalactic.ColCompatHelper.Iterable</code></li>
 * <li><code>String</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.Collection</code></li>
 * <li><code>java.util.Map</code></li>
 * </ul>
 */
object Aggregating extends AggregatingHighPriorityImplicits
