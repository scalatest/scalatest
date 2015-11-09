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

import org.scalactic.{Equality, Every}
import org.scalatest.words.ArrayWrapper
import scala.collection.GenTraversable
import scala.annotation.tailrec
import scala.collection.JavaConverters._

/**
 * Typeclass that enables for sequencing certain <code>contain</code> syntax in the ScalaTest matchers DSL.
 *
 * <p>
 * An <code>Sequencing[A]</code> provides access to the "sequenching nature" of type <code>A</code> in such
 * a way that relevant <code>contain</code> matcher syntax can be used with type <code>A</code>. An <code>A</code>
 * can be any type of <em>sequencing</em>&#8212;an object that in some way brings together other objects in order.
 * ScalaTest provides implicit implementations for several types out of the box in the
 * <a href="Sequencing$.html"><code>Sequencing</code> companion object</a>:
 * </p>
 *
 * <ul>
 * <li><code>scala.collection.GenSeq</code></li>
 * <li><code>scala.collection.SortedSet</code></li>
 * <li><code>scala.collection.SortedMap</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.List</code></li>
 * <li><code>java.util.SortedSet</code></li>
 * <li><code>java.util.SortedMap</code></li>
 * <li><code>String</code></li>
 * </ul>
 *
 * <p>
 * The <code>contain</code> syntax enabled by this trait is:
 * <p>
 *
 * <ul>
 * <li><code>result should contain inOrder (1, 2, 3)</code></li>
 * <li><code>result should contain inOrderOnly (1, 2, 3)</code></li>
 * <li><code>result should contain theSameElementsInOrderAs List(1, 2, 3)</code></li>
 * </ul>
 *
 * <p>
 * You can enable the <code>contain</code> matcher syntax enabled by <code>Sequencing</code> on your own
 * type <code>U</code> by defining an <code>Sequencing[U]</code> for the type and making it available implicitly.
 * </p>
 */
trait Sequencing[-S] {

  /**
   * Implements <code>contain</code> <code>inOrder</code> syntax for sequences of type <code>S</code>.
   *
   * @param sequence an sequence about which an assertion is being made
   * @param eles elements all of which should be contained, in order of appearance in <code>eles</code>, in the passed sequence
   * @return true if the passed sequence contains all of the passed elements in (iteration) order
   */
  def containsInOrder(sequence: S, eles: Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>inOrderOnly</code> syntax for sequences of type <code>S</code>.
   *
   * @param sequence an sequence about which an assertion is being made
   * @param eles the only elements that should be contained, in order of appearence in <code>eles</code>, in the passed sequence
   * @return true if the passed sequence contains only the passed elements in (iteration) order
   */
  def containsInOrderOnly(sequence: S, eles: Seq[Any]): Boolean

  /**
   * Implements <code>contain</code> <code>theSameElementsInOrderAs</code> syntax for sequences of type <code>S</code>.
   *
   * @param leftSequence an sequence about which an assertion is being made
   * @param rightSequence an sequence that should contain the same elements, in (iterated) order as the passed <code>leftSequence</code>
   * @return true if the passed <code>leftSequence</code> contains the same elements, in (iterated) order, as the passed <code>rightSequence</code>
   */
  def containsTheSameElementsInOrderAs(leftSequence: S, rightSequence: GenTraversable[Any]): Boolean
}

/**
 * Companion object for <code>Sequencing</code> that provides implicit implementations for the following types:
 *
 * <ul>
 * <li><code>scala.collection.GenSeq</code></li>
 * <li><code>scala.collection.SortedSet</code></li>
 * <li><code>scala.collection.SortedMap</code></li>
 * <li><code>Array</code></li>
 * <li><code>java.util.List</code></li>
 * <li><code>java.util.SortedSet</code></li>
 * <li><code>java.util.SortedMap</code></li>
 * <li><code>String</code></li>
 * </ul>
 */
object Sequencing {
  
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
  
  private def checkInOrder[T](left: GenTraversable[T], right: GenTraversable[Any], equality: Equality[T]): Boolean = {
    @tailrec
    def lastIndexOf(itr: Iterator[T], element: Any, idx: Option[Int], i: Int): Option[Int] = {
      if (itr.hasNext) {
        val next = itr.next
        if (equality.areEqual(next, element))
          lastIndexOf(itr, element, Some(i), i + 1)
        else
          lastIndexOf(itr, element, idx, i + 1)
      }
      else
        idx
    }
  
    @tailrec
    def checkEqual(left: GenTraversable[T], rightItr: Iterator[Any]): Boolean = {
      if (rightItr.hasNext) {
        val nextRight = rightItr.next
        lastIndexOf(left.toIterator, nextRight, None, 0) match {
          case Some(idx) => 
            checkEqual(left.drop(idx).tail, rightItr)
          case None => 
            false // Element not found, let's fail early
        }
      }
      else // No more element in right, left contains all of right.
        true
    }
    checkEqual(left, right.toIterator)
  }

  import scala.language.higherKinds

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>scala.collection.GenSeq</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.GenSeq</code>
   * @tparam E the type of the element in the <code>scala.collection.GenSeq</code>
   * @tparam SEQ any subtype of <code>scala.collection.GenSeq</code>
   * @return <code>Sequencing[SEQ[E]]</code> that supports <code>scala.collection.GenSeq</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfGenSeq[E, SEQ[e] <: scala.collection.GenSeq[e]](implicit equality: Equality[E]): Sequencing[SEQ[E]] =
    new Sequencing[SEQ[E]] {

      def containsInOrder(seq: SEQ[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(seq, elements, equality)
      }

      def containsInOrderOnly(seq: SEQ[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](seq, elements, equality)
      }

// TODO: Make elements a Sequencing
      def containsTheSameElementsInOrderAs(seq: SEQ[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](seq, elements, equality)
      }
    }

  import scala.language.implicitConversions

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>SEQ[E]</code>, where <code>SEQ</code> is a subtype of <code>scala.collection.GenSeq</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (List("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[List[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>scala.collection.GenSeq</code>
   * @tparam SEQ subtype of <code>scala.collection.GenSeq</code>
   * @return <code>Sequencing</code> of type <code>SEQ[E]</code>
   */
  implicit def convertEqualityToGenSeqSequencing[E, SEQ[e] <: scala.collection.GenSeq[e]](equality: Equality[E]): Sequencing[SEQ[E]] = 
    sequencingNatureOfGenSeq(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>scala.collection.SortedSet</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.SortedSet</code>
   * @tparam E the type of the element in the <code>scala.collection.SortedSet</code>
   * @tparam SET any subtype of <code>scala.collection.SortedSet</code>
   * @return <code>Sequencing[SET[E]]</code> that supports <code>scala.collection.SortedSet</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfSortedSet[E, SET[e] <: scala.collection.SortedSet[e]](implicit equality: Equality[E]): Sequencing[SET[E]] =
    new Sequencing[SET[E]] {

      def containsInOrder(set: SET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(set, elements, equality)
      }

      def containsInOrderOnly(set: SET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](set, elements, equality)
      }

      def containsTheSameElementsInOrderAs(set: SET[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](set, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>SET[E]</code>, where <code>SET</code> is a subtype of <code>scala.collection.SortedSet</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (SortedSet("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[SortedSet[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>scala.collection.SortedSet</code>
   * @tparam SET subtype of <code>scala.collection.SortedSet</code>
   * @return <code>Sequencing</code> of type <code>SET[E]</code>
   */
  implicit def convertEqualityToSortedSetSequencing[E, SET[e] <: scala.collection.SortedSet[e]](equality: Equality[E]): Sequencing[SET[E]] = 
    sequencingNatureOfSortedSet(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>scala.collection.SortedMap</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>scala.collection.SortedMap</code>
   * @tparam K the type of the key in the <code>scala.collection.SortedMap</code>
   * @tparam V the type of the value in the <code>scala.collection.SortedMap</code>
   * @tparam MAP any subtype of <code>scala.collection.SortedMap</code>
   * @return <code>Sequencing[MAP[K, V]]</code> that supports <code>scala.collection.SortedMap</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfSortedMap[K, V, MAP[k, v] <: scala.collection.SortedMap[k, v]](implicit equality: Equality[(K, V)]): Sequencing[MAP[K, V]] =
    new Sequencing[MAP[K, V]] {

      def containsInOrder(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(map, elements, equality)
      }

      def containsInOrderOnly(map: MAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(map, elements, equality)
      }

      def containsTheSameElementsInOrderAs(map: MAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>(K, V)</code>
   * into <code>Sequencing</code> of type <code>MAP[K, V]</code>, where <code>MAP</code> is a subtype of <code>scala.collection.SortedMap</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * // lowerCased needs to be implemented as Normalization[(K, V)]
   * (SortedMap("hi" -> "hi", "he" -> "he") should contain inOrderOnly ("HI" -> "HI", "HE" -> "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[SortedMap[String, String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>(K, V)</code>
   * @tparam K the type of the key in the <code>scala.collection.SortedMap</code>
   * @tparam V the type of the value in the <code>scala.collection.SortedMap</code>
   * @tparam MAP subtype of <code>scala.collection.SortedMap</code>
   * @return <code>Sequencing</code> of type <code>MAP[K, V]</code>
   */
  implicit def convertEqualityToSortedMapSequencing[K, V, MAP[k, v] <: scala.collection.SortedMap[k, v]](equality: Equality[(K, V)]): Sequencing[MAP[K, V]] = 
    sequencingNatureOfSortedMap(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>Array</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Array</code>
   * @tparam E the type of the element in the <code>Array</code>
   * @return <code>Sequencing[Array[E]]</code> that supports <code>Array</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfArray[E](implicit equality: Equality[E]): Sequencing[Array[E]] = 
    new Sequencing[Array[E]] {

      def containsInOrder(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(new ArrayWrapper(array), elements, equality)
      }

      def containsInOrderOnly(array: Array[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(new ArrayWrapper(array), elements, equality)
      }

      def containsTheSameElementsInOrderAs(array: Array[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](new ArrayWrapper(array), elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>Array[E]</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Array("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[Array[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>Array</code>
   * @return <code>Sequencing</code> of type <code>Array[E]</code>
   */
  implicit def convertEqualityToArraySequencing[E](equality: Equality[E]): Sequencing[Array[E]] = 
    sequencingNatureOfArray(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>java.util.List</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.List</code>
   * @tparam E the type of the element in the <code>java.util.List</code>
   * @tparam JLIST any subtype of <code>java.util.List</code>
   * @return <code>Sequencing[JLIST[E]]</code> that supports <code>java.util.List</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfJavaList[E, JLIST[e] <: java.util.List[e]](implicit equality: Equality[E]): Sequencing[JLIST[E]] = 
    new Sequencing[JLIST[E]] {

      def containsInOrder(col: JLIST[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(col.asScala, elements, equality)
      }

      def containsInOrderOnly(col: JLIST[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(col.asScala, elements, equality)
      }

      def containsTheSameElementsInOrderAs(col: JLIST[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(col.asScala, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>JLIST[E]</code>, where <code>JLIST</code> is a subtype of <code>java.util.List</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaList = new java.util.ArrayList[String]()
   * javaList.add("hi", "he")
   * (javaList should contain ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[java.util.ArrayList[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>java.util.List</code>
   * @tparam JLIST subtype of <code>java.util.List</code>
   * @return <code>Sequencing</code> of type <code>JLIST[E]</code>
   */
  implicit def convertEqualityToJavaListSequencing[E, JLIST[e] <: java.util.List[e]](equality: Equality[E]): Sequencing[JLIST[E]] = 
    sequencingNatureOfJavaList(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>java.util.SortedSet</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>java.util.SortedSet</code>
   * @tparam E the type of the element in the <code>java.util.SortedSet</code>
   * @tparam JSET any subtype of <code>java.util.SortedSet</code>
   * @return <code>Sequencing[JSET[E]]</code> that supports <code>java.util.SortedSet</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfJavaSortedSet[E, JSET[e] <: java.util.SortedSet[e]](implicit equality: Equality[E]): Sequencing[JSET[E]] =
    new Sequencing[JSET[E]] {

      def containsInOrder(set: JSET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(set.iterator.asScala.toVector, elements, equality)
      }

      def containsInOrderOnly(set: JSET[E], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly[E](set.iterator.asScala.toVector, elements, equality)
      }

      def containsTheSameElementsInOrderAs(set: JSET[E], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs[E](set.iterator.asScala.toVector, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>JSET[E]</code>, where <code>JSET</code> is a subtype of <code>java.util.SortedSet</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaSet = new java.util.TreeSet[String]()
   * javaSet.add("hi", "he")
   * (javaSet should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[java.util.TreeSet[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>java.util.List</code>
   * @tparam JSET subtype of <code>java.util.List</code>
   * @return <code>Sequencing</code> of type <code>JLIST[E]</code>
   */
  implicit def convertEqualityToJavaSortedSetSequencing[E, JSET[e] <: java.util.SortedSet[e]](equality: Equality[E]): Sequencing[JSET[E]] = 
    sequencingNatureOfJavaSortedSet(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>java.util.SortedMap</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of entry in the <code>java.util.SortedMap</code>
   * @tparam K the type of the key in the <code>java.util.SortedMap</code>
   * @tparam V the type of the value in the <code>java.util.SortedMap</code>
   * @tparam JMAP any subtype of <code>java.util.SortedMap</code>
   * @return <code>Sequencing[JMAP[K, V]]</code> that supports <code>java.util.SortedMap</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfJavaSortedMap[K, V, JMAP[k, v] <: java.util.SortedMap[k, v]](implicit equality: Equality[java.util.Map.Entry[K, V]]): Sequencing[JMAP[K, V]] =
    new Sequencing[JMAP[K, V]] {

      def containsInOrder(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(map.entrySet.iterator.asScala.toVector, elements, equality)
      }

      def containsInOrderOnly(map: JMAP[K, V], elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(map.entrySet.iterator.asScala.toVector, elements, equality)
      }

      def containsTheSameElementsInOrderAs(map: JMAP[K, V], elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(map.entrySet.iterator.asScala.toVector, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
   * into <code>Sequencing</code> of type <code>JMAP[K, V]</code>, where <code>JMAP</code> is a subtype of <code>java.util.SortedMap</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * val javaMap = new java.util.TreeMap[Int, String]()
   * javaMap.put(1, "one")
   * // lowerCased needs to be implemented as Normalization[java.util.Map.Entry[K, V]]
   * (javaMap should contain inOrderOnly (Entry(1, "ONE"))) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>java.util.Map.Entry[Int, String]</code></a>
   * and this implicit conversion will convert it into <code>Aggregating[java.util.TreeMap[Int, String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>java.util.Map.Entry[K, V]</code>
   * @tparam K the type of the key in the <code>java.util.SortedMap</code>
   * @tparam V the type of the value in the <code>java.util.SortedMap</code>
   * @tparam JMAP subtype of <code>java.util.SortedMap</code>
   * @return <code>Sequencing</code> of type <code>JMAP[K, V]</code>
   */
  implicit def convertEqualityToJavaSortedMapSequencing[K, V, JMAP[k, v] <: java.util.SortedMap[k, v]](equality: Equality[java.util.Map.Entry[K, V]]): Sequencing[JMAP[K, V]] = 
    sequencingNatureOfJavaSortedMap(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>String</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of <code>Char</code> in the <code>String</code>
   * @return <code>Sequencing[String]</code> that supports <code>String</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfString(implicit equality: Equality[Char]): Sequencing[String] = 
    new Sequencing[String] {

      def containsInOrder(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrder(s, elements, equality)
      }

      def containsInOrderOnly(s: String, elements: scala.collection.Seq[Any]): Boolean = {
        checkInOrderOnly(s, elements, equality)
      }

      def containsTheSameElementsInOrderAs(s: String, elements: GenTraversable[Any]): Boolean = {
        checkTheSameElementsInOrderAs(s, elements, equality)
      }
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
   * into <code>Sequencing</code> of type <code>String</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * // lowerCased needs to be implemented as Normalization[Char]
   * ("hi hello" should contain inOrderOnly ('E')) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[Char]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[String]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>Char</code>
   * @return <code>Sequencing</code> of type <code>String</code>
   */
  implicit def convertEqualityToStringSequencing(equality: Equality[Char]): Sequencing[String] = 
    sequencingNatureOfString(equality)

  /**
   * Implicit to support <code>Sequencing</code> nature of <code>Every</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> type class that is used to check equality of element in the <code>Every</code>
   * @tparam E the type of the element in the <code>Every</code>
   * @return <code>Sequencing[Every[E]]</code> that supports <code>Every</code> in relevant <code>contain</code> syntax
   */
  implicit def sequencingNatureOfEvery[E](implicit equality: Equality[E]): Sequencing[Every[E]] =
    new Sequencing[Every[E]] {

      def containsInOrder(every: Every[E], elements: scala.collection.Seq[Any]): Boolean =
        checkInOrder(every, elements, equality)

      def containsInOrderOnly(every: Every[E], elements: scala.collection.Seq[Any]): Boolean =
        checkInOrderOnly(every, elements, equality)

      def containsTheSameElementsInOrderAs(every: Every[E], elements: GenTraversable[Any]): Boolean =
        checkTheSameElementsInOrderAs[E](every, elements, equality)
    }

  /**
   * Implicit conversion that converts an <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * into <code>Sequencing</code> of type <code>Every[E]</code>.
   * This is required to support the explicit <a href="../../scalactic/Equality.html"><code>Equality</code></a> syntax, for example:
   *
   * <pre class="stHighlight">
   * (Every("hi", "he") should contain inOrderOnly ("HI", "HE")) (after being lowerCased)
   * </pre>
   *
   * <code>(after being lowerCased)</code> will returns an <a href="../../scalactic/Equality.html"><code>Equality[String]</code></a>
   * and this implicit conversion will convert it into <code>Sequencing[Every[String]]</code>.
   *
   * @param equality <a href="../../scalactic/Equality.html"><code>Equality</code></a> of type <code>E</code>
   * @tparam E type of elements in the <code>Every</code>
   * @return <code>Sequencing</code> of type <code>Every[E]</code>
   */
  implicit def convertEqualityToEverySequencing[E](equality: Equality[E]): Sequencing[Every[E]] =
    sequencingNatureOfEvery(equality)
    
}
