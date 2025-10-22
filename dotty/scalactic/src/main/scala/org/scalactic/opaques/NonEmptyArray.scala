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
package org.scalactic.opaques

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.GenSeq
import org.scalactic.ColCompatHelper.{Iterable, IterableOnce, GenIterable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.ArrayOps
import org.scalactic.{Every, Resources}
import scala.language.higherKinds

// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
/**
  * A non-empty array: an ordered, mutable, non-empty collection of elements with <code>IndexedSeq</code> performance characteristics.
  *
  * <p>
  * The purpose of <code>NonEmptyArray</code> is to allow you to express in a type that an <code>Array</code> is non-empty, thereby eliminating the
  * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty immutable sequence with <code>IndexedSeq</code>
  * performance, see <a href="Every.html"><code>Every</code></a>.
  * </p>
  *
  * <h2>Constructing <code>NonEmptyArray</code>s</h2>
  *
  * <p>
  * You can construct a <code>NonEmptyArray</code> by passing one or more elements to the <code>NonEmptyArray.apply</code> factory method:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; NonEmptyArray(1, 2, 3)
  * res0: org.scalactic.opaques.NonEmptyArray[Int] = NonEmptyArray(1, 2, 3)
  * </pre>
  *
  * <h2>Working with <code>NonEmptyArray</code>s</h2>
  *
  * <p>
  * <code>NonEmptyArray</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
  * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
  * you'll get an empty <code>Seq</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Array(1).tail
  * res6: Array[Int] = Array()
  * </pre>
  *
  * <p>
  * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
  * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptyArray</code> defines a method corresponding to every such <code>Seq</code>
  * method. Here are some examples:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyArray(1, 2, 3).map(_ + 1)                        // Result: NonEmptyArray(2, 3, 4)
  * NonEmptyArray(1).map(_ + 1)                              // Result: NonEmptyArray(2)
  * NonEmptyArray(1, 2, 3).containsSlice(NonEmptyArray(2, 3)) // Result: true
  * NonEmptyArray(1, 2, 3).containsSlice(NonEmptyArray(3, 4)) // Result: false
  * NonEmptyArray(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
  * </pre>
  *
  * <p>
  * <code>NonEmptyArray</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
  * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptyArray</code> to <code>Array</code>
  * is defined in the <code>NonEmptyArray</code> companion object that will be applied if you attempt to call one of the missing methods. As a
  * result, you can invoke <code>filter</code> on an <code>NonEmptyArray</code>, even though <code>filter</code> could result
  * in an empty sequence&mdash;but the result type will be <code>Array</code> instead of <code>NonEmptyArray</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyArray(1, 2, 3).filter(_ &lt; 10) // Result: Array(1, 2, 3)
  * NonEmptyArray(1, 2, 3).filter(_ &gt; 10) // Result: Array()
  * </pre>
  *
  *
  * <p>
  * You can use <code>NonEmptyArray</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyArray</code> unless
  * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
  * result type will switch to a <code>Array</code> at that point. Here are some examples:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; import org.scalactic.opaques._
  * import org.scalactic.opaques._
  *
  * scala&gt; for (i &lt;- NonEmptyArray(1, 2, 3)) yield i + 1
  * res0: org.scalactic.opaques.NonEmptyArray[Int] = NonEmptyArray(2, 3, 4)
  *
  * scala&gt; for (i &lt;- NonEmptyArray(1, 2, 3) if i &lt; 10) yield i + 1
  * res1: Array[Int] = Array(2, 3, 4)
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyArray(1, 2, 3)
  *      |   j &lt;- NonEmptyArray('a', 'b', 'c')
  *      | } yield (i, j)
  * res3: org.scalactic.opaques.NonEmptyArray[(Int, Char)] =
  *         NonEmptyArray((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyArray(1, 2, 3) if i &lt; 10
  *      |   j &lt;- NonEmptyArray('a', 'b', 'c')
  *      | } yield (i, j)
  * res6: Array[(Int, Char)] =
  *         Array((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  * </pre>
  *
  * @tparam T the type of elements contained in this <code>NonEmptyArray</code>
  */

opaque type NonEmptyArray[T] = Array[T] & { def length: Int & (1 | Int) }

/**
  * Companion object for class <code>NonEmptyArray</code>.
  */
object NonEmptyArray {

  /**
    * Constructs a new <code>NonEmptyArray</code> given at least one element.
    *
    * @tparam T the type of the element contained in the new <code>NonEmptyArray</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptyArray</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptyArray</code>
    */
  def apply[T](firstElement: T, otherElements: T*)(implicit classTag: ClassTag[T]): NonEmptyArray[T] = (firstElement +: otherElements).toArray

  /**
    * Variable argument extractor for <code>NonEmptyArray</code>s.
    *
    * @param nonEmptyArray: the <code>NonEmptyArray</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptyArray</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[T](nonEmptyArray: NonEmptyArray[T]): Option[Seq[T]] = Some(nonEmptyArray)

  //def unapply[A](nea: NonEmptyArray[A]): Some[Array[A]] = Some(nea)

  /*
    // TODO: Figure out how to get case NonEmptyArray() to not compile
    def unapplySeq[T](nonEmptyArray: NonEmptyArray[T]): Option[(T, Seq[T])] = Some(nonEmptyArray.head, nonEmptyArray.tail)
  */

  /**
    * Optionally construct a <code>NonEmptyArray</code> containing the elements, if any, of a given <code>GenSeq</code>.
    *
    * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyArray</code>
    * @return a <code>NonEmptyArray</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
    *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
    */
  def from[T](seq: GenSeq[T])(implicit classTag: ClassTag[T]): Option[NonEmptyArray[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some((first +: seq.tail).toArray)
    }

  import scala.language.implicitConversions
  /**
    * Given conversion from <code>NonEmptyArray</code> to <code>GenSeq</code>.
    *
    * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
    * @return the <code>GenSeq</code>
    */
  implicit def nonEmptyArrayToGenSeq[E](nonEmptyArray: NonEmptyArray[E]): scala.collection.GenSeq[E] = Vector(nonEmptyArray*)

  /**
    * Given conversion from <code>NonEmptyArray</code> to <code>PartialFunction</code>.
    *
    * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
    * @return the <code>PartialFunction</code> 
    */
  given [E]: Conversion[NonEmptyArray[E], PartialFunction[Int, E]] with {
    def apply(nonEmptyArray: NonEmptyArray[E]): PartialFunction[Int, E] =
      new PartialFunction[Int, E] {
        def apply(idx: Int): E = nonEmptyArray(idx)
        def isDefinedAt(idx: Int): Boolean = idx >= 0 && idx < nonEmptyArray.length
      }
  }

  extension [T](element: T) {
    /**
      * Returns a new <code>NonEmptyArray</code> with the given element prepended.
      *
      * <p>
      * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to prepend to this <code>NonEmptyArray</code>
      * @return a new <code>NonEmptyArray</code> consisting of <code>element</code> followed by all elements of this <code>NonEmptyArray</code>.
      */
    infix def +:[U >: T](array: NonEmptyArray[U])(using ClassTag[U]): NonEmptyArray[U] = {
      ArrayOps(array).prepended(element)
    }
  }

  extension [T] (nonEmptyArray: NonEmptyArray[T]) {
    /**
      * Returns a new <code>NonEmptyArray</code> containing the elements of this <code>NonEmptyArray</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyArray</code> is the most specific superclass encompassing the element types of this <code>NonEmptyArray</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyArray</code> that contains all the elements of this <code>NonEmptyArray</code> followed by all elements of <code>other</code>.
      */
    infix final def ++[U >: T](other: IterableOnce[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
      ArrayOps(nonEmptyArray).appendedAll(other)
    }

    /**
      * Returns a new <code>NonEmptyArray</code> with the given element appended.
      *
      * <p>
      * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to append to this <code>NonEmptyArray</code>
      * @return a new <code>NonEmptyArray</code> consisting of all elements of this <code>NonEmptyArray</code> followed by <code>element</code>.
      */
    def :+[U >: T](element: U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] = { 
      ArrayOps(nonEmptyArray).appended(element)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given value as an element.
      *
      * @param elem the element to look for
      * @return true if this <code>NonEmptyArray</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
      */
    final def contains(elem: T): Boolean = {
      val arrOps = new ArrayOps(nonEmptyArray)
      arrOps.contains(elem)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given <code>GenSeq</code> as a slice.
      *
      * @param that the <code>GenSeq</code> slice to look for
      * @return true if this <code>NonEmptyArray</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
      */
    final def containsSlice[B](that: GenSeq[B]): Boolean = {
      val col = Vector(nonEmptyArray*)
      col.containsSlice(that)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given <code>Every</code> as a slice.
      *
      * @param that the <code>Every</code> slice to look for
      * @return true if this <code>NonEmptyArray</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
      */
    final def containsSlice[B](that: Every[B]): Boolean = {
      val col = Vector(nonEmptyArray*)
      col.containsSlice(that)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given <code>NonEmptyArray</code> as a slice.
      *
      * @param that the <code>NonEmptyArray</code> slice to look for
      * @return true if this <code>NonEmptyArray</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
      */
    final def containsSlice[B](that: NonEmptyArray[B]): Boolean = {
      val col = Vector(nonEmptyArray*)
      col.containsSlice(that)
    }

    /**
    * The length of this <code>NonEmptyArray</code>.
    *
    * <p>
    * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
    * </p>
    *
    * @return the number of elements in this <code>NonEmptyArray</code>. 
    */
    final def length: Int = nonEmptyArray.length
  }
}
