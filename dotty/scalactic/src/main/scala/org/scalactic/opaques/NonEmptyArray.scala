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
import scala.reflect.ClassTag
import scala.collection.mutable.{ArrayBuffer, Buffer}
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

opaque type NonEmptyArray[T] = Array[T]

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
  def apply[T](firstElement: T, otherElements: T*)(using classTag: ClassTag[T]): NonEmptyArray[T] = (firstElement +: otherElements).toArray

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
  def from[T](seq: GenSeq[T])(using classTag: ClassTag[T]): Option[NonEmptyArray[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some((first +: seq.tail).toArray)
    } 

  /**
    * Conversion from <code>NonEmptyArray</code> to <code>IterableOnce</code>.
    *
    * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
    * @return the <code>IterableOnce</code>
    */
  given [E]: Conversion[NonEmptyArray[E], IterableOnce[E]] with {
    def apply(nonEmptyArray: NonEmptyArray[E]): IterableOnce[E] = 
      new IterableOnce[E] {
        def iterator: Iterator[E] = new ArrayOps(nonEmptyArray).iterator
      }
  }
  /**
    * Conversion from <code>NonEmptyArray</code> to <code>PartialFunction</code>.
    *
    * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
    * @return the <code>PartialFunction</code>
    */
  given [E]: Conversion[NonEmptyArray[E], PartialFunction[Int, E]] with {
    def apply(nonEmptyArray: NonEmptyArray[E]): PartialFunction[Int, E] =
      new PartialFunction[Int, E] {
        def apply(i: Int): E = nonEmptyArray.toArray.apply(i)
        def isDefinedAt(i: Int): Boolean = i >= 0 && i < nonEmptyArray.length
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
    infix def ++[U >: T](other: IterableOnce[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
      ArrayOps(nonEmptyArray) ++ other
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
    infix def :+[U >: T](element: U)(using classTag: ClassTag[U]): NonEmptyArray[U] = { 
      ArrayOps(nonEmptyArray).appended(element)
    }

    /**
      * Appends all elements of this <code>NonEmptyArray</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptyArray</code>, without any separator string.
      *
      * @param sb the string builder to which elements will be appended
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb)

    /**
      * Appends all elements of this <code>NonEmptyArray</code> to a string builder using a separator string. The written text will consist of a concatenation of the
      * result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptyArray</code>, separated by the string <code>sep</code>.
      *
      * @param sb the string builder to which elements will be appended
      * @param sep the separator string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, sep: String): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb, sep)

    /**
      * Appends all elements of this <code>NonEmptyArray</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
      * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptyArray</code>,
      * separated by the string <code>sep</code>; and the string <code>end</code>
      *
      * @param sb the string builder to which elements will be appended
      * @param start the starting string
      * @param sep the separator string
      * @param start the ending string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb, start, sep, end)

    /**
      * Finds the first element of this <code>NonEmptyArray</code> for which the given partial function is defined, if any, and applies the partial function to it.
      *
      * @param pf the partial function
      * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
      *    the partial function was not defined for any element.
      */
    def collectFirst[U](pf: PartialFunction[T, U])(using classTagOfU: ClassTag[U]): Option[U] = new ArrayOps(nonEmptyArray).collectFirst(pf)

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given value as an element.
      *
      * @param elem the element to look for
      * @return true if this <code>NonEmptyArray</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
      */
    def contains(elem: T): Boolean = {
      val arrOps = new ArrayOps(nonEmptyArray)
      arrOps.contains(elem)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> slice to look for
      * @return true if this <code>NonEmptyArray</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
      */
    def containsSlice[B](that: IterableOnce[B]): Boolean = nonEmptyArray.toIndexedSeq.containsSlice(that.toList)

    /**
    * Builds a new <code>NonEmptyArray</code> from this <code>NonEmptyArray</code> without any duplicate elements.
    *
    * @return A new <code>NonEmptyArray</code> that contains the first occurrence of every element of this <code>NonEmptyArray</code>. 
    */
    def distinct: NonEmptyArray[T] = {
      val arrOps = new ArrayOps(nonEmptyArray)
      arrOps.distinct
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> ends with the given <code>IterableOnce</code>.
      *
      * @param that the sequence to test
      * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
      */
    def endsWith[B](that: IterableOnce[B]): Boolean = new ArrayOps(nonEmptyArray).endsWith(that.toList)

    /**
      * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptyArray</code>.
      *
      * @param the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyArray</code>, otherwise <code>false</code>. 
      */
    def exists(p: T => Boolean): Boolean = new ArrayOps(nonEmptyArray).exists(p)

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code> and using the elements of the resulting <code>NonEmptyArray</code>s.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>
      * @param f the function to apply to each element.
      * @return a new <code>NonEmptyArray</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and concatenating
      *    the elements of resulting <code>NonEmptyArray</code>s. 
      */
    def flatMap[U](f: T => NonEmptyArray[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
      val buf = new ArrayBuffer[U]
      for (ele <- nonEmptyArray)
        buf ++= f(ele).toArray
      buf.toArray
    }

    /**
      * Finds the first element of this <code>NonEmptyArray</code> that satisfies the given predicate, if any.
      *
      * @param p the predicate used to test elements
      * @return an <code>Some</code> containing the first element in this <code>NonEmptyArray</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
      */
    def find(p: T => Boolean): Option[T] = new ArrayOps(nonEmptyArray).find(p)

    /**
      * Converts this <code>NonEmptyArray</code> of <code>NonEmptyArray</code>s into a <code>NonEmptyArray</code>
      * formed by the elements of the nested <code>NonEmptyArray</code>s.
      *
      * <p>
      * Note: You cannot use this <code>flatten</code> method on a <code>NonEmptyArray</code> that contains a <code>IterableOnce</code>s, because 
      * if all the nested <code>IterableOnce</code>s were empty, you'd end up with an empty <code>NonEmptyArray</code>.
      * </p>
      *
      * @tparm B the type of the elements of each nested <code>NonEmptyArray</code>
      * @return a new <code>NonEmptyArray</code> resulting from concatenating all nested <code>NonEmptyArray</code>s.
      */
    def flatten[B](using ev: T <:< NonEmptyArray[B], classTag: ClassTag[B]): NonEmptyArray[B] = flatMap(ev)

    /**
      * Folds the elements of this <code>NonEmptyArray</code> using the specified associative binary operator.
      *
      * <p>
      * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
      * </p>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T.
      * @param z a neutral element for the fold operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return the result of applying fold operator <code>op</code> between all the elements and <code>z</code>
      */
    def fold[U >: T](z: U)(op: (U, U) => U): U = new ArrayOps(nonEmptyArray).fold(z)(op)

    /**
      * Applies a binary operator to a start value and all elements of this <code>NonEmptyArray</code>, going left to right.
      *
      * @tparam B the result type of the binary operator.
      * @param z the start value.
      * @param op the binary operator.
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going left to right, with the start value,
      *     <code>z</code>, on the left:
      *
      * <pre>
      * op(...op(op(z, x_1), x_2), ..., x_n)
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
      * </p>
      */
    def foldLeft[B](z: B)(op: (B, T) => B): B = new ArrayOps(nonEmptyArray).foldLeft(z)(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyArray</code> and a start value, going right to left.
      *
      * @tparam B the result of the binary operator
      * @param z the start value
      * @param op the binary operator
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going right to left, with the start value,
      *     <code>z</code>, on the right:
      *
      * <pre>
      * op(x_1, op(x_2, ... op(x_n, z)...))
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
      * </p>
      */
    def foldRight[B](z: B)(op: (T, B) => B): B = new ArrayOps(nonEmptyArray).foldRight(z)(op)

    /**
      * Indicates whether a predicate holds for all elements of this <code>NonEmptyArray</code>.
      *
      * @param p the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptyArray</code>, otherwise <code>false</code>. 
      */
    def forall(p: T => Boolean): Boolean = new ArrayOps(nonEmptyArray).forall(p)

    /**
      * Applies a function <code>f</code> to all elements of this <code>NonEmptyArray</code>.
      *
      * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
      */
    def foreach(f: T => Unit): Unit = new ArrayOps(nonEmptyArray).foreach(f)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyArray</code> has a definite size, since all <code>NonEmptyArray</code>s are strict collections.
      */
    def hasDefiniteSize: Boolean = true

    /**
      * Selects the first element of this <code>NonEmptyArray</code>. 
      *
      * @return the first element of this <code>NonEmptyArray</code>.
      */
    def head: T = new ArrayOps(nonEmptyArray).head

    /**
      * Selects the first element of this <code>NonEmptyArray</code> and returns it wrapped in a <code>Some</code>. 
      *
      * @return the first element of this <code>NonEmptyArray</code>, wrapped in a <code>Some</code>.
      */
    def headOption: Option[T] = new ArrayOps(nonEmptyArray).headOption

    /**
      * Finds index of first occurrence of some value in this <code>NonEmptyArray</code>.
      *
      * @param elem the element value to search for. 
      * @return the index of the first element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexOf(elem: T): Int = new ArrayOps(nonEmptyArray).indexOf(elem, 0)

    /**
      * Finds index of first occurrence of some value in this <code>NonEmptyArray</code> after or at some start index.
      *
      * @param elem the element value to search for. 
      * @param from the start index
      * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexOf(elem: T, from: Int): Int = new ArrayOps(nonEmptyArray).indexOf(elem, from)


    /**
      * Finds first index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @return the first index at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
      *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def indexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyArray.toIndexedSeq.indexOfSlice(that.toList)

    /**
      * Finds first index after or at a start index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @param from the start index
      * @return the first index <code>&gt;=</code> <code>from</code> at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
      *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def indexOfSlice[U >: T](that: IterableOnce[U], from: Int): Int = nonEmptyArray.toIndexedSeq.indexOfSlice(that.toList, from)

    /**
      * Finds index of the first element satisfying some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the index of the first element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexWhere(p: T => Boolean): Int = new ArrayOps(nonEmptyArray).indexWhere(p)

    /**
      * Finds index of the first element satisfying some predicate after or at some start index.
      *
      * @param p the predicate used to test elements.
      * @param from the start index
      * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexWhere(p: T => Boolean, from: Int): Int = new ArrayOps(nonEmptyArray).indexWhere(p, from)

    /**
      * Produces the range of all indices of this <code>NonEmptyArray</code>. 
      *
      * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyArray</code>. 
      */
    def indices: Range = new ArrayOps(nonEmptyArray).indices

    /**
      * Returns <code>false</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, is non-empty.
      *
      * @return false
      */
    def isEmpty: Boolean = false

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, can be traversed repeatedly.
      *
      * @return true
      */
    def isTraversableAgain: Boolean = true

    /**
      * Selects the last element of this <code>NonEmptyArray</code>. 
      *
      * @return the last element of this <code>NonEmptyArray</code>.
      */
    def last: T = new ArrayOps(nonEmptyArray).last

    /**
      * Finds the index of the last occurrence of some value in this <code>NonEmptyArray</code>.
      *
      * @param elem the element value to search for.
      * @return the index of the last element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def lastIndexOf(elem: T): Int = new ArrayOps(nonEmptyArray).lastIndexOf(elem)

    /**
      * Finds the index of the last occurrence of some value in this <code>NonEmptyArray</code> before or at a given <code>end</code> index.
      *
      * @param elem the element value to search for.
      * @param end the end index. 
      * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>)
      *     to <code>elem</code>, or <code>-1</code>, if none exists.
      */
    def lastIndexOf(elem: T, end: Int): Int = new ArrayOps(nonEmptyArray).lastIndexOf(elem, end)

    /**
      * Finds the last index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice. 
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @return the last index at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
      *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def lastIndexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyArray.toIndexedSeq.lastIndexOfSlice(that.toList)

    /**
      * Finds the last index before or at a given end index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice. 
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @param end the end index
      * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
      *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def lastIndexOfSlice[U >: T](that: IterableOnce[U], end: Int): Int = nonEmptyArray.toIndexedSeq.lastIndexOfSlice(that.toList, end)

    /**
      * Finds index of last element satisfying some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the index of the last element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists. 
      */
    def lastIndexWhere(p: T => Boolean): Int = nonEmptyArray.toIndexedSeq.lastIndexWhere(p)

    /**
      * Finds index of last element satisfying some predicate before or at given end index.
      *
      * @param p the predicate used to test elements.
      * @param end the end index
      * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists. 
      */
    def lastIndexWhere(p: T => Boolean, end: Int): Int = nonEmptyArray.toIndexedSeq.lastIndexWhere(p, end)

    /**
      * Returns the last element of this <code>NonEmptyArray</code>, wrapped in a <code>Some</code>. 
      *
      * @return the last element, wrapped in a <code>Some</code>. 
      */
    def lastOption: Option[T] = new ArrayOps(nonEmptyArray).lastOption // Will always return a Some

    /**
      * The length of this <code>NonEmptyArray</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyArray</code>. 
      */
    def length: Int = nonEmptyArray.toArray.length

    /**
      * Compares the length of this <code>NonEmptyArray</code> to a test value. 
      *
      * @param len the test value that gets compared with the length.
      * @return a value <code>x</code> where
      *
      * <pre>
      * x &lt; 0 if this.length &lt; len
      * x == 0 if this.length == len
      * x &gt; 0 if this.length &gt; len
      * </pre>
      */
    def lengthCompare(len: Int): Int = new ArrayOps(nonEmptyArray).lengthCompare(len)

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyArray</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and collecting the results. 
      */
    def map[U](f: T => U)(using classTag: ClassTag[U]): NonEmptyArray[U] ={
      val buf = new ArrayBuffer[U]
      for (ele <- nonEmptyArray)
        buf += f(ele)
      buf.toArray
    }

    /**
      * Finds the largest element.
      *
      * @return the largest element of this <code>NonEmptyArray</code>. 
      */
    def max[U >: T](using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.max(cmp)

    /**
      * Finds the largest result after applying the given function to every element.
      *
      * @return the largest result of applying the given function to every element of this <code>NonEmptyArray</code>. 
      */
    def maxBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.maxBy(f)(cmp)

    /**
      * Finds the smallest element.
      *
      * @return the smallest element of this <code>NonEmptyArray</code>. 
      */
    def min[U >: T](using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.min(cmp)

    /**
      * Finds the smallest result after applying the given function to every element.
      *
      * @return the smallest result of applying the given function to every element of this <code>NonEmptyArray</code>. 
      */
    def minBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.minBy(f)(cmp)

    /**
      * Partitions this <code>NonEmptyArray</code> into a map of <code>NonEmptyArray</code>s according to some discriminator function.
      *
      * @tparam K the type of keys returned by the discriminator function.
      * @param f the discriminator function.
      * @return A map from keys to <code>NonEmptyArray</code>s such that the following invariant holds:
      *
      * <pre>
      * (nonEmptyArray.toArray partition f)(k) = xs filter (x =&gt; f(x) == k)
      * </pre>
      *
      * <p>
      * That is, every key <code>k</code> is bound to a <code>NonEmptyArray</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
      * </p>
      */
    def groupBy[K](f: T => K)(using classTag: ClassTag[T]): Map[K, NonEmptyArray[T]] = {
      val mapKToArray = (new ArrayOps(nonEmptyArray)).groupBy(f)
      (mapKToArray.mapValues{ list => NonEmptyArray(list.head, list.tail.toList*) }).toMap
    }

    /**
      * Partitions elements into fixed size <code>NonEmptyArray</code>s.
      *
      * @param size the number of elements per group
      * @return An iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
      */
    def grouped(size: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = {
      if (size < 1)
        throw new IllegalArgumentException(Resources.invalidSize(size))
      val itOfArray = (new ArrayOps(nonEmptyArray)).grouped(size)
      itOfArray.map { list => NonEmptyArray(list.head, list.tail.toList*) }
    }

    /**
      * Displays all elements of this <code>NonEmptyArray</code> in a string. 
      *
      * @return a string representation of this <code>NonEmptyArray</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptyArray</code> follow each other without any separator string. 
      */
    def mkString: String = nonEmptyArray.toIndexedSeq.mkString

    /**
      * Displays all elements of this <code>NonEmptyArray</code> in a string using a separator string. 
      *
      * @param sep the separator string
      * @return a string representation of this <code>NonEmptyArray</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptyArray</code> are separated by the string <code>sep</code>. 
      */
    def mkString(sep: String): String = nonEmptyArray.toIndexedSeq.mkString(sep)

    /**
      * Displays all elements of this <code>NonEmptyArray</code> in a string using start, end, and separator strings. 
      *
      * @param start the starting string.
      * @param sep the separator string.
      * @param end the ending string.
      * @return a string representation of this <code>NonEmptyArray</code>. The resulting string begins with the string <code>start</code> and ends with the string
      *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>NonEmptyArray</code> are
      *     separated by the string <code>sep</code>. 
      */
    def mkString(start: String, sep: String, end: String): String = nonEmptyArray.toIndexedSeq.mkString(start, sep, end)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, is non-empty.
      *
      * @return true
      */
    def nonEmpty: Boolean = true

    /**
      * A copy of this <code>NonEmptyArray</code> with an element value appended until a given target length is reached.
      *
      * @param len the target length 
      * @param elem he padding value
      * @return a new <code>NonEmptyArray</code> consisting of all elements of this <code>NonEmptyArray</code> followed by the minimal number of occurrences
      *     of <code>elem</code> so that the resulting <code>NonEmptyArray</code> has a length of at least <code>len</code>. 
      */
    def padTo[U >: T](len: Int, elem: U)(using classTag: ClassTag[U]): NonEmptyArray[U] = {
      (new ArrayOps(nonEmptyArray)).padTo(len, elem)
    }

    /**
      * Produces a new <code>NonEmptyArray</code> where a slice of elements in this <code>NonEmptyArray</code> is replaced by another <code>NonEmptyArray</code>
      *
      * @param from the index of the first replaced element 
      * @param that the <code>NonEmptyArray</code> whose elements should replace a slice in this <code>NonEmptyArray</code>
      * @param replaced the number of elements to drop in the original <code>NonEmptyArray</code>
      */
    def patch[U >: T](from: Int, that: NonEmptyArray[U], replaced: Int)(using classTag: ClassTag[U]): NonEmptyArray[U] =
      (new ArrayOps(nonEmptyArray)).patch(from, that.toArray, replaced)

    /**
      * Iterates over distinct permutations. 
      *
      * <p>
      * Here's an example:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray('a', 'b', 'b').permutations.toArray = Array(NonEmptyArray(a, b, b), NonEmptyArray(b, a, b), NonEmptyArray(b, b, a))
      * </pre>
      *
      * @return an iterator that traverses the distinct permutations of this <code>NonEmptyArray</code>.
      */
    def permutations: Iterator[NonEmptyArray[T]] =
      (new ArrayOps(nonEmptyArray)).permutations

    /**
      * Returns the length of the longest prefix whose elements all satisfy some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the length of the longest prefix of this <code>NonEmptyArray</code> such that every element
      *     of the segment satisfies the predicate <code>p</code>. 
      */
    def prefixLength(p: T => Boolean): Int = nonEmptyArray.toIndexedSeq.prefixLength(p)  

    /**
      * The result of multiplying all the elements of this <code>NonEmptyArray</code>.
      *
      * <p>
      * This method can be invoked for any <code>NonEmptyArray[T]</code> for which an given <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the product of all elements
      */
    def product[U >: T](using num: Numeric[U]): U = nonEmptyArray.toIndexedSeq.product(num)

    /**
      * Reduces the elements of this <code>NonEmptyArray</code> using the specified associative binary operator.
      *
      * <p>
      * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
      * </p>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T.
      * @param op a binary operator that must be associative.
      * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptyArray</code>.
      */
    def reduce[U >: T](op: (U, U) => U): U = nonEmptyArray.toIndexedSeq.reduce(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going left to right.
      *
      * @tparam U the result type of the binary operator.
      * @param op the binary operator.
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going left to right:
      *
      * <pre>
      * op(...op(op(x_1, x_2), x_3), ..., x_n)
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
      * </p>
      */
    def reduceLeft[U >: T](op: (U, T) => U): U = nonEmptyArray.toIndexedSeq.reduceLeft(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going left to right, returning the result in a <code>Some</code>.
      *
      * @tparam U the result type of the binary operator.
      * @param op the binary operator.
      * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
      * </p>
      */
    def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceLeftOption(op)

    def reduceOption[U >: T](op: (U, U) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceOption(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going right to left.
      *
      * @tparam U the result of the binary operator
      * @param op the binary operator
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going right to left:
      *
      * <pre>
      * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
      * </p>
      */
    def reduceRight[U >: T](op: (T, U) => U): U = nonEmptyArray.toIndexedSeq.reduceRight(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going right to left, returning the result in a <code>Some</code>.
      *
      * @tparam U the result of the binary operator
      * @param op the binary operator
      * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
      */
    def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceRightOption(op)

    /**
      * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyArray</code>. Copying
      * will stop once either the end of the current <code>NonEmptyArray</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      */
    def copyToArray[U >: T](arr: Array[U]): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, 0)

    /**
      * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyArray</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyArray</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      * @param start the starting index
      */
    def copyToArray[U >: T](arr: Array[U], start: Int): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, start)

    /**
      * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptyArray</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyArray</code> is reached, the end of the array is reached, or
      * <code>len</code> elements have been copied.
      *
      * @param arr the array to fill
      * @param start the starting index
      * @param len the maximum number of elements to copy
      */
    def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, start, len)

    /**
      * Copies all elements of this <code>NonEmptyArray</code> to a buffer. 
      *
      * @param buf the buffer to which elements are copied
      */
    def copyToBuffer[U >: T](buf: Buffer[U]): Unit = nonEmptyArray.toIndexedSeq.copyToBuffer(buf)

    /**
      * Indicates whether every element of this <code>NonEmptyArray</code> relates to the corresponding element of a given <code>IterableOnce</code> by satisfying a given predicate. 
      *
      * @tparam B the type of the elements of <code>that</code>
      * @param that the <code>IterableOnce</code> to compare for correspondence
      * @param p the predicate, which relates elements from this <code>NonEmptyArray</code> and the passed <code>IterableOnce</code>
      * @return true if this <code>NonEmptyArray</code> and the passed <code>IterableOnce</code> have the same length and <code>p(x, y)</code> is <code>true</code>
      *     for all corresponding elements <code>x</code> of this <code>NonEmptyArray</code> and <code>y</code> of that, otherwise <code>false</code>.
      */
    def corresponds[B](that: IterableOnce[B])(p: (T, B) => Boolean): Boolean = nonEmptyArray.toIndexedSeq.corresponds(that)(p)

    /**
      * Counts the number of elements in this <code>NonEmptyArray</code> that satisfy a predicate. 
      *
      * @param p the predicate used to test elements.
      * @return the number of elements satisfying the predicate <code>p</code>. 
      */
    def count(p: T => Boolean): Int = new ArrayOps(nonEmptyArray).count(p)

    /**
      * Returns new <code>NonEmptyArray</code> with elements in reverse order.
      *
      * @return a new <code>NonEmptyArray</code> with all elements of this <code>NonEmptyArray</code> in reversed order. 
      */
    def reverse: NonEmptyArray[T] =
      (new ArrayOps(nonEmptyArray)).reverse

    /**
      * An iterator yielding elements in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyArray.reverseIterator</code> is the same as <code>nonEmptyArray.reverse.iterator</code>, but might be more efficient. 
      * </p>
      *
      * @return an iterator yielding the elements of this <code>NonEmptyArray</code> in reversed order 
      */
    def reverseIterator: Iterator[T] = new ArrayOps(nonEmptyArray).reverseIterator  

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code> and collecting the results in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyArray.reverseMap(f)</code> is the same as <code>nonEmptyArray.reverse.map(f)</code>, but might be more efficient. 
      * </p>
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyArray</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code>
      *     and collecting the results in reverse order. 
      */
    def reverseMap[U](f: T => U)(using classTag: ClassTag[U]): NonEmptyArray[U] =
      nonEmptyArray.toIndexedSeq.reverseMap(f).toArray

    /**
      * Checks if the given <code>IterableOnce</code> contains the same elements in the same order as this <code>NonEmptyArray</code>.
      *
      * @param that the <code>IterableOnce</code> with which to compare
      * @return <code>true</code>, if both this <code>NonEmptyArray</code> and the given <code>IterableOnce</code> contain the same elements
      *     in the same order, <code>false</code> otherwise. 
      */
    def sameElements[U >: T](that: IterableOnce[U]): Boolean = nonEmptyArray.toIndexedSeq.sameElements(that)

    /**
      * Computes a prefix scan of the elements of this <code>NonEmptyArray</code>.
      *
      * <p>
      * Note: The neutral element z may be applied more than once. 
      * </p>
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray(1, 2, 3).scan(0)(_ + _) == NonEmptyArray(0, 1, 3, 6)
      * NonEmptyArray(1, 2, 3).scan("z")(_ + _.toString) == NonEmptyArray("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>NonEmptyArray</code>.
      * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return a new <code>NonEmptyArray</code> containing the prefix scan of the elements in this <code>NonEmptyArray</code> 
      */
    def scan[U >: T](z: U)(op: (U, U) => U)(using classTag: ClassTag[U]): NonEmptyArray[U] = new ArrayOps(nonEmptyArray).scan(z)(op)

    /**
      * Produces a <code>NonEmptyArray</code> containing cumulative results of applying the operator going left to right.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray(1, 2, 3).scanLeft(0)(_ + _) == NonEmptyArray(0, 1, 3, 6)
      * NonEmptyArray(1, 2, 3).scanLeft("z")(_ + _) == NonEmptyArray("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyArray</code>
      * @param z the start value.
      * @param op the binary operator.
      * @return a new <code>NonEmptyArray</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>,
      *     going left to right, with the start value, <code>z</code>, on the left.
      */
    def scanLeft[B](z: B)(op: (B, T) => B)(using classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanLeft(z)(op)

    /**
      * Produces a <code>NonEmptyArray</code> containing cumulative results of applying the operator going right to left.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray(1, 2, 3).scanRight(0)(_ + _) == NonEmptyArray(6, 5, 3, 0)
      * NonEmptyArray(1, 2, 3).scanRight("z")(_ + _) == NonEmptyArray("123z", "23z", "3z", "z")
      * </pre>
      *
      * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyArray</code>
      * @param z the start value
      * @param op the binary operator
      * @return a new <code>NonEmptyArray</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>,
      *     going right to left, with the start value, <code>z</code>, on the right.
      */
    def scanRight[B](z: B)(op: (T, B) => B)(using classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanRight(z)(op)

    /**
      * Computes length of longest segment whose elements all satisfy some predicate.
      *
      * @param p the predicate used to test elements.
      * @param from the index where the search starts.
      * @param the length of the longest segment of this <code>NonEmptyArray</code> starting from index <code>from</code> such that every element of the
      *     segment satisfies the predicate <code>p</code>. 
      */
    def segmentLength(p: T => Boolean, from: Int): Int = nonEmptyArray.toIndexedSeq.segmentLength(p, from)

    /**
      * The size of this <code>NonEmptyArray</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyArray</code>. 
      */
    def size: Int = new ArrayOps(nonEmptyArray).size

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
      *
      * @param size the number of elements per group
      * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size).map(l => NonEmptyArray(l.head, l.tail.toList*))

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
      * moving the sliding window by a given <code>step</code> each time.
      *
      * @param size the number of elements per group
      * @param step the distance between the first elements of successive groups
      * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int, step: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size, step).map(l => NonEmptyArray(l.head, l.tail.toList*))

    /**
      * Sorts this <code>NonEmptyArray</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
      *
      * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
      * @param f the transformation function mapping elements to some other domain <code>U</code>.
      * @param ord the ordering assumed on domain <code>U</code>.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the <code>Ordering</code> where
      *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
      */
    def sortBy[U](f: T => U)(using ord: Ordering[U]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortBy(f)

    /**
      * Sorts this <code>NonEmptyArray</code> according to a comparison function.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyArray</code> as in the original. 
      * </p>
      *
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the comparison function <code>lt</code>.
      */
    def sortWith(lt: (T, T) => Boolean): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortWith(lt)

    /**
      * Sorts this <code>NonEmptyArray</code> according to an <code>Ordering</code>.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyArray</code> as in the original. 
      * </p>
      *
      * @param ord the <code>Ordering</code> to be used to compare elements.
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the comparison function <code>lt</code>.
      */
    def sorted(using ord: Ordering[T]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sorted(ord)

    /**
      * The result of summing all the elements of this <code>NonEmptyArray</code>.
      *
      * <p>
      * This method can be invoked for any <code>NonEmptyArray[T]</code> for which a given <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the sum of all elements
      */
    def sum[U >: T](using num: Numeric[U]): U = nonEmptyArray.toIndexedSeq.sum(num)

    /**
      * Indicates whether this <code>NonEmptyArray</code> starts with the given <code>IterableOnce</code>. 
      *
      * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyArray</code>
      * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
      */
    def startsWith[B](that: IterableOnce[B]): Boolean = new ArrayOps(nonEmptyArray).startsWith(that)

    /**
      * Indicates whether this <code>NonEmptyArray</code> starts with the given <code>IterableOnce</code> at the given index. 
      *
      * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyArray</code>
      * @param offset the index at which this <code>NonEmptyArray</code> is searched.
      * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
      */
    def startsWith[B](that: IterableOnce[B], offset: Int): Boolean = new ArrayOps(nonEmptyArray).startsWith(that, offset)

    /**
      * Returns <code>"NonEmptyArray"</code>, the prefix of this object's <code>toString</code> representation.
      *
      * @return the string <code>"NonEmptyArray"</code>
      */
    def stringPrefix: String = "NonEmptyArray"

    def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] = 
      nonEmptyArray.toIndexedSeq.to(factory)

    /**
      * Converts this <code>NonEmptyArray</code> to a list.
      *
      * @return a list containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toArray: Array[T] = nonEmptyArray

    /**
      * Converts this <code>NonEmptyArray</code> to a list.
      *
      * @return a list containing all elements of this <code>NonEmptyArray</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyArray</code>.
      */
    def toList[U >: T]: List[U] = List(nonEmptyArray*)

    /**
      * Converts this <code>NonEmptyArray</code> to a <code>Vector</code>.
      *
      * @return a <code>Vector</code> containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toVector: Vector[T] = nonEmptyArray.toIndexedSeq.toVector

    /**
      * Converts this <code>NonEmptyArray</code> to a mutable buffer.
      *
      * @return a buffer containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toBuffer[U >: T]: Buffer[U] = nonEmptyArray.toIndexedSeq.toBuffer

    /**
      * Converts this <code>NonEmptyArray</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toIndexedSeq: collection.immutable.IndexedSeq[T] = 
      new collection.immutable.IndexedSeq[T] {
        def length: Int = nonEmptyArray.length
        def apply(idx: Int): T = nonEmptyArray(idx)
      }

    /**
      * Converts this <code>NonEmptyArray</code> to an iterable collection.
      *
      * @return an <code>Iterable</code> containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toIterable: scala.collection.Iterable[T] = nonEmptyArray.toIndexedSeq

    /**
      * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyArray</code>.
      *
      * @return an <code>Iterator</code> containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toIterator: Iterator[T] = new ArrayOps(nonEmptyArray).iterator

    /**
      * Converts this <code>NonEmptyArray</code> to a map.
      *
      * <p>
      * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
      * in the map. Duplicate keys will be overwritten by later keys.
      * </p>
      *
      * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>NonEmptyArray</code>. 
      */
    def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = nonEmptyArray.toIndexedSeq.toMap

    /**
      * Converts this <code>NonEmptyArray</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyArray</code>.
      */
    def toSeq: collection.immutable.Seq[T] = new ArrayOps(nonEmptyArray).toSeq

    /**
      * Converts this <code>NonEmptyArray</code> to a set.
      *
      * @return a set containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toSet[U >: T]: Set[U] = nonEmptyArray.toIndexedSeq.toSet

    /**
      * Converts this <code>NonEmptyArray</code> to a stream.
      *
      * @return a stream containing all elements of this <code>NonEmptyArray</code>. 
      */
    def toStream: Stream[T] = nonEmptyArray.toIndexedSeq.toStream

    /**
      * Converts this <code>NonEmptyArray</code> of pairs into two <code>NonEmptyArray</code>s of the first and second half of each pair. 
      *
      * @tparam L the type of the first half of the element pairs
      * @tparam R the type of the second half of the element pairs
      * @param asPair an given conversion that asserts that the element type of this <code>NonEmptyArray</code> is a pair.
      * @return a pair of <code>NonEmptyArray</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyArray</code>. 
      */
    def unzip[L, R](using asPair: T => (L, R), classTagL: ClassTag[L], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[R]) = {
      val unzipped = new ArrayOps(nonEmptyArray).unzip
      val left: NonEmptyArray[L] = unzipped._1.toArray
      val right: NonEmptyArray[R] = unzipped._2.toArray
      (left, right)
    }

    /**
      * Converts this <code>NonEmptyArray</code> of triples into three <code>NonEmptyArray</code>s of the first, second, and and third element of each triple. 
      *
      * @tparam L the type of the first member of the element triples
      * @tparam R the type of the second member of the element triples
      * @tparam R the type of the third member of the element triples
      * @param asTriple an given conversion that asserts that the element type of this <code>NonEmptyArray</code> is a triple.
      * @return a triple of <code>NonEmptyArray</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptyArray</code>. 
      */
    def unzip3[L, M, R](using asTriple: T => (L, M, R), classTagL: ClassTag[L], classTagM: ClassTag[M], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[M], NonEmptyArray[R]) = {
      val unzipped = new ArrayOps(nonEmptyArray).unzip3
      val left: NonEmptyArray[L] = unzipped._1.toArray
      val middle: NonEmptyArray[M] = unzipped._2.toArray
      val right: NonEmptyArray[R] = unzipped._3.toArray
      (left, middle, right)
    }

    /**
      * A copy of this <code>NonEmptyArray</code> with one single replaced element.
      *
      * @param idx the position of the replacement
      * @param elem the replacing element
      * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyArray</code>
      * @return a copy of this <code>NonEmptyArray</code> with the element at position <code>idx</code> replaced by <code>elem</code>. 
      */
    def updated[U >: T](idx: Int, elem: U)(using classTag: ClassTag[U]): NonEmptyArray[U] =
      new ArrayOps(nonEmptyArray).updated(idx, elem)

    /**
      * Returns a <code>NonEmptyArray</code> formed from this <code>NonEmptyArray</code> and an iterable collection by combining corresponding
      * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
      * shorter collection to the length of the longer.
      *
      * @tparm O the type of the second half of the returned pairs
      * @tparm U the type of the first half of the returned pairs
      * @param other the <code>IterableOnce</code> providing the second half of each result pair
      * @param thisElem the element to be used to fill up the result if this <code>NonEmptyArray</code> is shorter than <code>that</code> <code>Iterable</code>.
      * @param thatElem the element to be used to fill up the result if <code>that</code> <code>IterableOnce</code> is shorter than this <code>NonEmptyArray</code>.
      * @return a new <code>NonEmptyArray</code> containing pairs consisting of corresponding elements of this <code>NonEmptyArray</code> and <code>that</code>. The
      *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyArray</code> and <code>that</code>. If this <code>NonEmptyArray</code>
      *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
      *     <code>NonEmptyArray</code>, <code>thatElem</code> values are used to pad the result. 
      */
    def zipAll[O, U >: T](other: IterableOnce[O], thisElem: U, otherElem: O): NonEmptyArray[(U, O)] =
      new ArrayOps(nonEmptyArray).zipAll(other.toIterable, thisElem, otherElem)

    /**
      * Zips this <code>NonEmptyArray</code>  with its indices.
      *
      * @return A new <code>NonEmptyArray</code> containing pairs consisting of all elements of this <code>NonEmptyArray</code> paired with their index. Indices start at 0.
      */
    def zipWithIndex: NonEmptyArray[(T, Int)] = new ArrayOps(nonEmptyArray).zipWithIndex  
  }
}
