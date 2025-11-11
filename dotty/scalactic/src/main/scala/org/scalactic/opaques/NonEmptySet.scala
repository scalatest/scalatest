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

import scala.collection.GenSet
import scala.collection.mutable.{ArrayBuffer, Buffer}

/**
  * A non-empty Set: an ordered, immutable, non-empty collection of elements with <code>LinearSeq</code> performance characteristics.
  *
  * <p>
  * The purpose of <code>NonEmptySet</code> is to allow you to express in a type that a <code>Set</code> is non-empty, thereby eliminating the
  * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
  * performance, see <a href="Every.html"><code>Every</code></a>.
  * </p>
  *
  * <h2>Constructing <code>NonEmptySet</code>s</h2>
  *
  * <p>
  * You can construct a <code>NonEmptySet</code> by passing one or more elements to the <code>NonEmptySet.apply</code> factory method:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; NonEmptySet(1, 2, 3)
  * res0: org.scalactic.anyvals.NonEmptySet[Int] = NonEmptySet(1, 2, 3)
  * </pre>
  *
  * <p>
  * Alternatively you can <em>cons</em> elements onto the <code>End</code> singleton object, similar to making a <code>Set</code> starting with <code>Nil</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; 1 :: 2 :: 3 :: Nil
  * res0: Set[Int] = Set(1, 2, 3)
  *
  * scala&gt; 1 :: 2 :: 3 :: End
  * res1: org.scalactic.NonEmptySet[Int] = NonEmptySet(1, 2, 3)
  * </pre>
  *
  * <p>
  * Note that although <code>Nil</code> is a <code>Set[Nothing]</code>, <code>End</code> is
  * not a <code>NonEmptySet[Nothing]</code>, because no empty <code>NonEmptySet</code> exists. (A non-empty Set is a series
  * of connected links; if you have no links, you have no non-empty Set.)
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; val nil: Set[Nothing] = Nil
  * nil: Set[Nothing] = Set()
  *
  * scala&gt; val nada: NonEmptySet[Nothing] = End
  * &lt;console&gt;:16: error: type mismatch;
  * found   : org.scalactic.anyvals.End.type
  * required: org.scalactic.anyvals.NonEmptySet[Nothing]
  *        val nada: NonEmptySet[Nothing] = End
  *                                          ^
  * </pre>
  *
  * <h2>Working with <code>NonEmptySet</code>s</h2>
  *
  * <p>
  * <code>NonEmptySet</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
  * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
  * you'll get an empty <code>Seq</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Set(1).tail
  * res6: Set[Int] = Set()
  * </pre>
  *
  * <p>
  * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
  * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptySet</code> defines a method corresponding to every such <code>Seq</code>
  * method. Here are some examples:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptySet(1, 2, 3).map(_ + 1)                        // Result: NonEmptySet(2, 3, 4)
  * NonEmptySet(1).map(_ + 1)                              // Result: NonEmptySet(2)
  * NonEmptySet(1, 2, 3).containsSlice(NonEmptySet(2, 3)) // Result: true
  * NonEmptySet(1, 2, 3).containsSlice(NonEmptySet(3, 4)) // Result: false
  * NonEmptySet(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
  * </pre>
  *
  * <p>
  * <code>NonEmptySet</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
  * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptySet</code> to <code>Set</code>
  * is defined in the <code>NonEmptySet</code> companion object that will be applied if you attempt to call one of the missing methods. As a
  * result, you can invoke <code>filter</code> on an <code>NonEmptySet</code>, even though <code>filter</code> could result
  * in an empty sequence&mdash;but the result type will be <code>Set</code> instead of <code>NonEmptySet</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptySet(1, 2, 3).filter(_ &lt; 10) // Result: Set(1, 2, 3)
  * NonEmptySet(1, 2, 3).filter(_ &gt; 10) // Result: Set()
  * </pre>
  *
  *
  * <p>
  * You can use <code>NonEmptySet</code>s in <code>for</code> expressions. The result will be an <code>NonEmptySet</code> unless
  * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
  * result type will switch to a <code>Set</code> at that point. Here are some examples:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; import org.scalactic.anyvals._
  * import org.scalactic.anyvals._
  *
  * scala&gt; for (i &lt;- NonEmptySet(1, 2, 3)) yield i + 1
  * res0: org.scalactic.anyvals.NonEmptySet[Int] = NonEmptySet(2, 3, 4)
  *
  * scala&gt; for (i &lt;- NonEmptySet(1, 2, 3) if i &lt; 10) yield i + 1
  * res1: Set[Int] = Set(2, 3, 4)
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptySet(1, 2, 3)
  *      |   j &lt;- NonEmptySet('a', 'b', 'c')
  *      | } yield (i, j)
  * res3: org.scalactic.anyvals.NonEmptySet[(Int, Char)] =
  *         NonEmptySet((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptySet(1, 2, 3) if i &lt; 10
  *      |   j &lt;- NonEmptySet('a', 'b', 'c')
  *      | } yield (i, j)
  * res6: Set[(Int, Char)] =
  *         Set((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  * </pre>
  *
  * @tparam T the type of elements contained in this <code>NonEmptySet</code>
  */
opaque type NonEmptySet[T] = Set[T]

/**
  * Companion object for class <code>NonEmptyList</code>.
  */
object NonEmptySet {

  /**
    * Constructs a new <code>NonEmptySet</code> given at least one element.
    *
    * @tparam T the type of the element contained in the new <code>NonEmptySet</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptySet</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptySet</code>
    */
  def apply[T](firstElement: T, otherElements: T*): NonEmptySet[T] = otherElements.toSet + firstElement

  /**
    * Variable argument extractor for <code>NonEmptySet</code>s.
    *
    * @param nonEmptySet: the <code>NonEmptySet</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptySet</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[T](nonEmptySet: NonEmptySet[T]): Option[Seq[T]] = Some(nonEmptySet.toSeq)

  /**
    * Optionally construct a <code>NonEmptySet</code> containing the elements, if any, of a given <code>GenSet</code>.
    *
    * @param set the <code>GenSet</code> with which to construct a <code>NonEmptySet</code>
    * @return a <code>NonEmptySet</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
    *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
    */
  def from[T](set: GenSet[T]): Option[NonEmptySet[T]] =
    set.headOption match {
      case None => None
      case Some(first) => Some(scala.collection.immutable.Set.empty[T] ++ set)
    }

  /**
    * Conversion from <code>NonEmptySet</code> to <code>IterableOnce</code>.
    *
    * @param nonEmptySet the <code>NonEmptySet</code> to convert
    * @return the <code>IterableOnce</code>
    */
  given [E]: Conversion[NonEmptySet[E], IterableOnce[E]] with {
    def apply(nonEmptySet: NonEmptySet[E]): IterableOnce[E] = nonEmptySet
  }  

  extension [T](nonEmptySet: NonEmptySet[T]) {

    /**
      * Returns a new <code>NonEmptySet</code> containing the elements of this <code>NonEmptySet</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptySet</code> is the most specific superclass encompassing the element types of this <code>NonEmptySet</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptySet</code> that contains all the elements of this <code>NonEmptySet</code> followed by all elements of <code>other</code>.
      */
    def ++(other: IterableOnce[T]): NonEmptySet[T] =
      if (other.isEmpty) nonEmptySet else toSet ++ other.toSet

    /**
      * Returns a new <code>NonEmptySet</code> with the given element added.
      *
      *
      * @param element the element to add to this <code>NonEmptySet</code>
      * @return a new <code>NonEmptySet</code> consisting of <code>element</code> and all elements of this <code>NonEmptySet</code>.
      */
    def +(element: T): NonEmptySet[T] = toSet + element

    /**
      * Appends all elements of this <code>NonEmptySet</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptySet</code>, without any separator string.
      *
      * @param sb the string builder to which elements will be appended
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder): StringBuilder = toSet.addString(sb)

    /**
      * Appends all elements of this <code>NonEmptySet</code> to a string builder using a separator string. The written text will consist of a concatenation of the
      * result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptySet</code>, separated by the string <code>sep</code>.
      *
      * @param sb the string builder to which elements will be appended
      * @param sep the separator string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, sep: String): StringBuilder = toSet.addString(sb, sep)

    /**
      * Appends all elements of this <code>NonEmptySet</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
      * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptySet</code>,
      * separated by the string <code>sep</code>; and the string <code>end</code>
      *
      * @param sb the string builder to which elements will be appended
      * @param start the starting string
      * @param sep the separator string
      * @param start the ending string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = toSet.addString(sb, start, sep, end)

    /**
      * Check if an element exists at its index in the <code>NonEmptySet</code>.
      *
      * @return <code>true</code> if a element exists in <code>NonEmptySet</code> at index <code>idx</code>, where <code>false</code> indicates the element at index <code>idx</code> does not exist.
      */
    def apply(elem: T): Boolean = toSet(elem)

    /**
      * Finds the first element of this <code>NonEmptySet</code> for which the given partial function is defined, if any, and applies the partial function to it.
      *
      * @param pf the partial function
      * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
      *    the partial function was not defined for any element.
      */
    def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = toSet.collectFirst(pf)

    /**
      * Indicates whether this <code>NonEmptySet</code> contains a given value as an element.
      *
      * @param elem the element to look for
      * @return true if this <code>NonEmptySet</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
      */
    def contains(elem: T): Boolean = toSet.contains(elem)

    /**
      * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptySet</code>. Copying
      * will stop once either the end of the current <code>NonEmptySet</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      */
    def copyToArray[U >: T](arr: Array[U]): Unit = toSet.copyToArray(arr)

    /**
      * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptySet</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptySet</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      * @param start the starting index
      */
    def copyToArray[U >: T](arr: Array[U], start: Int): Unit = toSet.copyToArray(arr, start)

    /**
      * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptySet</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptySet</code> is reached, the end of the array is reached, or
      * <code>len</code> elements have been copied.
      *
      * @param arr the array to fill
      * @param start the starting index
      * @param len the maximum number of elements to copy
      */
    def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = toSet.copyToArray(arr, start, len)

    /**
      * Copies all elements of this <code>NonEmptySet</code> to a buffer. 
      *
      * @param buf the buffer to which elements are copied
      */
    def copyToBuffer[U >: T](buf: Buffer[U]): Unit = toSet.copyToBuffer(buf)

    /**
      * Counts the number of elements in this <code>NonEmptySet</code> that satisfy a predicate. 
      *
      * @param p the predicate used to test elements.
      * @return the number of elements satisfying the predicate <code>p</code>. 
      */
    def count(p: T => Boolean): Int = toSet.count(p)

    /**
      * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptySet</code>.
      *
      * @param p the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptySet</code>, otherwise <code>false</code>. 
      */
    def exists(p: T => Boolean): Boolean = toSet.exists(p)

    /**
      * Finds the first element of this <code>NonEmptySet</code> that satisfies the given predicate, if any.
      *
      * @param p the predicate used to test elements
      * @return an <code>Some</code> containing the first element in this <code>NonEmptySet</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
      */
    def find(p: T => Boolean): Option[T] = toSet.find(p)

    /**
      * Builds a new <code>NonEmptySet</code> by applying a function to all elements of this <code>NonEmptySet</code> and using the elements of the resulting <code>NonEmptySet</code>s.
      *
      * @tparam U the element type of the returned <code>NonEmptySet</code>
      * @param f the function to apply to each element.
      * @return a new <code>NonEmptySet</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptySet</code> and concatenating
      *    the elements of resulting <code>NonEmptySet</code>s. 
      */
    def flatMap[U](f: T => NonEmptySet[U]): NonEmptySet[U] = {
      val buf = new ArrayBuffer[U]
      for (ele <- toSet)
        buf ++= f(ele).toSet
      buf.toSet
    }

    /**
      * Converts this <code>NonEmptySet</code> of <code>NonEmptySet</code>s into a <code>NonEmptySet</code>
      * formed by the elements of the nested <code>NonEmptySet</code>s.
      *
      * <p>
      * Note: You cannot use this <code>flatten</code> method on a <code>NonEmptySet</code> that contains a <code>IterableOnce</code>s, because 
      * if all the nested <code>IterableOnce</code>s were empty, you'd end up with an empty <code>NonEmptySet</code>.
      * </p>
      *
      * @tparm B the type of the elements of each nested <code>NonEmptySet</code>
      * @return a new <code>NonEmptySet</code> resulting from concatenating all nested <code>NonEmptySet</code>s.
      */
    def flatten[B](using ev: T <:< NonEmptySet[B]): NonEmptySet[B] = toSet.flatten(ev)

    /**
      * Folds the elements of this <code>NonEmptySet</code> using the specified associative binary operator.
      *
      * <p>
      * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
      * </p>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T.
      * @param z a neutral element for the fold operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for Set concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return the result of applying fold operator <code>op</code> between all the elements and <code>z</code>
      */
    def fold[U >: T](z: U)(op: (U, U) => U): U = toSet.fold(z)(op)

    /**
      * Applies a binary operator to a start value and all elements of this <code>NonEmptySet</code>, going left to right.
      *
      * @tparam B the result type of the binary operator.
      * @param z the start value.
      * @param op the binary operator.
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>, going left to right, with the start value,
      *     <code>z</code>, on the left:
      *
      * <pre>
      * op(...op(op(z, x_1), x_2), ..., x_n)
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptySet</code>. 
      * </p>
      */
    def foldLeft[B](z: B)(op: (B, T) => B): B = toSet.foldLeft(z)(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptySet</code> and a start value, going right to left.
      *
      * @tparam B the result of the binary operator
      * @param z the start value
      * @param op the binary operator
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>, going right to left, with the start value,
      *     <code>z</code>, on the right:
      *
      * <pre>
      * op(x_1, op(x_2, ... op(x_n, z)...))
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptySet</code>. 
      * </p>
      */
    def foldRight[B](z: B)(op: (T, B) => B): B = toSet.foldRight(z)(op)

    /**
      * Indicates whether a predicate holds for all elements of this <code>NonEmptySet</code>.
      *
      * @param p the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptySet</code>, otherwise <code>false</code>. 
      */
    def forall(p: T => Boolean): Boolean = toSet.forall(p)

    /**
      * Applies a function <code>f</code> to all elements of this <code>NonEmptySet</code>.
      *
      * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
      */
    def foreach(f: T => Unit): Unit = toSet.foreach(f)

    /**
      * Builds a new <code>NonEmptySet</code> by applying a function to all elements of this <code>NonEmptySet</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptySet</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptySet</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptySet</code> and collecting the results. 
      */
    def map[U](f: T => U): NonEmptySet[U] = toSet.map(f)

    /**
      * Converts this <code>NonEmptySet</code> to a standard Scala <code>Set</code>.
      *
      * @return a <code>Set</code> containing all elements of this <code>NonEmptySet</code>
      */
    def toSet: Set[T] = nonEmptySet

    /**
      * The size of this <code>NonEmptySet</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptySet</code>. 
      */
    def size: Int = toSet.size

  }

}