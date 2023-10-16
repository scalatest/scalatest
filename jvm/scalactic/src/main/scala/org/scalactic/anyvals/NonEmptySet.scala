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
package org.scalactic.anyvals

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.GenSet
import org.scalactic.ColCompatHelper.{IterableOnce, Iterable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import org.scalactic.Every


// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
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
final class NonEmptySet[T] private (val toSet: Set[T]) extends AnyVal {

  /**
    * Returns a new <code>NonEmptySet</code> containing the elements of this <code>NonEmptySet</code> followed by the elements of the passed <code>NonEmptySet</code>.
    * The element type of the resulting <code>NonEmptySet</code> is the most specific superclass encompassing the element types of this and the passed <code>NonEmptySet</code>.
    *
    * @tparam U the element type of the returned <code>NonEmptySet</code>
    * @param other the <code>NonEmptySet</code> to append
    * @return a new <code>NonEmptySet</code> that contains all the elements of this <code>NonEmptySet</code> followed by all elements of <code>other</code>.
    */
  def ++[U >: T](other: NonEmptySet[U]): NonEmptySet[U] = new NonEmptySet(toSet ++ other.toSet)

  /**
    * Returns a new <code>NonEmptySet</code> containing the elements of this <code>NonEmptySet</code> followed by the elements of the passed <code>Every</code>.
    * The element type of the resulting <code>NonEmptySet</code> is the most specific superclass encompassing the element types of this <code>NonEmptySet</code> and the passed <code>Every</code>.
    *
    * @tparam U the element type of the returned <code>NonEmptySet</code>
    * @param other the <code>Every</code> to append
    * @return a new <code>NonEmptySet</code> that contains all the elements of this <code>NonEmptySet</code> followed by all elements of <code>other</code>.
    */
  def ++[U >: T](other: Every[U]): NonEmptySet[U] = new NonEmptySet(toSet ++ other.toVector)

  // TODO: Have I added these extra ++, etc. methods to Every that take a NonEmptySet?

  /**
    * Returns a new <code>NonEmptySet</code> containing the elements of this <code>NonEmptySet</code> followed by the elements of the passed <code>IterableOnce</code>.
    * The element type of the resulting <code>NonEmptySet</code> is the most specific superclass encompassing the element types of this <code>NonEmptySet</code>
    * and the passed <code>IterableOnce</code>.
    *
    * @param other the <code>IterableOnce</code> to append
    * @return a new <code>NonEmptySet</code> that contains all the elements of this <code>NonEmptySet</code> followed by all elements of <code>other</code>.
    */
  def ++(other: org.scalactic.ColCompatHelper.IterableOnce[T]): NonEmptySet[T] =
    if (other.isEmpty) this else new NonEmptySet(toSet ++ other.toSet)

  /**
    * Returns a new <code>NonEmptySet</code> with the given element added.
    *
    *
    * @param element the element to add to this <code>NonEmptySet</code>
    * @return a new <code>NonEmptySet</code> consisting of <code>element</code> and all elements of this <code>NonEmptySet</code>.
    */
  final def +(element: T): NonEmptySet[T] = new NonEmptySet(toSet + element)

  /**
    * Returns a new <code>NonEmptySet</code> with the given element added.
    *
    * @param elem1 the first element to add.
    * @param elem2 the second element to add.
    * @param elems the remaining elements to add.
    * @return   a new <code>NonEmptySet</code> with the given elements added, omitting duplicates.
    */
  final def +(elem1: T, elem2: T, elems: T*): NonEmptySet[T] = new NonEmptySet(toSet + elem1 + elem2 ++ elems)

  /**
    * Appends all elements of this <code>NonEmptySet</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
    * on of every element of this <code>NonEmptySet</code>, without any separator string.
    *
    * @param sb the string builder to which elements will be appended
    * @return the string builder, <code>sb</code>, to which elements were appended.
    */
  final def addString(sb: StringBuilder): StringBuilder = toSet.addString(sb)

  /**
    * Check if an element exists at its index in the <code>NonEmptySet</code>.
    *
    * @return <code>true</code> if a element exists in <code>NonEmptySet</code> at index <code>idx</code>, where <code>false</code> indicates the element at index <code>idx</code> does not exist.
    */
  final def apply(elem: T): Boolean = toSet(elem)

  /**
    * Appends all elements of this <code>NonEmptySet</code> to a string builder using a separator string. The written text will consist of a concatenation of the
    * result of invoking <code>toString</code>
    * on of every element of this <code>NonEmptySet</code>, separated by the string <code>sep</code>.
    *
    * @param sb the string builder to which elements will be appended
    * @param sep the separator string
    * @return the string builder, <code>sb</code>, to which elements were appended.
    */
  final def addString(sb: StringBuilder, sep: String): StringBuilder = toSet.addString(sb, sep)

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
  final def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = toSet.addString(sb, start, sep, end)

  /**
    * Finds the first element of this <code>NonEmptySet</code> for which the given partial function is defined, if any, and applies the partial function to it.
    *
    * @param pf the partial function
    * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
    *    the partial function was not defined for any element.
    */
  final def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = toSet.collectFirst(pf)

  /**
    * Indicates whether this <code>NonEmptySet</code> contains a given value as an element.
    *
    * @param elem the element to look for
    * @return true if this <code>NonEmptySet</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
    */
  final def contains(elem: T): Boolean = toSet.contains(elem)

  /**
    * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptySet</code>. Copying
    * will stop once either the end of the current <code>NonEmptySet</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    */
  final def copyToArray[U >: T](arr: Array[U]): Unit = toSet.copyToArray(arr)

  /**
    * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptySet</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptySet</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    * @param start the starting index
    */
  final def copyToArray[U >: T](arr: Array[U], start: Int): Unit = toSet.copyToArray(arr, start)

  /**
    * Copies values of this <code>NonEmptySet</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptySet</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptySet</code> is reached, the end of the array is reached, or
    * <code>len</code> elements have been copied.
    *
    * @param arr the array to fill
    * @param start the starting index
    * @param len the maximum number of elements to copy
    */
  final def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = toSet.copyToArray(arr, start, len)

  /**
    * Copies all elements of this <code>NonEmptySet</code> to a buffer. 
    *
    * @param buf the buffer to which elements are copied
    */
  final def copyToBuffer[U >: T](buf: Buffer[U]): Unit = toSet.copyToBuffer(buf)

  /**
    * Counts the number of elements in this <code>NonEmptySet</code> that satisfy a predicate. 
    *
    * @param p the predicate used to test elements.
    * @return the number of elements satisfying the predicate <code>p</code>. 
    */
  final def count(p: T => Boolean): Int = toSet.count(p)

  /**
    * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptySet</code>.
    *
    * @param p the predicate used to test elements.
    * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptySet</code>, otherwise <code>false</code>. 
    */
  final def exists(p: T => Boolean): Boolean = toSet.exists(p)

  /**
    * Finds the first element of this <code>NonEmptySet</code> that satisfies the given predicate, if any.
    *
    * @param p the predicate used to test elements
    * @return an <code>Some</code> containing the first element in this <code>NonEmptySet</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
    */
  final def find(p: T => Boolean): Option[T] = toSet.find(p)

  /**
    * Builds a new <code>NonEmptySet</code> by applying a function to all elements of this <code>NonEmptySet</code> and using the elements of the resulting <code>NonEmptySet</code>s.
    *
    * @tparam U the element type of the returned <code>NonEmptySet</code>
    * @param f the function to apply to each element.
    * @return a new <code>NonEmptySet</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptySet</code> and concatenating
    *    the elements of resulting <code>NonEmptySet</code>s. 
    */
  final def flatMap[U](f: T => NonEmptySet[U]): NonEmptySet[U] = {
    val buf = new ArrayBuffer[U]
    for (ele <- toSet)
      buf ++= f(ele).toSet
    new NonEmptySet(buf.toSet)
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
  final def flatten[B](implicit ev: T <:< NonEmptySet[B]): NonEmptySet[B] = flatMap(ev)

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
  final def fold[U >: T](z: U)(op: (U, U) => U): U = toSet.fold(z)(op)

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
  final def foldLeft[B](z: B)(op: (B, T) => B): B = toSet.foldLeft(z)(op)

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
  final def foldRight[B](z: B)(op: (T, B) => B): B = toSet.foldRight(z)(op)

  /**
    * Indicates whether a predicate holds for all elements of this <code>NonEmptySet</code>.
    *
    * @param p the predicate used to test elements.
    * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptySet</code>, otherwise <code>false</code>. 
    */
  final def forall(p: T => Boolean): Boolean = toSet.forall(p)

  /**
    * Applies a function <code>f</code> to all elements of this <code>NonEmptySet</code>.
    *
    * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
    */
  final def foreach(f: T => Unit): Unit = toSet.foreach(f)

  /**
    * Partitions this <code>NonEmptySet</code> into a map of <code>NonEmptySet</code>s according to some discriminator function.
    *
    * @tparam K the type of keys returned by the discriminator function.
    * @param f the discriminator function.
    * @return A map from keys to <code>NonEmptySet</code>s such that the following invariant holds:
    *
    * <pre>
    * (NonEmptySet.toSet partition f)(k) = xs filter (x =&gt; f(x) == k)
    * </pre>
    *
    * <p>
    * That is, every key <code>k</code> is bound to a <code>NonEmptySet</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
    * </p>
    */
  final def groupBy[K](f: T => K): Map[K, NonEmptySet[T]] = {
    val mapKToSet = toSet.groupBy(f)
    mapKToSet.mapValues { Set => new NonEmptySet(Set) }.toMap
  }

  /**
    * Partitions elements into fixed size <code>NonEmptySet</code>s.
    *
    * @param size the number of elements per group
    * @return An iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
    */
  final def grouped(size: Int): Iterator[NonEmptySet[T]] = {
    val itOfSet = toSet.grouped(size)
    itOfSet.map { Set => new NonEmptySet(Set) }
  }

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptySet</code> has a definite size, since all <code>NonEmptySet</code>s are strict collections.
    */
  final def hasDefiniteSize: Boolean = true

  // override def hashCode: Int = toSet.hashCode

  /**
    * Selects the first element of this <code>NonEmptySet</code>. 
    *
    * @return the first element of this <code>NonEmptySet</code>.
    */
  final def head: T = toSet.head

  // Methods like headOption I can't get rid of because of the implicit conversion to Iterable.
  // Users can call any of the methods I've left out on a NonEmptySet, and get whatever Set would return
  // for that method call. Eventually I'll probably implement them all to save the implicit conversion.

  /**
    * Selects the first element of this <code>NonEmptySet</code> and returns it wrapped in a <code>Some</code>. 
    *
    * @return the first element of this <code>NonEmptySet</code>, wrapped in a <code>Some</code>.
    */
  final def headOption: Option[T] = toSet.headOption

  /**
    * Returns <code>false</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, is non-empty.
    *
    * @return false
    */
  final def isEmpty: Boolean = false

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, can be traversed repeatedly.
    *
    * @return true
    */
  final def isTraversableAgain: Boolean = true

  /**
    * Creates and returns a new iterator over all elements contained in this <code>NonEmptySet</code>.
    *
    * @return the new iterator
    */
  final def iterator: Iterator[T] = toSet.iterator

  /**
    * Selects the last element of this <code>NonEmptySet</code>. 
    *
    * @return the last element of this <code>NonEmptySet</code>.
    */
  final def last: T = toSet.last

  /**
    * Returns the last element of this <code>NonEmptySet</code>, wrapped in a <code>Some</code>. 
    *
    * @return the last element, wrapped in a <code>Some</code>. 
    */
  final def lastOption: Option[T] = toSet.lastOption // Will always return a Some

  /**
    * Builds a new <code>NonEmptySet</code> by applying a function to all elements of this <code>NonEmptySet</code>.
    *
    * @tparam U the element type of the returned <code>NonEmptySet</code>.
    * @param f the function to apply to each element. 
    * @return a new <code>NonEmptySet</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptySet</code> and collecting the results. 
    */
  final def map[U](f: T => U): NonEmptySet[U] =
    new NonEmptySet(toSet.map(f))

  /**
    * Finds the largest element.
    *
    * @return the largest element of this <code>NonEmptySet</code>. 
    */
  final def max[U >: T](implicit cmp: Ordering[U]): T = toSet.max(cmp)

  /**
    * Finds the largest result after applying the given function to every element.
    *
    * @return the largest result of applying the given function to every element of this <code>NonEmptySet</code>. 
    */
  final def maxBy[U](f: T => U)(implicit cmp: Ordering[U]): T = toSet.maxBy(f)(cmp)

  /**
    * Finds the smallest element.
    *
    * @return the smallest element of this <code>NonEmptySet</code>. 
    */
  final def min[U >: T](implicit cmp: Ordering[U]): T = toSet.min(cmp)

  /**
    * Finds the smallest result after applying the given function to every element.
    *
    * @return the smallest result of applying the given function to every element of this <code>NonEmptySet</code>. 
    */
  final def minBy[U](f: T => U)(implicit cmp: Ordering[U]): T = toSet.minBy(f)(cmp)

  /**
    * Displays all elements of this <code>NonEmptySet</code> in a string. 
    *
    * @return a string representation of this <code>NonEmptySet</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
    *     <code>NonEmptySet</code> follow each other without any separator string. 
    */
  final def mkString: String = toSet.mkString

  /**
    * Displays all elements of this <code>NonEmptySet</code> in a string using a separator string. 
    *
    * @param sep the separator string
    * @return a string representation of this <code>NonEmptySet</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
    *     <code>NonEmptySet</code> are separated by the string <code>sep</code>. 
    */
  final def mkString(sep: String): String = toSet.mkString(sep)

  /**
    * Displays all elements of this <code>NonEmptySet</code> in a string using start, end, and separator strings. 
    *
    * @param start the starting string.
    * @param sep the separator string.
    * @param end the ending string.
    * @return a string representation of this <code>NonEmptySet</code>. The resulting string begins with the string <code>start</code> and ends with the string
    *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>NonEmptySet</code> are
    *     separated by the string <code>sep</code>. 
    */
  final def mkString(start: String, sep: String, end: String): String = toSet.mkString(start, sep, end)

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, is non-empty.
    *
    * @return true
    */
  final def nonEmpty: Boolean = true

  /**
    * The result of multiplying all the elements of this <code>NonEmptySet</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptySet[T]</code> for which an implicit <code>Numeric[T]</code> exists.
    * </p>
    *
    * @return the product of all elements
    */
  final def product[U >: T](implicit num: Numeric[U]): U = toSet.product(num)

  /**
    * Reduces the elements of this <code>NonEmptySet</code> using the specified associative binary operator.
    *
    * <p>
    * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
    * </p>
    *
    * @tparam U a type parameter for the binary operator, a supertype of T.
    * @param op a binary operator that must be associative.
    * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptySet</code>.
    */
  final def reduce[U >: T](op: (U, U) => U): U = toSet.reduce(op)

  /**
    * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going left to right.
    *
    * @tparam U the result type of the binary operator.
    * @param op the binary operator.
    * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>, going left to right:
    *
    * <pre>
    * op(...op(op(x_1, x_2), x_3), ..., x_n)
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptySet</code>. 
    * </p>
    */
  final def reduceLeft[U >: T](op: (U, T) => U): U = toSet.reduceLeft(op)

  /**
    * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going left to right, returning the result in a <code>Some</code>.
    *
    * @tparam U the result type of the binary operator.
    * @param op the binary operator.
    * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
    * </p>
    */
  final def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = toSet.reduceLeftOption(op)

  final def reduceOption[U >: T](op: (U, U) => U): Option[U] = toSet.reduceOption(op)

  /**
    * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going right to left.
    *
    * @tparam U the result of the binary operator
    * @param op the binary operator
    * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>, going right to left:
    *
    * <pre>
    * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptySet</code>. 
    * </p>
    */
  final def reduceRight[U >: T](op: (T, U) => U): U = toSet.reduceRight(op)

  /**
    * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going right to left, returning the result in a <code>Some</code>.
    *
    * @tparam U the result of the binary operator
    * @param op the binary operator
    * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
    */
  final def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = toSet.reduceRightOption(op)

  /**
    * Checks if the given <code>Iterable</code> contains the same elements in the same order as this <code>NonEmptySet</code>.
    *
    * @param that the <code>Iterable</code> with which to compare
    * @return <code>true</code>, if both this <code>NonEmptySet</code> and the given <code>Iterable</code> contain the same elements
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[U >: T](that: Iterable[U]): Boolean = toSet.sameElements(that)

  /**
    * Checks if the given <code>Every</code> contains the same elements in the same order as this <code>NonEmptySet</code>.
    *
    * @param that the <code>Every</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>Every</code> contain the same elements
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[U >: T](that: Every[U]): Boolean = toSet.sameElements(that.toVector)

  /**
    * Checks if the given <code>NonEmptySet</code> contains the same elements in the same order as this <code>NonEmptySet</code>.
    *
    * @param that the <code>NonEmptySet</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>NonEmptySet</code> contain the same elements
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[U >: T](that: NonEmptySet[U]): Boolean = toSet.sameElements(that.toSet)

  /**
    * Computes a prefix scan of the elements of this <code>NonEmptySet</code>.
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
    * NonEmptySet(1, 2, 3).scan(0)(_ + _) == NonEmptySet(0, 1, 3, 6)
    * NonEmptySet(1, 2, 3).scan("z")(_ + _.toString) == NonEmptySet("z", "z1", "z12", "z123")
    * </pre>
    *
    * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>NonEmptySet</code>.
    * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
    *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for Set concatenation,
    *     0 for addition, or 1 for multiplication.)
    * @param op a binary operator that must be associative
    * @return a new <code>NonEmptySet</code> containing the prefix scan of the elements in this <code>NonEmptySet</code> 
    */
  final def scan[U >: T](z: U)(op: (U, U) => U): NonEmptySet[U] = new NonEmptySet(toSet.scan(z)(op))

  /**
    * Produces a <code>NonEmptySet</code> containing cumulative results of applying the operator going left to right.
    *
    * <p>
    * Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptySet(1, 2, 3).scanLeft(0)(_ + _) == NonEmptySet(0, 1, 3, 6)
    * NonEmptySet(1, 2, 3).scanLeft("z")(_ + _) == NonEmptySet("z", "z1", "z12", "z123")
    * </pre>
    *
    * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptySet</code>
    * @param z the start value.
    * @param op the binary operator.
    * @return a new <code>NonEmptySet</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>,
    *     going left to right, with the start value, <code>z</code>, on the left.
    */
  final def scanLeft[B](z: B)(op: (B, T) => B): NonEmptySet[B] = new NonEmptySet(toSet.scanLeft(z)(op))

  /**
    * Produces a <code>NonEmptySet</code> containing cumulative results of applying the operator going right to left.
    *
    * <p>
    * Here are some examples:
    * </p>
    *
    * <pre class="stHighlight">
    * NonEmptySet(1, 2, 3).scanRight(0)(_ + _) == NonEmptySet(6, 5, 3, 0)
    * NonEmptySet(1, 2, 3).scanRight("z")(_ + _) == NonEmptySet("123z", "23z", "3z", "z")
    * </pre>
    *
    * @tparam B the result of the binary operator and type of the resulting <code>NonEmptySet</code>
    * @param z the start value
    * @param op the binary operator
    * @return a new <code>NonEmptySet</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptySet</code>,
    *     going right to left, with the start value, <code>z</code>, on the right.
    */
  final def scanRight[B](z: B)(op: (T, B) => B): NonEmptySet[B] = new NonEmptySet(toSet.scanRight(z)(op))

  /**
    * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
    *
    * @param size the number of elements per group
    * @return an iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last and the only element will be truncated
    *     if there are fewer elements than <code>size</code>.
    */
  final def sliding(size: Int): Iterator[NonEmptySet[T]] = toSet.sliding(size).map(new NonEmptySet(_))

  /**
    * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
    * moving the sliding window by a given <code>step</code> each time.
    *
    * @param size the number of elements per group
    * @param step the distance between the first elements of successive groups
    * @return an iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last and the only element will be truncated
    *     if there are fewer elements than <code>size</code>.
    */
  final def sliding(size: Int, step: Int): Iterator[NonEmptySet[T]] = toSet.sliding(size, step).map(new NonEmptySet(_))

  /**
    * The size of this <code>NonEmptySet</code>.
    *
    * <p>
    * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
    * </p>
    *
    * @return the number of elements in this <code>NonEmptySet</code>. 
    */
  final def size: Int = toSet.size

  /**
    * Returns <code>"NonEmptySet"</code>, the prefix of this object's <code>toString</code> representation.
    *
    * @return the string <code>"NonEmptySet"</code>
    */
  def stringPrefix: String = "NonEmptySet"

  /**
    * The result of summing all the elements of this <code>NonEmptySet</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptySet[T]</code> for which an implicit <code>Numeric[T]</code> exists.
    * </p>
    *
    * @return the sum of all elements
    */
  final def sum[U >: T](implicit num: Numeric[U]): U = toSet.sum(num)

  import scala.language.higherKinds

  /**
    * Converts this <code>NonEmptySet</code> into a collection of type <code>Col</code> by copying all elements.
    *
    * @tparam Col the collection type to build.
    * @return a new collection containing all elements of this <code>NonEmptySet</code>. 
    */
  final def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] =
    toSet.to(factory)

  /**
    * Converts this <code>NonEmptySet</code> to an array.
    *
    * @return an array containing all elements of this <code>NonEmptySet</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptySet</code>. 
    */
  final def toArray[U >: T](implicit classTag: ClassTag[U]): Array[U] = toSet.toArray

  /**
    * Converts this <code>NonEmptySet</code> to a <code>Vector</code>.
    *
    * @return a <code>Vector</code> containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toVector: Vector[T] = toSet.toVector

  /**
    * Converts this <code>NonEmptySet</code> to a mutable buffer.
    *
    * @return a buffer containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toBuffer[U >: T]: Buffer[U] = toSet.toBuffer

  /**
    * Converts this <code>NonEmptySet</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toIndexedSeq: collection.immutable.IndexedSeq[T] = toSet.toVector

  /**
    * Converts this <code>NonEmptySet</code> to an iterable collection.
    *
    * @return an <code>Iterable</code> containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toIterable: Iterable[T] = toSet.toIterable

  /**
    * Returns an <code>Iterator</code> over the elements in this <code>NonEmptySet</code>.
    *
    * @return an <code>Iterator</code> containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toIterator: Iterator[T] = toSet.toIterator

  /**
    * Converts this <code>NonEmptySet</code> to a Set.
    *
    * @return a Set containing all elements of this <code>NonEmptySet</code>. 
    */
  // final def toSet: Set[T] = toSet

  /**
    * Converts this <code>NonEmptySet</code> to a map.
    *
    * <p>
    * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
    * in the map. Duplicate keys will be overwritten by later keys.
    * </p>
    *
    * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>NonEmptySet</code>. 
    */
  final def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = toSet.toMap

  /**
    * Converts this <code>NonEmptySet</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptySet</code>.
    */
  final def toSeq: Seq[T] = toSet.toSeq

  /**
    * Converts this <code>NonEmptySet</code> to a set.
    *
    * @return a set containing all elements of this <code>NonEmptySet</code>.
    */
  final def toList: collection.immutable.List[T] = toSet.toList

  /**
    * Converts this <code>NonEmptySet</code> to a stream.
    *
    * @return a stream containing all elements of this <code>NonEmptySet</code>. 
    */
  final def toStream: Stream[T] = toSet.toStream

  /**
    * Returns a string representation of this <code>NonEmptySet</code>.
    *
    * @return the string <code>"NonEmptySet"</code> followed by the result of invoking <code>toString</code> on
    *   this <code>NonEmptySet</code>'s elements, surrounded by parentheses.
    */
  override def toString: String = "NonEmptySet(" + toSet.mkString(", ") + ")"

  final def transpose[U](implicit ev: T <:< NonEmptySet[U]): NonEmptySet[NonEmptySet[U]] = {
    val asSets = toSet.map(ev)
    val Set = asSets.transpose
    new NonEmptySet(Set.map(new NonEmptySet(_)))
  }

  /**
    * Produces a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> and also all elements of a given <code>Every</code>.
    *
    * <p>
    * <code>NonEmptySetX</code> <code>union</code> <code>everyY</code> is equivalent to <code>NonEmptySetX</code> <code>++</code> <code>everyY</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>NonEmptySetX</code> <code>union</code> <code>everyY</code> computes the order-presevring multi-set union
    * of <code>NonEmptySetX</code> and <code>everyY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>Every</code> to add.
    * @return a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> followed by all elements of <code>that</code> <code>Every</code>.
    */
  final def union(that: Every[T]): NonEmptySet[T] = new NonEmptySet(toSet union that.toSet)

  /**
    * Produces a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> and also all elements of a given <code>NonEmptySet</code>.
    *
    * <p>
    * <code>NonEmptySetX</code> <code>union</code> <code>NonEmptySetY</code> is equivalent to <code>NonEmptySetX</code> <code>++</code> <code>NonEmptySetY</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>NonEmptySetX</code> <code>union</code> <code>NonEmptySetY</code> computes the order-presevring multi-set union
    * of <code>NonEmptySetX</code> and <code>NonEmptySetY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>NonEmptySet</code> to add.
    * @return a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> followed by all elements of <code>that</code>.
    */
  final def union(that: NonEmptySet[T]): NonEmptySet[T] = new NonEmptySet(toSet union that.toSet)

  /**
    * Produces a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> and also all elements of a given <code>GenSeq</code>.
    *
    * <p>
    * <code>NonEmptySetX</code> <code>union</code> <code>ys</code> is equivalent to <code>NonEmptySetX</code> <code>++</code> <code>ys</code>.
    * </p>
    *
    * <p>
    * Another way to express this is that <code>NonEmptySetX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
    * of <code>NonEmptySetX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
    * also work on multi-sets.
    * </p>
    *
    * @param that the <code>GenSet</code> to add.
    * @return a new <code>NonEmptySet</code> that contains all elements of this <code>NonEmptySet</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
    */
  final def union(that: GenSet[T]): NonEmptySet[T] = new NonEmptySet(toSet.union(that))

  /**
    * Converts this <code>NonEmptySet</code> of pairs into two <code>NonEmptySet</code>s of the first and second half of each pair. 
    *
    * @tparam L the type of the first half of the element pairs
    * @tparam R the type of the second half of the element pairs
    * @param asPair an implicit conversion that asserts that the element type of this <code>NonEmptySet</code> is a pair.
    * @return a pair of <code>NonEmptySet</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptySet</code>. 
    */
  final def unzip[L, R](implicit asPair: T => (L, R)): (NonEmptySet[L], NonEmptySet[R]) = {
    val unzipped = toSet.unzip
    (new NonEmptySet(unzipped._1), new NonEmptySet(unzipped._2))
  }

  /**
    * Converts this <code>NonEmptySet</code> of triples into three <code>NonEmptySet</code>s of the first, second, and and third element of each triple. 
    *
    * @tparam L the type of the first member of the element triples
    * @tparam M the type of the second member of the element triples
    * @tparam R the type of the third member of the element triples
    * @param asTriple an implicit conversion that asserts that the element type of this <code>NonEmptySet</code> is a triple.
    * @return a triple of <code>NonEmptySet</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptySet</code>. 
    */
  final def unzip3[L, M, R](implicit asTriple: T => (L, M, R)): (NonEmptySet[L], NonEmptySet[M], NonEmptySet[R]) = {
    val unzipped = toSet.unzip3
    (new NonEmptySet(unzipped._1), new NonEmptySet(unzipped._2), new NonEmptySet(unzipped._3))
  }

  /**
    * Returns a <code>NonEmptySet</code> formed from this <code>NonEmptySet</code> and an iterable collection by combining corresponding
    * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
    * shorter collection to the length of the longer.
    *
    * @tparm O the type of the second half of the returned pairs
    * @tparm U the type of the first half of the returned pairs
    * @param other the <code>Iterable</code> providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this <code>NonEmptySet</code> is shorter than <code>that</code> <code>Iterable</code>.
    * @param otherElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptySet</code>.
    * @return a new <code>NonEmptySet</code> containing pairs consisting of corresponding elements of this <code>NonEmptySet</code> and <code>that</code>. The
    *     length of the returned collection is the maximum of the lengths of this <code>NonEmptySet</code> and <code>that</code>. If this <code>NonEmptySet</code>
    *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
    *     <code>NonEmptySet</code>, <code>thatElem</code> values are used to pad the result. 
    */
  final def zipAll[O, U >: T](other: collection.Iterable[O], thisElem: U, otherElem: O): NonEmptySet[(U, O)] =
    new NonEmptySet(toSet.zipAll(other, thisElem, otherElem))

  /**
    * Zips this <code>NonEmptySet</code>  with its indices.
    *
    * @return A new <code>NonEmptySet</code> containing pairs consisting of all elements of this <code>NonEmptySet</code> paired with their index. Indices start at 0.
    */
  final def zipWithIndex: NonEmptySet[(T, Int)] = new NonEmptySet(toSet.zipWithIndex)
}

/**
  * Companion object for class <code>NonEmptySet</code>.
  */
object NonEmptySet {

  /**
    * Constructs a new <code>NonEmptySet</code> given at least one element.
    *
    * @tparam T the type of the element contained in the new <code>NonEmptySet</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptySet</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptySet</code>
    */
  def apply[T](firstElement: T, otherElements: T*): NonEmptySet[T] = new NonEmptySet(otherElements.toSet + firstElement)

  /**
    * Variable argument extractor for <code>NonEmptySet</code>s.
    *
    * @param nonEmptySet: the <code>NonEmptySet</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptySet</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[T](nonEmptySet: NonEmptySet[T]): Option[Seq[T]] = Some(nonEmptySet.toSeq)

  /*
    // TODO: Figure out how to get case NonEmptySet() to not compile
    def unapplySeq[T](NonEmptySet: NonEmptySet[T]): Option[(T, Seq[T])] = Some(NonEmptySet.head, NonEmptySet.tail)
  */

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
      case Some(first) => Some(new NonEmptySet(scala.collection.immutable.Set.empty[T] ++ set))
    }

  import scala.language.implicitConversions

  /**
    * Implicit conversion from <code>NonEmptySet</code> to <code>Set</code>.
    *
    * <p>
    * One use case for this implicit conversion is to enable <code>GenSeq[NonEmptySet]</code>s to be flattened.
    * Here's an example:
    * </p>
    *
    * <pre class="stREPL">
    * scala&gt; Vector(NonEmptySet(1, 2, 3), NonEmptySet(3, 4), NonEmptySet(5, 6, 7, 8)).flatten
    * res0: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 3, 4, 5, 6, 7, 8)
    * </pre>
    *
    * @param NonEmptySet the <code>NonEmptySet</code> to convert to a <code>Set</code>
    * @return a <code>Set</code> containing the elements, in order, of this <code>NonEmptySet</code>
    */
  implicit def NonEmptySetToSet[E](NonEmptySet: NonEmptySet[E]): scala.collection.immutable.Set[E] = NonEmptySet.toSet
}
