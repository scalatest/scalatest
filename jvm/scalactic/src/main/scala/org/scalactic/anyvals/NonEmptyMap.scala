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
import scala.collection.GenSeq
import org.scalactic.ColCompatHelper.{IterableOnce, Iterable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import org.scalactic.Every


// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
/**
  * A non-empty map: an ordered, immutable, non-empty collection of key-value tuples with <code>LinearSeq</code> performance characteristics.
  *
  * <p>
  * The purpose of <code>NonEmptyMap</code> is to allow you to express in a type that a <code>Map</code> is non-empty, thereby eliminating the
  * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
  * performance, see <a href="Every.html"><code>Every</code></a>.
  * </p>
  *
  * <h2>Constructing <code>NonEmptyMap</code>s</h2>
  *
  * <p>
  * You can construct a <code>NonEmptyMap</code> by passing one or more elements to the <code>NonEmptyMap.apply</code> factory method:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")
  * res0: org.scalactic.anyvals.NonEmptyMap[Int, String] = NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")
  * </pre>
  *
  * <h2>Working with <code>NonEmptyMap</code>s</h2>
  *
  * <p>
  * <code>NonEmptyMap</code> does not extend Scala's <code>Map</code> or <code>Traversable</code> traits because these require that
  * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
  * you'll get an empty <code>Seq</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Map(1 -> "one").tail
  * res6: Map[Int] = Map()
  * </pre>
  *
  * <p>
  * On the other hand, many useful methods exist on <code>Map</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
  * to not result in an empty <code>Map</code>. For convenience, <code>NonEmptyMap</code> defines a method corresponding to every such <code>Map</code>
  * method. Here are an example:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").map(t => (t._1 + 1, t._2))                        // Result: NonEmptyMap(2 -> "one", 3 -> "two", 4 -> "three")
  * </pre>
  *
  * <p>
  * <code>NonEmptyMap</code> does <em>not</em> currently define any methods corresponding to <code>Map</code> methods that could result in
  * an empty <code>Map</code>. However, an implicit converison from <code>NonEmptyMap</code> to <code>Map</code>
  * is defined in the <code>NonEmptyMap</code> companion object that will be applied if you attempt to call one of the missing methods. As a
  * result, you can invoke <code>filter</code> on an <code>NonEmptyMap</code>, even though <code>filter</code> could result
  * in an empty map&mdash;but the result type will be <code>Map</code> instead of <code>NonEmptyMap</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").filter(_._1 &lt; 10) // Result: Map(1 -> "one", 2 -> "two", 3 -> "three")
  * NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three").filter(_._ 1&gt; 10) // Result: Map()
  * </pre>
  *
  *
  * <p>
  * You can use <code>NonEmptyMap</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyMap</code> unless
  * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
  * result type will switch to a <code>Map</code> at that point. Here are some examples:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; import org.scalactic.anyvals._
  * import org.scalactic.anyvals._
  *
  * scala&gt; for ((i, j) &lt;- NonEmptyMap(1 -> "one", 2 -> "two", 3 -> "three")) yield (i + 1, j)
  * res0: org.scalactic.anyvals.NonEmptyMap[Int, String] = NonEmptyMap(2 -> "one", 3 -> "two", 4 -> "three")
  *
  * scala&gt; for ((i, j) &lt;- NonEmptyMap(1, 2, 3) if i &lt; 10) yield (i + 1, j)
  * res1: Map[Int, String] = Map(2 -> "one", 3 -> "two", 4 -> "three")
  * </pre>
  *
  * @tparam K the type of key contained in this <code>NonEmptyMap</code>
  * @tparam V the type of value contained in this <code>NonEmptyMap</code>
  */
final class NonEmptyMap[K, +V] private (val toMap: Map[K, V]) extends AnyVal {

  /**
    * Returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>NonEmptyMap</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this and the passed <code>NonEmptyMap</code>.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>NonEmptyMap</code> to append
    * @return a new <code>NonEmptyMap</code> that contains all the elements of this <code>NonEmptyMap</code> and all elements of <code>other</code>.
    */
  def ++[V1 >: V](other: NonEmptyMap[K, V1]): NonEmptyMap[K, V1] = new NonEmptyMap(toMap ++ other.toMap)

  /**
    * Returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>Every</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this <code>NonEmptyMap</code> and the passed <code>Every</code>.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>Every</code> to append
    * @return a new <code>NonEmptyMap</code> that contains all the entries of this <code>NonEmptyMap</code> and all elements of <code>other</code>.
    */
  def ++[V1 >: V](other: Every[(K, V1)]): NonEmptyMap[K, V1] = new NonEmptyMap(toMap ++ other.toVector)

  // TODO: Have I added these extra ++, etc. methods to Every that take a NonEmptyMap?

  /**
    * Returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>IterableOnce</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this <code>NonEmptyMap</code>
    * and the passed <code>IterableOnce</code>.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>IterableOnce</code> to append
    * @return a new <code>NonEmptyMap</code> that contains all the elements of this <code>NonEmptyMap</code> followed by all elements of <code>other</code>.
    */
  def ++[V1 >: V](other: org.scalactic.ColCompatHelper.IterableOnce[(K, V1)]): NonEmptyMap[K, V1] =
    if (other.isEmpty) this else new NonEmptyMap(toMap ++ other.toMap)

  /**
    * Returns a new <code>NonEmptyMap</code> with the given entry added.
    *
    * <p>
    * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
    * </p>
    *
    * @param entry the element to add to this <code>NonEmptyMap</code>
    * @return a new <code>NonEmptyMap</code> consisting of <code>element</code> followed by all elements of this <code>NonEmptyMap</code>.
    */
  final def +:[V1 >: V](entry: (K, V1)): NonEmptyMap[K, V1] = new NonEmptyMap(toMap + entry)

  /**
    * As with <code>++</code>, returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>NonEmptyMap</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this and the passed <code>NonEmptyMap</code>.
    *
    * It differs from <code>++</code> in that the right operand determines the type of the resulting collection rather than the left one. Mnemonic: the COLon is on the side of the new COLlection type.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>NonEmptyMap</code> to add
    * @return a new <code>NonEmptyMap</code> that contains all the elements of this <code>NonEmptyMap</code> and all elements of <code>other</code>.
    */
  def ++:[V1 >: V](other: NonEmptyMap[K, V1]): NonEmptyMap[K, V1] = new NonEmptyMap(toMap ++ other.toMap)

  /**
    * As with <code>++</code>, returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>Every</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this <code>NonEmptyMap</code> and the passed <code>Every</code>.
    *
    * It differs from <code>++</code> in that the right operand determines the type of the resulting collection rather than the left one. Mnemonic: the COLon is on the side of the new COLlection type.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>Every</code> to append
    * @return a new <code>NonEmptyMap</code> that contains all the entries of this <code>NonEmptyMap</code> and all elements of <code>other</code>.
    */
  def ++:[V1 >: V](other: Every[(K, V1)]): NonEmptyMap[K, V1] = new NonEmptyMap(toMap ++ other.toVector)

  /**
    * Returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>IterableOnce</code>.
    * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this <code>NonEmptyMap</code>
    * and the passed <code>IterableOnce</code>.
    *
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param other the <code>IterableOnce</code> to append
    * @return a new <code>NonEmptyMap</code> that contains all the elements of this <code>NonEmptyMap</code> followed by all elements of <code>other</code>.
    */
  def ++:[V1 >: V](other: org.scalactic.ColCompatHelper.IterableOnce[(K, V1)]): NonEmptyMap[K, V1] =
    if (other.isEmpty) this else new NonEmptyMap(toMap ++ other.toMap)

  /**
    * Returns a new <code>NonEmptyMap</code> with the given entry added.
    *
    * @param entry the entry to add to this <code>NonEmptyMap</code>
    * @return a new <code>NonEmptyMap</code> consisting of all entries of this <code>NonEmptyMap</code> and <code>entry</code>.
    */
  def +[V1 >: V](entry: (K, V1)): NonEmptyMap[K, V1] = new NonEmptyMap(toMap + entry)

  /**
    * Returns a new <code>NonEmptyMap</code> with the given entries added.
    *
    * @param entries the entries to add to this <code>NonEmptyMap</code>
    * @return a new <code>NonEmptyMap</code> consisting of all entries of this <code>NonEmptyMap</code> and <code>entries</code>.
    */
  def +[V1 >: V](entries: (K, V1)*): NonEmptyMap[K, V1] = new NonEmptyMap(toMap ++ entries)

  /**
    * Appends all entries of this <code>NonEmptyMap</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
    * on of every entry of this <code>NonEmptyMap</code>, without any separator string.
    *
    * @param sb the string builder to which entries will be appended
    * @return the string builder, <code>sb</code>, to which entries were appended.
    */
  final def addString(sb: StringBuilder): StringBuilder = toMap.addString(sb)

  /**
    * Appends all entries of this <code>NonEmptyMap</code> to a string builder using a separator string. The written text will consist of a concatenation of the
    * result of invoking <code>toString</code>
    * on of every element of this <code>NonEmptyMap</code>, separated by the string <code>sep</code>.
    *
    * @param sb the string builder to which entries will be appended
    * @param sep the separator string
    * @return the string builder, <code>sb</code>, to which elements were appended.
    */
  final def addString(sb: StringBuilder, sep: String): StringBuilder = toMap.addString(sb, sep)

  /**
    * Appends all entries of this <code>NonEmptyMap</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
    * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptyMap</code>,
    * separated by the string <code>sep</code>; and the string <code>end</code>
    *
    * @param sb the string builder to which elements will be appended
    * @param start the starting string
    * @param sep the separator string
    * @param start the ending string
    * @return the string builder, <code>sb</code>, to which elements were appended.
    */
  final def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = toMap.addString(sb, start, sep, end)

  /**
    * Selects an value by its key in the <code>NonEmptyMap</code>.
    *
    * @return the value of this <code>NonEmptyMap</code> at key <code>k</code>.
    */
  final def apply(k: K): V = toMap(k)

  /**
    * Finds the first entry of this <code>NonEmptyMap</code> for which the given partial function is defined, if any, and applies the partial function to it.
    *
    * @param pf the partial function
    * @return an <code>Option</code> containing <code>pf</code> applied to the first entry for which it is defined, or <code>None</code> if
    *    the partial function was not defined for any entry.
    */
  final def collectFirst[U](pf: PartialFunction[(K, V), U]): Option[U] = toMap.collectFirst(pf)

  /**
    * Indicates whether this <code>NonEmptyMap</code> contains a binding for given key.
    *
    * @param key the key to look for
    * @return true if this <code>NonEmptyMap</code> has a binding that is equal (as determined by <code>==)</code> to <code>key</code>, false otherwise.
    */
  final def contains(key: K): Boolean = toMap.contains(key)

  /**
    * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with entries of this <code>NonEmptyMap</code>. Copying
    * will stop once either the end of the current <code>NonEmptyMap</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    */
  final def copyToArray[V1 >: V](arr: Array[(K, V1)]): Unit = toMap.copyToArray(arr)

  /**
    * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with entries of this <code>NonEmptyMap</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyMap</code> is reached, or the end of the array is reached.
    *
    * @param arr the array to fill
    * @param start the starting index
    */
  final def copyToArray[V1 >: V](arr: Array[(K, V1)], start: Int): Unit = toMap.copyToArray(arr, start)

  /**
    * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> entries of this <code>NonEmptyMap</code>, beginning at
    * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyMap</code> is reached, the end of the array is reached, or
    * <code>len</code> elements have been copied.
    *
    * @param arr the array to fill
    * @param start the starting index
    * @param len the maximum number of elements to copy
    */
  final def copyToArray[V1 >: V](arr: Array[(K, V1)], start: Int, len: Int): Unit = toMap.copyToArray(arr, start, len)

  /**
    * Copies all elements of this <code>NonEmptyMap</code> to a buffer. 
    *
    * @param buf the buffer to which elements are copied
    */
  final def copyToBuffer[V1 >: V](buf: Buffer[(K, V1)]): Unit = toMap.copyToBuffer(buf)

  /**
    * Counts the number of elements in this <code>NonEmptyMap</code> that satisfy a predicate. 
    *
    * @param p the predicate used to test elements.
    * @return the number of elements satisfying the predicate <code>p</code>. 
    */
  final def count(p: ((K, V)) => Boolean): Int = toMap.count(p)

  /*
    override def equals(o: Any): Boolean =
      o match {
        case nonEmptyMap: NonEmptyMap[_] => toMap == nonEmptyMap.toMap
        case _ => false
      }
  */

  /**
    * Indicates whether a predicate holds for at least one of the entries of this <code>NonEmptyMap</code>.
    *
    * @param p the predicate used to test entries.
    * @return <code>true</code> if the given predicate <code>p</code> holds for some of the entries of this <code>NonEmptyMap</code>, otherwise <code>false</code>.
    */
  final def exists(p: ((K, V)) => Boolean): Boolean = toMap.exists(p)

  /**
    * Finds the first entry of this <code>NonEmptyMap</code> that satisfies the given predicate, if any.
    *
    * @param p the predicate used to test elements
    * @return an <code>Some</code> containing the first element in this <code>NonEmptyMap</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
    */
  final def find(p: ((K, V)) => Boolean): Option[(K, V)] = toMap.find(p)

  /**
    * Builds a new <code>NonEmptyMap</code> by applying a function to all entries of this <code>NonEmptyMap</code> and using the entries of the resulting <code>NonEmptyMap</code>s.
    *
    * @tparam K1 the key type of the returned <code>NonEmptyMap</code>
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
    * @param f the function to apply to each entry.
    * @return a new <code>NonEmptyMap</code> containing entries obtained by applying the given function <code>f</code> to each entry of this <code>NonEmptyMap</code> and concatenating
    *    the entries of resulting <code>NonEmptyMap</code>s.
    */
  final def flatMap[K1, V1](f: ((K, V)) => NonEmptyMap[K1, V1]): NonEmptyMap[K1, V1] = {
    val buf = new ArrayBuffer[(K1, V1)]
    for (ele <- toMap)
      buf ++= f(ele).toMap
    new NonEmptyMap(buf.toMap)
  }

  /**
    * Folds the entries of this <code>NonEmptyMap</code> using the specified associative binary operator.
    *
    * <p>
    * The order in which operations are performed on entries is unspecified and may be nondeterministic.
    * </p>
    *
    * @tparam U a type parameter for the binary operator, a supertype of (K, V).
    * @param z a neutral element for the fold operation; may be added to the result an arbitrary number of
    *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
    *     0 for addition, or 1 for multiplication.)
    * @param op a binary operator that must be associative
    * @return the result of applying fold operator <code>op</code> between all the elements and <code>z</code>
    */
  final def fold[U >: (K, V)](z: U)(op: (U, U) => U): U = toMap.fold(z)(op)

  /**
    * Applies a binary operator to a start value and all elements of this <code>NonEmptyMap</code>, going left to right.
    *
    * @tparam B the result type of the binary operator.
    * @param z the start value.
    * @param op the binary operator.
    * @return the result of inserting <code>op</code> between consecutive entries of this <code>NonEmptyMap</code>, going left to right, with the start value,
    *     <code>z</code>, on the left:
    *
    * <pre>
    * op(...op(op(z, x_1), x_2), ..., x_n)
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyMap</code>. 
    * </p>
    */
  final def foldLeft[B](z: B)(op: (B, (K, V)) => B): B = toMap.foldLeft(z)(op)

  /**
    * Applies a binary operator to all entries of this <code>NonEmptyMap</code> and a start value, going right to left.
    *
    * @tparam B the result of the binary operator
    * @param z the start value
    * @param op the binary operator
    * @return the result of inserting <code>op</code> between consecutive entries of this <code>NonEmptyMap</code>, going right to left, with the start value,
    *     <code>z</code>, on the right:
    *
    * <pre>
    * op(x_1, op(x_2, ... op(x_n, z)...))
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyMap</code>. 
    * </p>
    */
  final def foldRight[B](z: B)(op: ((K, V), B) => B): B = toMap.foldRight(z)(op)

  /**
    * Indicates whether a predicate holds for all entries of this <code>NonEmptyMap</code>.
    *
    * @param p the predicate used to test entries.
    * @return <code>true</code> if the given predicate <code>p</code> holds for all entries of this <code>NonEmptyMap</code>, otherwise <code>false</code>.
    */
  final def forall(p: ((K, V)) => Boolean): Boolean = toMap.forall(p)

  /**
    * Applies a function <code>f</code> to all entries of this <code>NonEmptyMap</code>.
    *
    * @param f the function that is applied for its side-effect to every entry. The result of function <code>f</code> is discarded.
    */
  final def foreach(f: ((K, V)) => Unit): Unit = toMap.foreach(f)

  /**
    * Partitions this <code>NonEmptyMap</code> into a map of <code>NonEmptyMap</code>s according to some discriminator function.
    *
    * @param f the discriminator function.
    * @return A map from keys to <code>NonEmptyMap</code>s such that the following invariant holds:
    *
    * <pre>
    * (nonEmptyMap.toMap partition f)(k) = xs filter (x =&gt; f(x) == k)
    * </pre>
    *
    * <p>
    * That is, every key <code>k</code> is bound to a <code>NonEmptyMap</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
    * </p>
    */
  final def groupBy(f: ((K, V)) => K): Map[K, NonEmptyMap[K, V]] = {
    val mapKToMap = toMap.groupBy(f)
    mapKToMap.mapValues { list => new NonEmptyMap(list) }.toMap
  }

  /**
    * Partitions entries into fixed size <code>NonEmptyMap</code>s.
    *
    * @param size the number of entries per group
    * @return An iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last will be truncated if the entries don't divide evenly.
    */
  final def grouped(size: Int): Iterator[NonEmptyMap[K, V]] = {
    val itOfMap = toMap.grouped(size)
    itOfMap.map { list => new NonEmptyMap(list) }
  }

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyMap</code> has a definite size, since all <code>NonEmptyMap</code>s are strict collections.
    */
  final def hasDefiniteSize: Boolean = true

  // override def hashCode: Int = toMap.hashCode

  /**
    * Selects the first element of this <code>NonEmptyMap</code>. 
    *
    * @return the first element of this <code>NonEmptyMap</code>.
    */
  final def head: (K, V) = toMap.head

  // Methods like headOption I can't get rid of because of the implicit conversion to Iterable.
  // Users can call any of the methods I've left out on a NonEmptyMap, and get whatever Map would return
  // for that method call. Eventually I'll probably implement them all to save the implicit conversion.

  /**
    * Selects the first element of this <code>NonEmptyMap</code> and returns it wrapped in a <code>Some</code>. 
    *
    * @return the first element of this <code>NonEmptyMap</code>, wrapped in a <code>Some</code>.
    */
  final def headOption: Option[(K, V)] = toMap.headOption

  /**
    * Tests whether this <code>NonEmptyMap</code> contains given key.
    *
    * @param key the key to test
    * @return true if this <code>NonEmptyMap</code> contains a binding for the given <code>key</code>, <code>false</code> otherwise.
    */
  final def isDefinedAt(key: K): Boolean = toMap.isDefinedAt(key)

  /**
    * Returns <code>false</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, is non-empty.
    *
    * @return false
    */
  final def isEmpty: Boolean = false

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, can be traversed repeatedly.
    *
    * @return true
    */
  final def isTraversableAgain: Boolean = true

  /**
    * Creates and returns a new iterator over all elements contained in this <code>NonEmptyMap</code>.
    *
    * @return the new iterator
    */
  final def iterator: Iterator[(K, V)] = toMap.iterator

  /**
    * Selects the last entry of this <code>NonEmptyMap</code>.
    *
    * @return the last entry of this <code>NonEmptyMap</code>.
    */
  final def last: (K, V) = toMap.last

  /**
    * Returns the last element of this <code>NonEmptyMap</code>, wrapped in a <code>Some</code>. 
    *
    * @return the last element, wrapped in a <code>Some</code>. 
    */
  final def lastOption: Option[(K, V)] = toMap.lastOption // Will always return a Some

  /**
    * Builds a new <code>NonEmptyMap</code> by applying a function to all entries of this <code>NonEmptyMap</code>.
    *
    * @tparam K1 the key type of the returned <code>NonEmptyMap</code>.
    * @tparam V1 the value type of the returned <code>NonEmptyMap</code>.
    * @param f the function to apply to each element. 
    * @return a new <code>NonEmptyMap</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyMap</code> and collecting the results. 
    */
  final def map[K1, V1](f: ((K, V)) => (K1, V1)): NonEmptyMap[K1, V1] =
    new NonEmptyMap(toMap.map(f))

  /**
    * Finds the largest entry.
    *
    * @return the largest entry of this <code>NonEmptyMap</code>.
    */
  final def max[U >: (K, V)](implicit cmp: Ordering[U]): (K, V) = toMap.max(cmp)

  /**
    * Finds the largest result after applying the given function to every entry.
    *
    * @return the largest result of applying the given function to every entry of this <code>NonEmptyMap</code>.
    */
  final def maxBy[U](f: ((K, V)) => U)(implicit cmp: Ordering[U]): (K, V) = toMap.maxBy(f)(cmp)

  /**
    * Finds the smallest entry.
    *
    * @return the smallest entry of this <code>NonEmptyMap</code>.
    */
  final def min[U >: (K, V)](implicit cmp: Ordering[U]): (K, V) = toMap.min(cmp)

  /**
    * Finds the smallest result after applying the given function to every entry.
    *
    * @return the smallest result of applying the given function to every entry of this <code>NonEmptyMap</code>.
    */
  final def minBy[U](f: ((K, V)) => U)(implicit cmp: Ordering[U]): (K, V) = toMap.minBy(f)(cmp)

  /**
    * Displays all entries of this <code>NonEmptyMap</code> in a string.
    *
    * @return a string representation of this <code>NonEmptyMap</code>. In the resulting string, the result of invoking <code>toString</code> on all entries of this
    *     <code>NonEmptyMap</code> follow each other without any separator string. 
    */
  final def mkString: String = toMap.mkString

  /**
    * Displays all entries of this <code>NonEmptyMap</code> in a string using a separator string.
    *
    * @param sep the separator string
    * @return a string representation of this <code>NonEmptyMap</code>. In the resulting string, the result of invoking <code>toString</code> on all entries of this
    *     <code>NonEmptyMap</code> are separated by the string <code>sep</code>. 
    */
  final def mkString(sep: String): String = toMap.mkString(sep)

  /**
    * Displays all entries of this <code>NonEmptyMap</code> in a string using start, end, and separator strings.
    *
    * @param start the starting string.
    * @param sep the separator string.
    * @param end the ending string.
    * @return a string representation of this <code>NonEmptyMap</code>. The resulting string begins with the string <code>start</code> and ends with the string
    *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all entries of this <code>NonEmptyMap</code> are
    *     separated by the string <code>sep</code>. 
    */
  final def mkString(start: String, sep: String, end: String): String = toMap.mkString(start, sep, end)

  /**
    * Returns <code>true</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, is non-empty.
    *
    * @return true
    */
  final def nonEmpty: Boolean = true

  /**
    * The result of multiplying all the entries of this <code>NonEmptyMap</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptyMap[T]</code> for which an implicit <code>Numeric[T]</code> exists.
    * </p>
    *
    * @return the product of all elements
    */
  final def product[U >: (K, V)](implicit num: Numeric[U]): U = toMap.product(num)

  /**
    * Reduces the entries of this <code>NonEmptyMap</code> using the specified associative binary operator.
    *
    * <p>
    * The order in which operations are performed on entries is unspecified and may be nondeterministic.
    * </p>
    *
    * @tparam U a type parameter for the binary operator, a supertype of T.
    * @param op a binary operator that must be associative.
    * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptyMap</code>.
    */
  final def reduce[U >: (K, V)](op: (U, U) => U): U = toMap.reduce(op)

  /**
    * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going left to right.
    *
    * @tparam U the result type of the binary operator.
    * @param op the binary operator.
    * @return the result of inserting <code>op</code> between consecutive entries of this <code>NonEmptyMap</code>, going left to right:
    *
    * <pre>
    * op(...op(op(x_1, x_2), x_3), ..., x_n)
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyMap</code>. 
    * </p>
    */
  final def reduceLeft[U >: (K, V)](op: (U, (K, V)) => U): U = toMap.reduceLeft(op)

  /**
    * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going left to right, returning the result in a <code>Some</code>.
    *
    * @tparam U the result type of the binary operator.
    * @param op the binary operator.
    * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
    * </p>
    */
  final def reduceLeftOption[U >: (K, V)](op: (U, (K, V)) => U): Option[U] = toMap.reduceLeftOption(op)

  final def reduceOption[U >: (K, V)](op: (U, U) => U): Option[U] = toMap.reduceOption(op)

  /**
    * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going right to left.
    *
    * @tparam U the result of the binary operator
    * @param op the binary operator
    * @return the result of inserting <code>op</code> between consecutive entries of this <code>NonEmptyMap</code>, going right to left:
    *
    * <pre>
    * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
    * </pre>
    *
    * <p>
    * where x<sub>1</sub>, ..., x<sub>n</sub> are the entries of this <code>NonEmptyMap</code>.
    * </p>
    */
  final def reduceRight[U >: (K, V)](op: ((K, V), U) => U): U = toMap.reduceRight(op)

  /**
    * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going right to left, returning the result in a <code>Some</code>.
    *
    * @tparam U the result of the binary operator
    * @param op the binary operator
    * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
    */
  final def reduceRightOption[U >: (K, V)](op: ((K, V), U) => U): Option[U] = toMap.reduceRightOption(op)

  /**
    * Checks if the given <code>Iterable</code> contains the same entries in the same order as this <code>NonEmptyMap</code>.
    *
    * @param that the <code>Iterable</code> with which to compare
    * @return <code>true</code>, if both this <code>NonEmptyMap</code> and the given <code>Iterable</code> contain the same entries
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[U >: (K, V)](that: Iterable[U]): Boolean = toMap.sameElements(that)

  /**
    * Checks if the given <code>Every</code> contains the same entries in the same order as this <code>NonEmptyMap</code>.
    *
    * @param that the <code>Every</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>Every</code> contain the same entries
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[U >: (K, V)](that: Every[U]): Boolean = toMap.sameElements(that.toVector)

  /**
    * Checks if the given <code>NonEmptyMap</code> contains the same entries in the same order as this <code>NonEmptyMap</code>.
    *
    * @param that the <code>NonEmptyMap</code> with which to compare
    * @return <code>true</code>, if both this and the given <code>NonEmptyMap</code> contain the same entries
    *     in the same order, <code>false</code> otherwise. 
    */
  final def sameElements[V1 >: V](that: NonEmptyMap[K, V1]): Boolean = toMap.sameElements(that.toMap)

  /**
    * Computes a prefix scan of the entries of this <code>NonEmptyMap</code>.
    *
    * <p>
    * Note: The neutral element z may be applied more than once. 
    * </p>
    *
    * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
    *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
    *     0 for addition, or 1 for multiplication.)
    * @param op a binary operator that must be associative
    * @return a new <code>NonEmptyMap</code> containing the prefix scan of the elements in this <code>NonEmptyMap</code> 
    */
  final def scan[V1 >: V](z: (K, V1))(op: ((K, V1), (K, V1)) => (K, V1)): NonEmptyMap[K, V1] = new NonEmptyMap(toMap.scan(z)(op).toMap)

  /**
    * Groups entries in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
    *
    * @param size the number of entries per group
    * @return an iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last and the only element will be truncated
    *     if there are fewer entries than <code>size</code>.
    */
  final def sliding(size: Int): Iterator[NonEmptyMap[K, V]] = toMap.sliding(size).map(new NonEmptyMap(_))

  /**
    * Groups entries in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
    * moving the sliding window by a given <code>step</code> each time.
    *
    * @param size the number of entries per group
    * @param step the distance between the first entries of successive groups
    * @return an iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last and the only element will be truncated
    *     if there are fewer elements than <code>size</code>.
    */
  final def sliding(size: Int, step: Int): Iterator[NonEmptyMap[K, V]] = toMap.sliding(size, step).map(new NonEmptyMap(_))

  /**
    * The size of this <code>NonEmptyMap</code>.
    *
    * <p>
    * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
    * </p>
    *
    * @return the number of elements in this <code>NonEmptyMap</code>. 
    */
  final def size: Int = toMap.size

  /**
    * Returns <code>"NonEmptyMap"</code>, the prefix of this object's <code>toString</code> representation.
    *
    * @return the string <code>"NonEmptyMap"</code>
    */
  def stringPrefix: String = "NonEmptyMap"

  /**
    * The result of summing all the elements of this <code>NonEmptyMap</code>.
    *
    * <p>
    * This method can be invoked for any <code>NonEmptyMap[T]</code> for which an implicit <code>Numeric[T]</code> exists.
    * </p>
    *
    * @return the sum of all elements
    */
  final def sum[U >: (K, V)](implicit num: Numeric[U]): U = toMap.sum(num)

  import scala.language.higherKinds

  /**
    * Converts this <code>NonEmptyMap</code> into a collection of type <code>Col</code> by copying all entries.
    *
    * @tparam Col the collection type to build.
    * @return a new collection containing all entries of this <code>NonEmptyMap</code>.
    */
  //final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, (K, V), Col[(K, V) @uV]]): Col[(K, V) @uV] = toMap.to[Col](cbf)
  final def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[(K, V), Col[(K, V) @ uV]]): Col[(K, V) @ uV] =
    toMap.to(factory)

  /**
    * Converts this <code>NonEmptyMap</code> to an array.
    *
    * @return an array containing all entries of this <code>NonEmptyMap</code>. A <code>ClassTag</code> must be available for the entry type of this <code>NonEmptyMap</code>.
    */
  final def toArray[U >: (K, V)](implicit classTag: ClassTag[U]): Array[U] = toMap.toArray

  /**
    * Converts this <code>NonEmptyMap</code> to a <code>Vector</code>.
    *
    * @return a <code>Vector</code> containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toVector: Vector[(K, V)] = toMap.toVector

  /**
    * Converts this <code>NonEmptyMap</code> to a mutable buffer.
    *
    * @return a buffer containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toBuffer[U >: (K, V)]: Buffer[U] = toMap.toBuffer

  /**
    * Converts this <code>NonEmptyMap</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toIndexedSeq: collection.immutable.IndexedSeq[(K, V)] = toMap.toVector

  /**
    * Converts this <code>NonEmptyMap</code> to an iterable collection.
    *
    * @return an <code>Iterable</code> containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toIterable: Iterable[(K, V)] = toMap.toIterable

  /**
    * Returns an <code>Iterator</code> over the entries in this <code>NonEmptyMap</code>.
    *
    * @return an <code>Iterator</code> containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toIterator: Iterator[(K, V)] = toMap.toIterator

  /**
    * Converts this <code>NonEmptyMap</code> to an immutable <code>IndexedSeq</code>.
    *
    * @return an immutable <code>IndexedSeq</code> containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toSeq: collection.immutable.Seq[(K, V)] = collection.immutable.Seq.empty[(K, V)] ++ toMap.toSeq

  /**
    * Converts this <code>NonEmptyMap</code> to a set.
    *
    * @return a set containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toSet[U >: (K, V)]: Set[U] = toMap.toSet

  /**
    * Converts this <code>NonEmptyMap</code> to a stream.
    *
    * @return a stream containing all entries of this <code>NonEmptyMap</code>.
    */
  final def toStream: Stream[(K, V)] = toMap.toStream

  /**
    * Returns a string representation of this <code>NonEmptyMap</code>.
    *
    * @return the string <code>"NonEmptyMap"</code> followed by the result of invoking <code>toString</code> on
    *   this <code>NonEmptyMap</code>'s elements, surrounded by parentheses.
    */
  override def toString: String = "NonEmptyMap(" + toMap.mkString(", ") + ")"

  /**
    * Converts this <code>NonEmptyMap</code> of pairs into two <code>Iterable</code>s of the first and second half of each pair.
    *
    * @tparam L the type of the first half of the element pairs
    * @tparam R the type of the second half of the element pairs
    * @param asPair an implicit conversion that asserts that the element type of this <code>NonEmptyMap</code> is a pair.
    * @return a pair of <code>NonEmptyMap</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyMap</code>. 
    */
  final def unzip[L, R](implicit asPair: ((K, V)) => (L, R)): (Iterable[L], Iterable[R]) = toMap.unzip

  /**
    * Converts this <code>NonEmptyMap</code> of triples into three <code>NonEmptyMap</code>s of the first, second, and and third entry of each triple.
    *
    * @tparam L the type of the first member of the entry triples
    * @tparam R the type of the second member of the entry triples
    * @tparam R the type of the third member of the entry triples
    * @param asTriple an implicit conversion that asserts that the entry type of this <code>NonEmptyMap</code> is a triple.
    * @return a triple of <code>NonEmptyMap</code>s, containing the first, second, and third member, respectively, of each entry triple of this <code>NonEmptyMap</code>.
    */
  final def unzip3[L, M, R](implicit asTriple: ((K, V)) => (L, M, R)): (Iterable[L], Iterable[M], Iterable[R]) = toMap.unzip3

  /**
    * A copy of this <code>NonEmptyMap</code> with one single replaced entry.
    *
    * @param key the key of the replacement
    * @param value the replacing value
    * @return a copy of this <code>NonEmptyMap</code> with the value at <code>key</code> replaced by the given <code>value</code>.
    */
  final def updated[V1 >: V](key: K, value: V1): NonEmptyMap[K, V1] =
    new NonEmptyMap(toMap.updated(key, value))

  /**
    * Returns a <code>NonEmptyMap</code> formed from this <code>NonEmptyMap</code> and an iterable collection by combining corresponding
    * entries in pairs. If one of the two collections is shorter than the other, placeholder entries will be used to extend the
    * shorter collection to the length of the longer.
    *
    * @tparam O the type of the second half of the returned pairs
    * @tparam V1 the subtype of the value type of this <code>NonEmptyMap</code>
    * @param other the <code>Iterable</code> providing the second half of each result pair
    * @param thisElem the element to be used to fill up the result if this <code>NonEmptyMap</code> is shorter than <code>that</code> <code>Iterable</code>.
    * @param otherElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptyMap</code>.
    * @return a new <code>NonEmptyMap</code> containing pairs consisting of corresponding entries of this <code>NonEmptyMap</code> and <code>that</code>. The
    *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyMap</code> and <code>that</code>. If this <code>NonEmptyMap</code>
    *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
    *     <code>NonEmptyMap</code>, <code>thatElem</code> values are used to pad the result. 
    */
  final def zipAll[O, V1 >: V](other: collection.Iterable[O], thisElem: (K, V1), otherElem: O): NonEmptyMap[(K, V1), O] =
    new NonEmptyMap(toMap.zipAll(other, thisElem, otherElem).toMap)

  /**
    * Zips this <code>NonEmptyMap</code>  with its indices.
    *
    * @return A new <code>NonEmptyMap</code> containing pairs consisting of all elements of this <code>NonEmptyMap</code> paired with their index. Indices start at 0.
    */
  final def zipWithIndex[V1 >: V]: NonEmptyMap[(K, V1), Int] = new NonEmptyMap(toMap.zipWithIndex.toMap)
}

/**
  * Companion object for class <code>NonEmptyMap</code>.
  */
object NonEmptyMap {

  /**
    * Constructs a new <code>NonEmptyMap</code> given at least one element.
    *
    * @tparam K the type of the key contained in the new <code>NonEmptyMap</code>
    * @tparam V the type of the value contained in the new <code>NonEmptyMap</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptyMap</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptyMap</code>
    */
  def apply[K, V](firstElement: (K, V), otherElements: (K, V)*): NonEmptyMap[K, V] = new NonEmptyMap(otherElements.toMap + firstElement)

  /**
    * Variable argument extractor for <code>NonEmptyMap</code>s.
    *
    * @tparam K the type of the key contained in the <code>NonEmptyMap</code>
    * @tparam V the type of the value contained in the <code>NonEmptyMap</code>
    * @param nonEmptyMap: the <code>NonEmptyMap</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptyMap</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[K, V](nonEmptyMap: NonEmptyMap[K, V]): Option[Seq[(K, V)]] = Some(nonEmptyMap.toSeq)

  /**
    * Optionally construct a <code>NonEmptyMap</code> containing the elements, if any, of a given <code>GenSeq</code>.
    *
    * @tparam K the type of the key contained in the new <code>NonEmptyMap</code>
    * @tparam V the type of the value contained in the new <code>NonEmptyMap</code>
    * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyMap</code>
    * @return a <code>NonEmptyMap</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
    *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
    */
  def from[K, V](seq: GenSeq[(K, V)]): Option[NonEmptyMap[K, V]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some(new NonEmptyMap(scala.collection.immutable.Map.empty[K, V] ++ seq.tail.toMap + first))
    }

  def from[K, V](map: scala.collection.GenMap[K, V]): Option[NonEmptyMap[K, V]] =
    map.headOption match {
      case None => None
      case Some(first) => Some(new NonEmptyMap(scala.collection.immutable.Map.empty[K, V] ++ map))
    }

  import scala.language.implicitConversions

  /**
    * Implicit conversion from <code>NonEmptyMap</code> to <code>Map</code>.
    *
    * <p>
    * One use case for this implicit conversion is to enable <code>GenSeq[NonEmptyMap]</code>s to be flattened.
    * Here's an example:
    * </p>
    *
    * <pre class="stREPL">
    * scala&gt; Vector(NonEmptyMap(1, 2, 3), NonEmptyMap(3, 4), NonEmptyMap(5, 6, 7, 8)).flatten
    * res0: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 3, 4, 5, 6, 7, 8)
    * </pre>
    *
    * @tparam K the type of the key contained in the <code>NonEmptyMap</code>
    * @tparam V the type of the value contained in the <code>NonEmptyMap</code>
    * @param nonEmptyMap the <code>NonEmptyMap</code> to convert to a <code>Map</code>
    * @return a <code>Map</code> containing the elements, in order, of this <code>NonEmptyMap</code>
    */
  implicit def nonEmptyMapToMap[K, V](nonEmptyMap: NonEmptyMap[K, V]): scala.collection.immutable.Map[K, V] = nonEmptyMap.toMap
}
