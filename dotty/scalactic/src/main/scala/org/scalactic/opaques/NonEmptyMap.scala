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

import scala.collection.GenSeq
import scala.collection.mutable.ArrayBuffer

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

  opaque type NonEmptyMap[K, +V] = Map[K, V] & { def size: Int & (1 | Int) }

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
  def apply[K, V](firstElement: (K, V), otherElements: (K, V)*): NonEmptyMap[K, V] = otherElements.toMap + firstElement

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
      case Some(first) => Some(scala.collection.immutable.Map.empty[K, V] ++ seq.tail.toMap + first)
    }

  def from[K, V](map: scala.collection.GenMap[K, V]): Option[NonEmptyMap[K, V]] =
    map.headOption match {
      case None => None
      case Some(first) => Some(scala.collection.immutable.Map.empty[K, V] ++ map)
    }

  import scala.language.implicitConversions

  /**
    * Implicit conversion from <code>NonEmptyMap</code> to <code>Map</code>.
    *
    * @param nonEmptyMap the <code>Map</code> to convert
    * @return the <code>Map</code>
    */
  implicit def nonEmptyMapToMap[K, V](nonEmptyMap: NonEmptyMap[K, V]): Map[K, V] = // given Conversion just won't work!
    nonEmptyMap

  extension [K, V](entry: (K, V)) {
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
    infix def +:[V1 >: V](nonEmptyMap: NonEmptyMap[K, V1]): NonEmptyMap[K, V1] = nonEmptyMap + entry
  }

  extension [K, V] (nonEmptyMap: NonEmptyMap[K, V]) {
    /**
      * Builds a new <code>NonEmptyMap</code> by applying a function to all entries of this <code>NonEmptyMap</code> and using the entries of the resulting <code>NonEmptyMap</code>s.
      *
      * @tparam K1 the key type of the returned <code>NonEmptyMap</code>
      * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
      * @param f the function to apply to each entry.
      * @return a new <code>NonEmptyMap</code> containing entries obtained by applying the given function <code>f</code> to each entry of this <code>NonEmptyMap</code> and concatenating
      *    the entries of resulting <code>NonEmptyMap</code>s.
      */
    def flatMap[K1, V1](f: ((K, V)) => NonEmptyMap[K1, V1]): NonEmptyMap[K1, V1] = {
      val buf = new ArrayBuffer[(K1, V1)]
      for (ele <- nonEmptyMap)
        buf ++= f(ele).toMap
      buf.toMap
    }

    /**
      * Builds a new <code>NonEmptyMap</code> by applying a function to all entries of this <code>NonEmptyMap</code>.
      *
      * @tparam K1 the key type of the returned <code>NonEmptyMap</code>.
      * @tparam V1 the value type of the returned <code>NonEmptyMap</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyMap</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyMap</code> and collecting the results. 
      */
    def map[K1, V1](f: ((K, V)) => (K1, V1)): NonEmptyMap[K1, V1] =
      (nonEmptyMap: Map[K, V]).map(f)

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
    def groupBy(f: ((K, V)) => K): Map[K, NonEmptyMap[K, V]] = 
      (nonEmptyMap: Map[K, V]).groupBy(f)

    /**
      * Partitions entries into fixed size <code>NonEmptyMap</code>s.
      *
      * @param size the number of entries per group
      * @return An iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last will be truncated if the entries don't divide evenly.
      */
    def grouped(size: Int): Iterator[NonEmptyMap[K, V]] = 
      (nonEmptyMap: Map[K, V]).grouped(size)

    /**
      * Checks if the given <code>Iterable</code> contains the same entries in the same order as this <code>NonEmptyMap</code>.
      *
      * @param that the <code>Iterable</code> with which to compare
      * @return <code>true</code>, if both this <code>NonEmptyMap</code> and the given <code>Iterable</code> contain the same entries
      *     in the same order, <code>false</code> otherwise. 
      */
    def sameElements[U >: (K, V)](that: Iterable[U]): Boolean = {
      val thisIterator = nonEmptyMap.iterator
      val thatIterator = that.iterator
      
      while (thisIterator.hasNext && thatIterator.hasNext) {
        if (thisIterator.next() != thatIterator.next()) {
          return false
        }
      }
      
      // Both must be exhausted for true equality
      !thisIterator.hasNext && !thatIterator.hasNext
    }

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
    def scan[V1 >: V](z: (K, V1))(op: ((K, V1), (K, V1)) => (K, V1)): NonEmptyMap[K, V1] = (nonEmptyMap: Map[K, V1]).scan(z)(op).toMap

    /**
      * Returns <code>"NonEmptyMap"</code>, the prefix of this object's <code>toString</code> representation.
      *
      * @return the string <code>"NonEmptyMap"</code>
      */
    def stringPrefix: String = "NonEmptyMap"

    /**
      * A copy of this <code>NonEmptyMap</code> with one single replaced entry.
      *
      * @param key the key of the replacement
      * @param value the replacing value
      * @return a copy of this <code>NonEmptyMap</code> with the value at <code>key</code> replaced by the given <code>value</code>.
      */
    final def updated[V1 >: V](key: K, value: V1): NonEmptyMap[K, V1] =
      (nonEmptyMap: Map[K, V1]).updated(key, value)
  }

}