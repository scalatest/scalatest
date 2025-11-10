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

  extension [T](nonEmptySet: NonEmptySet[T]) {

    /**
      * Check if an element exists at its index in the <code>NonEmptySet</code>.
      *
      * @return <code>true</code> if a element exists in <code>NonEmptySet</code> at index <code>idx</code>, where <code>false</code> indicates the element at index <code>idx</code> does not exist.
      */
    def apply(elem: T): Boolean = toSet(elem)

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