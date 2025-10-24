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
import scala.reflect.ClassTag
import org.scalactic.Every

/**
 * A non-empty list: an ordered, immutable, non-empty collection of elements with <code>LinearSeq</code> performance characteristics.
 *
 * <p>
 * The purpose of <code>NonEmptyList</code> is to allow you to express in a type that a <code>List</code> is non-empty, thereby eliminating the
 * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
 * performance, see <a href="Every.html"><code>Every</code></a>.
 * </p>
 * 
 * <h2>Constructing <code>NonEmptyList</code>s</h2>
 *
 * <p>
 * You can construct a <code>NonEmptyList</code> by passing one or more elements to the <code>NonEmptyList.apply</code> factory method:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; NonEmptyList(1, 2, 3)
 * res0: org.scalactic.anyvals.NonEmptyList[Int] = NonEmptyList(1, 2, 3)
 * </pre>
 *
 * <p>
 * Alternatively you can <em>cons</em> elements onto the <code>End</code> singleton object, similar to making a <code>List</code> starting with <code>Nil</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; 1 :: 2 :: 3 :: Nil
 * res0: List[Int] = List(1, 2, 3)
 *
 * scala&gt; 1 :: 2 :: 3 :: End
 * res1: org.scalactic.NonEmptyList[Int] = NonEmptyList(1, 2, 3)
 * </pre>
 *
 * <p>
 * Note that although <code>Nil</code> is a <code>List[Nothing]</code>, <code>End</code> is
 * not a <code>NonEmptyList[Nothing]</code>, because no empty <code>NonEmptyList</code> exists. (A non-empty list is a series
 * of connected links; if you have no links, you have no non-empty list.)
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; val nil: List[Nothing] = Nil
 * nil: List[Nothing] = List()
 *
 * scala&gt; val nada: NonEmptyList[Nothing] = End
 * &lt;console&gt;:16: error: type mismatch;
 * found   : org.scalactic.anyvals.End.type
 * required: org.scalactic.anyvals.NonEmptyList[Nothing]
 *        val nada: NonEmptyList[Nothing] = End
 *                                          ^
 * </pre>
 *
 * <h2>Working with <code>NonEmptyList</code>s</h2>
 *
 * <p>
 * <code>NonEmptyList</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
 * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
 * you'll get an empty <code>Seq</code>:
 * </p>
 *
 * <pre class="stREPL">
 * scala&gt; List(1).tail
 * res6: List[Int] = List()
 * </pre>
 *
 * <p>
 * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
 * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptyList</code> defines a method corresponding to every such <code>Seq</code>
 * method. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * NonEmptyList(1, 2, 3).map(_ + 1)                        // Result: NonEmptyList(2, 3, 4)
 * NonEmptyList(1).map(_ + 1)                              // Result: NonEmptyList(2)
 * NonEmptyList(1, 2, 3).containsSlice(NonEmptyList(2, 3)) // Result: true
 * NonEmptyList(1, 2, 3).containsSlice(NonEmptyList(3, 4)) // Result: false
 * NonEmptyList(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
 * </pre>
 *
 * <p>
 * <code>NonEmptyList</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
 * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptyList</code> to <code>List</code>
 * is defined in the <code>NonEmptyList</code> companion object that will be applied if you attempt to call one of the missing methods. As a
 * result, you can invoke <code>filter</code> on an <code>NonEmptyList</code>, even though <code>filter</code> could result
 * in an empty sequence&mdash;but the result type will be <code>List</code> instead of <code>NonEmptyList</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * NonEmptyList(1, 2, 3).filter(_ &lt; 10) // Result: List(1, 2, 3)
 * NonEmptyList(1, 2, 3).filter(_ &gt; 10) // Result: List()
 * </pre>
 * 
 *
 * <p>
 * You can use <code>NonEmptyList</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyList</code> unless
 * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
 * result type will switch to a <code>List</code> at that point. Here are some examples:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalactic.anyvals._
 * import org.scalactic.anyvals._
 *
 * scala&gt; for (i &lt;- NonEmptyList(1, 2, 3)) yield i + 1
 * res0: org.scalactic.anyvals.NonEmptyList[Int] = NonEmptyList(2, 3, 4)
 *
 * scala&gt; for (i &lt;- NonEmptyList(1, 2, 3) if i &lt; 10) yield i + 1
 * res1: List[Int] = List(2, 3, 4)
 *
 * scala&gt; for {
 *      |   i &lt;- NonEmptyList(1, 2, 3)
 *      |   j &lt;- NonEmptyList('a', 'b', 'c')
 *      | } yield (i, j)
 * res3: org.scalactic.anyvals.NonEmptyList[(Int, Char)] =
 *         NonEmptyList((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
 *
 * scala&gt; for {
 *      |   i &lt;- NonEmptyList(1, 2, 3) if i &lt; 10
 *      |   j &lt;- NonEmptyList('a', 'b', 'c')
 *      | } yield (i, j)
 * res6: List[(Int, Char)] =
 *         List((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
 * </pre>
 *
 * @tparam T the type of elements contained in this <code>NonEmptyList</code>
 */
opaque type NonEmptyList[T] = List[T] & { def length: Int & (1 | Int) }

/**
  * Companion object for class <code>NonEmptyList</code>.
  */
object NonEmptyList {

  /**
   * Constructs a new <code>NonEmptyList</code> given at least one element.
   *
   * @tparam T the type of the element contained in the new <code>NonEmptyList</code>
   * @param firstElement the first element (with index 0) contained in this <code>NonEmptyList</code>
   * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptyList</code>
   */
  def apply[T](firstElement: T, otherElements: T*): NonEmptyList[T] = firstElement :: otherElements.toList

  /**
   * Variable argument extractor for <code>NonEmptyList</code>s.
   *
   * @param nonEmptyList: the <code>NonEmptyList</code> containing the elements to extract
   * @return an <code>Seq</code> containing this <code>NonEmptyList</code>s elements, wrapped in a <code>Some</code> 
   */
  def unapplySeq[T](nonEmptyList: NonEmptyList[T]): Option[Seq[T]] = Some(nonEmptyList.toList)

  /**
   * Optionally construct a <code>NonEmptyList</code> containing the elements, if any, of a given <code>GenSeq</code>.
   *
   * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyList</code>
   * @return a <code>NonEmptyList</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
   *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
   */
  def from[T](seq: GenSeq[T]): Option[NonEmptyList[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some(first :: seq.tail.toList)
    }

  import scala.language.implicitConversions  

  /**
    * Implicit conversion from <code>NonEmptyList</code> to <code>GenSeq</code>.
    *
    * @param nonEmptyList the <code>NonEmptyList</code> to convert
    * @return the <code>GenSeq</code>
    */
  implicit def nonEmptyListToGenSeq[E](nonEmptyList: NonEmptyList[E]): scala.collection.GenSeq[E] = // given Conversion just won't work!
    new scala.collection.IndexedSeq[E] {
      def apply(i: Int): E = nonEmptyList(i)
      def length: Int = nonEmptyList.length
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
    infix def ::[U >: T](list: NonEmptyList[U])(using classTag: ClassTag[U]): NonEmptyList[U] = 
      NonEmptyList(element, list*)
  }  

  extension [T] (nonEmptyList: NonEmptyList[T]) {
    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>NonEmptyList</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this and the passed <code>NonEmptyList</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>NonEmptyList</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    def ++[U >: T](other: NonEmptyList[U]): NonEmptyList[U] = nonEmptyList.appendedAll(other)

    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>Every</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this <code>NonEmptyList</code> and the passed <code>Every</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>Every</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    def ++[U >: T](other: Every[U]): NonEmptyList[U] = nonEmptyList.appendedAll(other)

    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this <code>NonEmptyList</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    def ++[U >: T](other: IterableOnce[U]): NonEmptyList[U] =
      if (other.isEmpty) nonEmptyList else nonEmptyList.appendedAll(other)

    /**
      * The length of this <code>NonEmptyList</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyList</code>. 
      */
    final def length: Int = (nonEmptyList: List[T]).length

      
  }

}