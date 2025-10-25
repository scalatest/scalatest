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
import scala.collection.mutable.ArrayBuffer

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
  implicit def nonEmptyListToList[E](nonEmptyList: NonEmptyList[E]): List[E] = // given Conversion just won't work!
    nonEmptyList
    /*new scala.collection.IndexedSeq[E] {
      def apply(i: Int): E = nonEmptyList(i)
      def length: Int = nonEmptyList.length
    }*/

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
    infix def ::[U >: T](nonEmptyList: NonEmptyList[U])(using classTag: ClassTag[U]): NonEmptyList[U] = 
      NonEmptyList(element, nonEmptyList*)

    /**
      * Returns a new <code>NonEmptyList</code> with the given element prepended.
      *
      * <p>
      * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to prepend to this <code>NonEmptyList</code>
      * @return a new <code>NonEmptyList</code> consisting of <code>element</code> followed by all elements of this <code>NonEmptyList</code>.
      */
    infix def +:[U >: T](nonEmptyList: NonEmptyList[U]): NonEmptyList[U] = NonEmptyList(element, nonEmptyList*)
  }

  extension [T] (other: IterableOnce[T]) {
    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this <code>NonEmptyList</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    infix def :::[U >: T](nonEmptyList: NonEmptyList[U]): NonEmptyList[U] =
      if (other.isEmpty) nonEmptyList else other.toList ++ nonEmptyList  
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
    infix def ++[U >: T](other: NonEmptyList[U]): NonEmptyList[U] = nonEmptyList.appendedAll(other)

    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>Every</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this <code>NonEmptyList</code> and the passed <code>Every</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>Every</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    infix def ++[U >: T](other: Every[U]): NonEmptyList[U] = nonEmptyList.appendedAll(other)

    /**
      * Returns a new <code>NonEmptyList</code> containing the elements of this <code>NonEmptyList</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyList</code> is the most specific superclass encompassing the element types of this <code>NonEmptyList</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyList</code> that contains all the elements of this <code>NonEmptyList</code> followed by all elements of <code>other</code>.
      */
    infix def ++[U >: T](other: IterableOnce[U]): NonEmptyList[U] =
      if (other.isEmpty) nonEmptyList else nonEmptyList.appendedAll(other)

    /**
      * Returns a new <code>NonEmptyList</code> with the given element appended.
      *
      * <p>
      * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to append to this <code>NonEmptyList</code>
      * @return a new <code>NonEmptyList</code> consisting of all elements of this <code>NonEmptyList</code> followed by <code>element</code>.
      */
    def :+[U >: T](element: U): NonEmptyList[U] = nonEmptyList.appended(element)  

    /**
      * Builds a new <code>NonEmptyList</code> from this <code>NonEmptyList</code> without any duplicate elements.
      *
      * @return A new <code>NonEmptyList</code> that contains the first occurrence of every element of this <code>NonEmptyList</code>. 
      */
    def distinct: NonEmptyList[T] = (nonEmptyList: List[T]).distinct

    /**
      * Builds a new <code>NonEmptyList</code> by applying a function to all elements of this <code>NonEmptyList</code> and using the elements of the resulting <code>NonEmptyList</code>s.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>
      * @param f the function to apply to each element.
      * @return a new <code>NonEmptyList</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptyList</code> and concatenating
      *    the elements of resulting <code>NonEmptyList</code>s. 
      */
    def flatMap[U](f: T => NonEmptyList[U]): NonEmptyList[U] = 
      (nonEmptyList: List[T]).flatMap(f)

    /**
      * Converts this <code>NonEmptyList</code> of <code>NonEmptyList</code>s into a <code>NonEmptyList</code>
      * formed by the elements of the nested <code>NonEmptyList</code>s.
      *
      * <p>
      * Note: You cannot use this <code>flatten</code> method on a <code>NonEmptyList</code> that contains a <code>IterableOnce</code>s, because 
      * if all the nested <code>IterableOnce</code>s were empty, you'd end up with an empty <code>NonEmptyList</code>.
      * </p>
      *
      * @tparm B the type of the elements of each nested <code>NonEmptyList</code>
      * @return a new <code>NonEmptyList</code> resulting from concatenating all nested <code>NonEmptyList</code>s.
      */
    def flatten[B](implicit ev: T <:< NonEmptyList[B]): NonEmptyList[B] = flatMap(ev)

    /**
      * Partitions this <code>NonEmptyList</code> into a map of <code>NonEmptyList</code>s according to some discriminator function.
      *
      * @tparam K the type of keys returned by the discriminator function.
      * @param f the discriminator function.
      * @return A map from keys to <code>NonEmptyList</code>s such that the following invariant holds:
      *
      * <pre>
      * (nonEmptyList.toList partition f)(k) = xs filter (x =&gt; f(x) == k)
      * </pre>
      *
      * <p>
      * That is, every key <code>k</code> is bound to a <code>NonEmptyList</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
      * </p>
      */
    def groupBy[K](f: T => K): Map[K, NonEmptyList[T]] = {
      val mapKToList = (nonEmptyList: List[T]).groupBy(f)
      mapKToList.mapValues { list => NonEmptyList(list.head, list.tail*) }.toMap
    }

    /**
      * Partitions elements into fixed size <code>NonEmptyList</code>s.
      *
      * @param size the number of elements per group
      * @return An iterator producing <code>NonEmptyList</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
      */
    def grouped(size: Int): Iterator[NonEmptyList[T]] = {
      val itOfList = (nonEmptyList: List[T]).grouped(size)
      itOfList.map { list => NonEmptyList(list.head, list.tail*) }
    }

    /**
      * The length of this <code>NonEmptyList</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyList</code>. 
      */
    def length: Int = (nonEmptyList: List[T]).length  

    /**
      * Builds a new <code>NonEmptyList</code> by applying a function to all elements of this <code>NonEmptyList</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyList</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyList</code> and collecting the results. 
      */
    def map[U](f: T => U): NonEmptyList[U] =
      (nonEmptyList: List[T]).map(f)

    /**
      * A copy of this <code>NonEmptyList</code> with an element value appended until a given target length is reached.
      *
      * @param len the target length 
      * @param elem he padding value
      * @return a new <code>NonEmptyList</code> consisting of all elements of this <code>NonEmptyList</code> followed by the minimal number of occurrences
      *     of <code>elem</code> so that the resulting <code>NonEmptyList</code> has a length of at least <code>len</code>. 
      */
    def padTo[U >: T](len: Int, elem: U): NonEmptyList[U] =
      (nonEmptyList: List[T]).padTo(len, elem)

    /**
      * Produces a new <code>NonEmptyList</code> where a slice of elements in this <code>NonEmptyList</code> is replaced by another <code>NonEmptyList</code>
      *
      * @param from the index of the first replaced element 
      * @param that the <code>NonEmptyList</code> whose elements should replace a slice in this <code>NonEmptyList</code>
      * @param replaced the number of elements to drop in the original <code>NonEmptyList</code>
      */
    def patch[U >: T](from: Int, that: NonEmptyList[U], replaced: Int): NonEmptyList[U] =
      (nonEmptyList: List[T]).patch(from, that.toVector, replaced)

    /**
      * Returns new <code>NonEmptyList</code> with elements in reverse order.
      *
      * @return a new <code>NonEmptyList</code> with all elements of this <code>NonEmptyList</code> in reversed order. 
      */
    def reverse: NonEmptyList[T] =
      (nonEmptyList: List[T]).reverse  

    /**
      * Builds a new <code>NonEmptyList</code> by applying a function to all elements of this <code>NonEmptyList</code> and collecting the results in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyList.reverseMap(f)</code> is the same as <code>nonEmptyList.reverse.map(f)</code>, but might be more efficient. 
      * </p>
      *
      * @tparam U the element type of the returned <code>NonEmptyList</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyList</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyList</code>
      *     and collecting the results in reverse order. 
      */
    def reverseMap[U](f: T => U): NonEmptyList[U] =
      (nonEmptyList: List[T]).reverseMap(f)

    /**
      * Computes a prefix scan of the elements of this <code>NonEmptyList</code>.
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
      * NonEmptyList(1, 2, 3).scan(0)(_ + _) == NonEmptyList(0, 1, 3, 6)
      * NonEmptyList(1, 2, 3).scan("z")(_ + _.toString) == NonEmptyList("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>NonEmptyList</code>.
      * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return a new <code>NonEmptyList</code> containing the prefix scan of the elements in this <code>NonEmptyList</code> 
      */
    def scan[U >: T](z: U)(op: (U, U) => U): NonEmptyList[U] = (nonEmptyList: List[T]).scan(z)(op)

    /**
      * Produces a <code>NonEmptyList</code> containing cumulative results of applying the operator going left to right.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyList(1, 2, 3).scanLeft(0)(_ + _) == NonEmptyList(0, 1, 3, 6)
      * NonEmptyList(1, 2, 3).scanLeft("z")(_ + _) == NonEmptyList("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyList</code>
      * @param z the start value.
      * @param op the binary operator.
      * @return a new <code>NonEmptyList</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>,
      *     going left to right, with the start value, <code>z</code>, on the left.
      */ 
    def scanLeft[B](z: B)(op: (B, T) => B): NonEmptyList[B] = (nonEmptyList: List[T]).scanLeft(z)(op)

    /**
      * Produces a <code>NonEmptyList</code> containing cumulative results of applying the operator going right to left.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyList(1, 2, 3).scanRight(0)(_ + _) == NonEmptyList(6, 5, 3, 0)
      * NonEmptyList(1, 2, 3).scanRight("z")(_ + _) == NonEmptyList("123z", "23z", "3z", "z")
      * </pre>
      *
      * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyList</code>
      * @param z the start value
      * @param op the binary operator
      * @return a new <code>NonEmptyList</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>,
      *     going right to left, with the start value, <code>z</code>, on the right.
      */
    def scanRight[B](z: B)(op: (T, B) => B): NonEmptyList[B] = (nonEmptyList: List[T]).scanRight(z)(op)

    /**
      * Sorts this <code>NonEmptyList</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
      *
      * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
      * @param f the transformation function mapping elements to some other domain <code>U</code>.
      * @param ord the ordering assumed on domain <code>U</code>.
      * @return a <code>NonEmptyList</code> consisting of the elements of this <code>NonEmptyList</code> sorted according to the <code>Ordering</code> where
      *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
      */
    final def sortBy[U](f: T => U)(implicit ord: Ordering[U]): NonEmptyList[T] = (nonEmptyList: List[T]).sortBy(f)

    /**
      * Sorts this <code>NonEmptyList</code> according to a comparison function.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyList</code> as in the original. 
      * </p>
      *
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyList</code> consisting of the elements of this <code>NonEmptyList</code> sorted according to the comparison function <code>lt</code>.
      */
    final def sortWith(lt: (T, T) => Boolean): NonEmptyList[T] = (nonEmptyList: List[T]).sortWith(lt)

    /**
      * Sorts this <code>NonEmptyList</code> according to an <code>Ordering</code>.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyList</code> as in the original. 
      * </p>
      *
      * @param ord the <code>Ordering</code> to be used to compare elements.
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyList</code> consisting of the elements of this <code>NonEmptyList</code> sorted according to the comparison function <code>lt</code>.
      */
    final def sorted[U >: T](implicit ord: Ordering[U]): NonEmptyList[U] = (nonEmptyList: List[T]).sorted(ord)
  }

}