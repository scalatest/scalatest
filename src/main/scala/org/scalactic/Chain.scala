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
package org.scalactic

import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.GenTraversableOnce
import scala.reflect.ClassTag
import scala.collection.mutable.Buffer
import scala.collection.GenSeq
import scala.collection.GenIterable
import scala.collection.generic.CanBuildFrom
import scala.annotation.unchecked.{ uncheckedVariance => uV }

// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
/**
 * A non-empty list: an ordered, immutable, non-empty collection of elements with <code>LinearSeq</code> performance characteristics.
 *
 * <p>
 * The purpose of <code>Chain</code> is to allow you to express in a type that a <code>List</code> is non-empty, thereby eliminating the
 * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
 * performance, see <a href="Every.html"><code>Every</code></a>.
 * </p>
 * 
 * <h2>Constructing <code>Chain</code>s</h2>
 *
 * <p>
 * You can construct a <code>Chain</code> by passing one or more elements to the <code>Chain.apply</code> factory method:
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; Chain(1, 2, 3)
 * res0: org.scalactic.Chain[Int] = Chain(1, 2, 3)
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
 * res1: org.scalactic.Chain[Int] = Chain(1, 2, 3)
 * </pre>
 *
 * <p>
 * Note that although <code>Nil</code> is a <code>List[Nothing]</code>, <code>End</code> is
 * not a <code>Chain[Nothing]</code>, because no empty <code>Chain</code> exists. (A chain is a series
 * of connected links; if you have no links, you have no chain.)
 * </p>
 *
 * <pre class="stHighlight">
 * scala&gt; val nil: List[Nothing] = Nil
 * nil: List[Nothing] = List()
 *
 * scala&gt; val nada: Chain[Nothing] = End
 * &lt;console&gt;:16: error: type mismatch;
 * found   : org.scalactic.End.type
 * required: org.scalactic.Chain[Nothing]
 *        val nada: Chain[Nothing] = End
 *                                   ^
 * </pre>
 *
 * <h2>Working with <code>Chain</code>s</h2>
 *
 * <p>
 * <code>Chain</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
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
 * to not result in an empty <code>Seq</code>. For convenience, <code>Chain</code> defines a method corresponding to every such <code>Seq</code>
 * method. Here are some examples:
 * </p>
 *
 * <pre class="stHighlight">
 * Chain(1, 2, 3).map(_ + 1)                 // Result: Chain(2, 3, 4)
 * Chain(1).map(_ + 1)                       // Result: Chain(2)
 * Chain(1, 2, 3).containsSlice(Chain(2, 3)) // Result: true
 * Chain(1, 2, 3).containsSlice(Chain(3, 4)) // Result: false
 * Chain(-1, -2, 3, 4, 5).minBy(_.abs)       // Result: -1
 * </pre>
 *
 * <p>
 * <code>Chain</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
 * an empty <code>Seq</code>. However, an implicit converison from <code>Chain</code> to <code>List</code>
 * is defined in the <code>Chain</code> companion object that will be applied if you attempt to call one of the missing methods. As a
 * result, you can invoke <code>filter</code> on an <code>Chain</code>, even though <code>filter</code> could result
 * in an empty sequence&mdash;but the result type will be <code>List</code> instead of <code>Chain</code>:
 * </p>
 *
 * <pre class="stHighlight">
 * Chain(1, 2, 3).filter(_ &lt; 10) // Result: List(1, 2, 3)
 * Chain(1, 2, 3).filter(_ &gt; 10) // Result: List()
 * </pre>
 * 
 *
 * <p>
 * You can use <code>Chain</code>s in <code>for</code> expressions. The result will be an <code>Chain</code> unless
 * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
 * result type will switch to a <code>List</code> at that point. Here are some examples:
 * </p>
 * 
 * <pre class="stREPL">
 * scala&gt; import org.scalactic._
 * import org.scalactic._
 *
 * scala&gt; for (i &lt;- Chain(1, 2, 3)) yield i + 1
 * res0: org.scalactic.Chain[Int] = Chain(2, 3, 4)
 *
 * scala&gt; for (i &lt;- Chain(1, 2, 3) if i &lt; 10) yield i + 1
 * res1: List[Int] = List(2, 3, 4)
 *
 * scala&gt; for {
 *      |   i &lt;- Chain(1, 2, 3)
 *      |   j &lt;- Chain('a', 'b', 'c')
 *      | } yield (i, j)
 * res3: org.scalactic.Chain[(Int, Char)] =
 *         Chain((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
 *
 * scala&gt; for {
 *      |   i &lt;- Chain(1, 2, 3) if i &lt; 10
 *      |   j &lt;- Chain('a', 'b', 'c')
 *      | } yield (i, j)
 * res6: List[(Int, Char)] =
 *         List((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
 * </pre>
 *
 * @tparam T the type of elements contained in this <code>Chain</code>
 */
final class Chain[+T] private (underlying: List[T]) extends PartialFunction[Int, T] {

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>Chain</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this and the passed <code>Chain</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>Chain</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def ++[U >: T](other: Chain[U]): Chain[U] = new Chain(underlying ++ other.toList)

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>Every</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this <code>Chain</code> and the passed <code>Every</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>Every</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def ++[U >: T](other: Every[U]): Chain[U] = new Chain(underlying ++ other.toVector)

  // TODO: Have I added these extra ++, etc. methods to Every that take a Chain?

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>GenTraversableOnce</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this <code>Chain</code>
   * and the passed <code>GenTraversableOnce</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>GenTraversableOnce</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def ++[U >: T](other: GenTraversableOnce[U]): Chain[U] =
    if (other.isEmpty) this else new Chain(underlying ++ other)

  /**
   * Fold left: applies a binary operator to a start value, <code>z</code>, and all elements of this <code>Chain</code>, going left to right.
   *
   * <p>
   * Note: <code>/:</code> is alternate syntax for the <code>foldLeft</code> method; <code>z</code> <code>/:</code> <code>chain</code> is the
   * same as <code>chain</code> <code>foldLeft</code> <code>z</code>.
   * </p>
   *
   * @tparam B the result of the binary operator
   * @param z the start value
   * @param op the binary operator
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going left to right, with the start value,
   *     <code>z</code>, on the left:
   *
   * <pre>
   * op(...op(op(z, x_1), x_2), ..., x_n)
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */
  final def /:[B](z: B)(op: (B, T) => B): B = underlying./:(z)(op)

  /**
   * Fold right: applies a binary operator to all elements of this <code>Chain</code> and a start value, going right to left.
   *
   * <p>
   * Note: <code>:\</code> is alternate syntax for the <code>foldRight</code> method; <code>chain</code> <code>:\</code> <code>z</code> is the same
   * as <code>chain</code> <code>foldRight</code> <code>z</code>.
   * </p>
   *
   * @tparam B the result of the binary operator
   * @param z the start value
   * @param op the binary operator
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going right to left, with the start value,
   *     <code>z</code>, on the right:
   *
   * <pre>
   * op(x_1, op(x_2, ... op(x_n, z)...))
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */
  final def :\[B](z: B)(op: (T, B) => B): B = underlying.:\(z)(op)

  /**
   * Returns a new <code>Chain</code> with the given element prepended.
   *
   * <p>
   * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
   * </p>
   *
   * @param element the element to prepend to this <code>Chain</code>
   * @return a new <code>Chain</code> consisting of <code>element</code> followed by all elements of this <code>Chain</code>.
   */
  final def +:[U >: T](element: U): Chain[U] = new Chain(element +: underlying)

  /**
   * Adds an element to the beginning of this <code>Chain</code>.
   *
   * <p>
   * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
   * </p>
   *
   * @param element the element to prepend to this <code>Chain</code>
   * @return a <code>Chain</code> that contains <code>element</code> as first element and that continues with this <code>Chain</code>.
   */
  final def ::[U >: T](element: U): Chain[U] = new Chain(element +: underlying)

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>Chain</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this and the passed <code>Chain</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>Chain</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def :::[U >: T](other: Chain[U]): Chain[U] = new Chain(other.toList ::: underlying)

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>Every</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this and the passed <code>Every</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>Every</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def :::[U >: T](other: Every[U]): Chain[U] = new Chain(other.toList ::: underlying)

  /**
   * Returns a new <code>Chain</code> containing the elements of this <code>Chain</code> followed by the elements of the passed <code>GenTraversableOnce</code>.
   * The element type of the resulting <code>Chain</code> is the most specific superclass encompassing the element types of this <code>Chain</code>
   * and the passed <code>GenTraversableOnce</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param other the <code>GenTraversableOnce</code> to append
   * @return a new <code>Chain</code> that contains all the elements of this <code>Chain</code> followed by all elements of <code>other</code>.
   */
  def :::[U >: T](other: GenTraversableOnce[U]): Chain[U] =
    if (other.isEmpty) this else new Chain(other.toList ::: underlying)

  /**
   * Returns a new <code>Chain</code> with the given element appended.
   *
   * <p>
   * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
   * </p>
   *
   * @param element the element to append to this <code>Chain</code>
   * @return a new <code>Chain</code> consisting of all elements of this <code>Chain</code> followed by <code>element</code>.
   */
  def :+[U >: T](element: U): Chain[U] = new Chain(underlying :+ element)

  /**
   * Appends all elements of this <code>Chain</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
   * on of every element of this <code>Chain</code>, without any separator string.
   *
   * @param sb the string builder to which elements will be appended
   * @return the string builder, <code>sb</code>, to which elements were appended.
   */
  final def addString(sb: StringBuilder): StringBuilder = underlying.addString(sb)

  /**
   * Appends all elements of this <code>Chain</code> to a string builder using a separator string. The written text will consist of a concatenation of the
   * result of invoking <code>toString</code>
   * on of every element of this <code>Chain</code>, separated by the string <code>sep</code>.
   *
   * @param sb the string builder to which elements will be appended
   * @param sep the separator string
   * @return the string builder, <code>sb</code>, to which elements were appended.
   */
  final def addString(sb: StringBuilder, sep: String): StringBuilder = underlying.addString(sb, sep)

  /**
   * Appends all elements of this <code>Chain</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
   * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>Chain</code>,
   * separated by the string <code>sep</code>; and the string <code>end</code>
   *
   * @param sb the string builder to which elements will be appended
   * @param start the starting string
   * @param sep the separator string
   * @param start the ending string
   * @return the string builder, <code>sb</code>, to which elements were appended.
   */
  final def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.addString(sb, start, sep, end)

  /**
   * Selects an element by its index in the <code>Chain</code>.
   *
   * @return the element of this <code>Chain</code> at index <code>idx</code>, where 0 indicates the first element.
   */
  final def apply(idx: Int): T = underlying(idx)

  /**
   * Finds the first element of this <code>Chain</code> for which the given partial function is defined, if any, and applies the partial function to it.
   *
   * @param pf the partial function
   * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
   *    the partial function was not defined for any element.
   */
  final def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = underlying.collectFirst(pf)

  /**
   * Indicates whether this <code>Chain</code> contains a given value as an element.
   *
   * @param elem the element to look for
   * @return true if this <code>Chain</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
   */ 
  final def contains(elem: Any): Boolean = underlying.contains(elem)

  /**
   * Indicates whether this <code>Chain</code> contains a given <code>GenSeq</code> as a slice.
   *
   * @param that the <code>GenSeq</code> slice to look for
   * @return true if this <code>Chain</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
   */
  final def containsSlice[B](that: GenSeq[B]): Boolean = underlying.containsSlice(that)

  /**
   * Indicates whether this <code>Chain</code> contains a given <code>Every</code> as a slice.
   *
   * @param that the <code>Every</code> slice to look for
   * @return true if this <code>Chain</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
   */
  final def containsSlice[B](that: Every[B]): Boolean = underlying.containsSlice(that.toVector)

  /**
   * Indicates whether this <code>Chain</code> contains a given <code>Chain</code> as a slice.
   *
   * @param that the <code>Chain</code> slice to look for
   * @return true if this <code>Chain</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
   */
  final def containsSlice[B](that: Chain[B]): Boolean = underlying.containsSlice(that.toList)

  /**
   * Copies values of this <code>Chain</code> to an array. Fills the given array <code>arr</code> with values of this <code>Chain</code>. Copying
   * will stop once either the end of the current <code>Chain</code> is reached, or the end of the array is reached.
   *
   * @param arr the array to fill
   */
  final def copyToArray[U >: T](arr: Array[U]): Unit = underlying.copyToArray(arr)

  /**
   * Copies values of this <code>Chain</code> to an array. Fills the given array <code>arr</code> with values of this <code>Chain</code>, beginning at
   * index <code>start</code>. Copying will stop once either the end of the current <code>Chain</code> is reached, or the end of the array is reached.
   *
   * @param arr the array to fill
   * @param start the starting index
   */
  final def copyToArray[U >: T](arr: Array[U], start: Int): Unit = underlying.copyToArray(arr, start)

  /**
   * Copies values of this <code>Chain</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>Chain</code>, beginning at
   * index <code>start</code>. Copying will stop once either the end of the current <code>Chain</code> is reached, the end of the array is reached, or
   * <code>len</code> elements have been copied.
   *
   * @param arr the array to fill
   * @param start the starting index
   * @param len the maximum number of elements to copy
   */
  final def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = underlying.copyToArray(arr, start, len)

  /**
   * Copies all elements of this <code>Chain</code> to a buffer. 
   *
   * @param buf the buffer to which elements are copied
   */
  final def copyToBuffer[U >: T](buf: Buffer[U]): Unit = underlying.copyToBuffer(buf)

  /**
   * Indicates whether every element of this <code>Chain</code> relates to the corresponding element of a given <code>GenSeq</code> by satisfying a given predicate. 
   *
   * @tparam B the type of the elements of <code>that</code>
   * @param that the <code>GenSeq</code> to compare for correspondence
   * @param p the predicate, which relates elements from this <code>Chain</code> and the passed <code>GenSeq</code>
   * @return true if this <code>Chain</code> and the passed <code>GenSeq</code> have the same length and <code>p(x, y)</code> is <code>true</code>
   *     for all corresponding elements <code>x</code> of this <code>Chain</code> and <code>y</code> of that, otherwise <code>false</code>.
   */
  final def corresponds[B](that: GenSeq[B])(p: (T, B) => Boolean): Boolean = underlying.corresponds(that)(p)

  /**
   * Indicates whether every element of this <code>Chain</code> relates to the corresponding element of a given <code>Every</code> by satisfying a given predicate. 
   *
   * @tparam B the type of the elements of <code>that</code>
   * @param that the <code>Every</code> to compare for correspondence
   * @param p the predicate, which relates elements from this <code>Chain</code> and the passed <code>Every</code>
   * @return true if this <code>Chain</code> and the passed <code>Every</code> have the same length and <code>p(x, y)</code> is <code>true</code>
   *     for all corresponding elements <code>x</code> of this <code>Chain</code> and <code>y</code> of that, otherwise <code>false</code>.
   */
  final def corresponds[B](that: Every[B])(p: (T, B) => Boolean): Boolean = underlying.corresponds(that.toVector)(p)

  /**
   * Indicates whether every element of this <code>Chain</code> relates to the corresponding element of a given <code>Chain</code> by satisfying a given predicate. 
   *
   * @tparam B the type of the elements of <code>that</code>
   * @param that the <code>Chain</code> to compare for correspondence
   * @param p the predicate, which relates elements from this and the passed <code>Chain</code>
   * @return true if this and the passed <code>Chain</code> have the same length and <code>p(x, y)</code> is <code>true</code>
   *     for all corresponding elements <code>x</code> of this <code>Chain</code> and <code>y</code> of that, otherwise <code>false</code>.
   */
  final def corresponds[B](that: Chain[B])(p: (T, B) => Boolean): Boolean = underlying.corresponds(that.toList)(p)

  /**
   * Counts the number of elements in this <code>Chain</code> that satisfy a predicate. 
   *
   * @param p the predicate used to test elements.
   * @return the number of elements satisfying the predicate <code>p</code>. 
   */
  final def count(p: T => Boolean): Int = underlying.count(p)

  /**
   * Builds a new <code>Chain</code> from this <code>Chain</code> without any duplicate elements.
   *
   * @return A new <code>Chain</code> that contains the first occurrence of every element of this <code>Chain</code>. 
   */
  final def distinct: Chain[T] = new Chain(underlying.distinct)

  /**
   * Indicates whether this <code>Chain</code> ends with the given <code>GenSeq</code>.
   *
   * @param that the sequence to test
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
   */
  final def endsWith[B](that: GenSeq[B]): Boolean = underlying.endsWith(that)

  /**
   * Indicates whether this <code>Chain</code> ends with the given <code>Every</code>.
   *
   * @param that the <code>Every</code> to test
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
   */
  final def endsWith[B](that: Every[B]): Boolean = underlying.endsWith(that.toVector)

  // TODO: Search for that: Every in here and add a that: Chain in Every.
  /**
   * Indicates whether this <code>Chain</code> ends with the given <code>Chain</code>.
   *
   * @param that the <code>Chain</code> to test
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
   */
  final def endsWith[B](that: Chain[B]): Boolean = underlying.endsWith(that.toList)

  override def equals(o: Any): Boolean =
    o match {
      case chain: Chain[_] => underlying == chain.toList
      case _ => false
    }

  /**
   * Indicates whether a predicate holds for at least one of the elements of this <code>Chain</code>.
   *
   * @param the predicate used to test elements.
   * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>Chain</code>, otherwise <code>false</code>. 
   */
  final def exists(p: T => Boolean): Boolean = underlying.exists(p)

  /**
   * Finds the first element of this <code>Chain</code> that satisfies the given predicate, if any.
   *
   * @param p the predicate used to test elements
   * @return an <code>Some</code> containing the first element in this <code>Chain</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
   */
  final def find(p: T => Boolean): Option[T] = underlying.find(p)

  /**
   * Builds a new <code>Chain</code> by applying a function to all elements of this <code>Chain</code> and using the elements of the resulting <code>Chain</code>s.
   *
   * @tparam U the element type of the returned <code>Chain</code>
   * @param f the function to apply to each element.
   * @return a new <code>Chain</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>Chain</code> and concatenating
   *    the elements of resulting <code>Chain</code>s. 
   */
  final def flatMap[U](f: T => Chain[U]): Chain[U] = {
    val buf = new ArrayBuffer[U]
    for (ele <- underlying)
      buf ++= f(ele).toList
    new Chain(buf.toList)
  }

  /**
   * Converts this <code>Chain</code> of <code>Chain</code>s into a <code>Chain</code>
   * formed by the elements of the nested <code>Chain</code>s.
   *
   * <p>
   * Note: You cannot use this <code>flatten</code> method on a <code>Chain</code> that contains a <code>GenTraversableOnce</code>s, because 
   * if all the nested <code>GenTraversableOnce</code>s were empty, you'd end up with an empty <code>Chain</code>.
   * </p>
   *
   * @tparm B the type of the elements of each nested <code>Chain</code>
   * @return a new <code>Chain</code> resulting from concatenating all nested <code>Chain</code>s.
   */
  final def flatten[B](implicit ev: T <:< Chain[B]): Chain[B] = flatMap(ev)

  /**
   * Folds the elements of this <code>Chain</code> using the specified associative binary operator.
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
  final def fold[U >: T](z: U)(op: (U, U) => U): U = underlying.fold(z)(op)

  /**
   * Applies a binary operator to a start value and all elements of this <code>Chain</code>, going left to right.
   *
   * @tparam B the result type of the binary operator.
   * @param z the start value.
   * @param op the binary operator.
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going left to right, with the start value,
   *     <code>z</code>, on the left:
   *
   * <pre>
   * op(...op(op(z, x_1), x_2), ..., x_n)
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */ 
  final def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.foldLeft(z)(op)

  /**
   * Applies a binary operator to all elements of this <code>Chain</code> and a start value, going right to left.
   *
   * @tparam B the result of the binary operator
   * @param z the start value
   * @param op the binary operator
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going right to left, with the start value,
   *     <code>z</code>, on the right:
   *
   * <pre>
   * op(x_1, op(x_2, ... op(x_n, z)...))
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */
  final def foldRight[B](z: B)(op: (T, B) => B): B = underlying.foldRight(z)(op)

  /**
   * Indicates whether a predicate holds for all elements of this <code>Chain</code>.
   *
   * @param p the predicate used to test elements.
   * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>Chain</code>, otherwise <code>false</code>. 
   */
  final def forall(p: T => Boolean): Boolean = underlying.forall(p)

  /**
   * Applies a function <code>f</code> to all elements of this <code>Chain</code>.
   *
   * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
   */ 
  final def foreach(f: T => Unit): Unit = underlying.foreach(f)

  /**
   * Partitions this <code>Chain</code> into a map of <code>Chain</code>s according to some discriminator function.
   *
   * @tparam K the type of keys returned by the discriminator function.
   * @param f the discriminator function.
   * @return A map from keys to <code>Chain</code>s such that the following invariant holds:
   *
   * <pre>
   * (chain.toList partition f)(k) = xs filter (x =&gt; f(x) == k)
   * </pre>
   *
   * <p>
   * That is, every key <code>k</code> is bound to a <code>Chain</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
   * </p>
   */
  final def groupBy[K](f: T => K): Map[K, Chain[T]] = {
    val mapKToList = underlying.groupBy(f)
    mapKToList.mapValues { list => new Chain(list) }
  }

  /**
   * Partitions elements into fixed size <code>Chain</code>s.
   *
   * @param size the number of elements per group
   * @return An iterator producing <code>Chain</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
   */
  final def grouped(size: Int): Iterator[Chain[T]] = {
    val itOfList = underlying.grouped(size)
    itOfList.map { list => new Chain(list) }
  }

  /**
   * Returns <code>true</code> to indicate this <code>Chain</code> has a definite size, since all <code>Chain</code>s are strict collections.
   */
  final def hasDefiniteSize: Boolean = true

  override def hashCode: Int = underlying.hashCode

  /**
   * Selects the first element of this <code>Chain</code>. 
   *
   * @return the first element of this <code>Chain</code>.
   */
  final def head: T = underlying.head

  // Methods like headOption I can't get rid of because of the implicit conversion to GenTraversable.
  // Users can call any of the methods I've left out on a Chain, and get whatever List would return
  // for that method call. Eventually I'll probably implement them all to save the implicit conversion.

  /**
   * Selects the first element of this <code>Chain</code> and returns it wrapped in a <code>Some</code>. 
   *
   * @return the first element of this <code>Chain</code>, wrapped in a <code>Some</code>.
   */
  final def headOption: Option[T] = underlying.headOption

  /**
   * Finds index of first occurrence of some value in this <code>Chain</code>.
   *
   * @param elem the element value to search for. 
   * @return the index of the first element of this <code>Chain</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
   *     or <code>-1</code>, if none exists.
   */
  final def indexOf[U >: T](elem: U): Int = underlying.indexOf(elem, 0)

  /**
   * Finds index of first occurrence of some value in this <code>Chain</code> after or at some start index.
   *
   * @param elem the element value to search for. 
   * @param from the start index
   * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>Chain</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
   *     or <code>-1</code>, if none exists.
   */
  final def indexOf[U >: T](elem: U, from: Int): Int = underlying.indexOf(elem, from)


  /**
   * Finds first index where this <code>Chain</code> contains a given <code>GenSeq</code> as a slice.
   * 
   * @param that the <code>GenSeq</code> defining the slice to look for
   * @return the first index at which the elements of this <code>Chain</code> starting at that index match the elements of
   *     <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: GenSeq[U]): Int = underlying.indexOfSlice(that)

  /**
   * Finds first index after or at a start index where this <code>Chain</code> contains a given <code>GenSeq</code> as a slice.
   * 
   * @param that the <code>GenSeq</code> defining the slice to look for
   * @param from the start index
   * @return the first index <code>&gt;=</code> <code>from</code> at which the elements of this <code>Chain</code> starting at that index match the elements of
   *     <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: GenSeq[U], from: Int): Int = underlying.indexOfSlice(that, from)

  /**
   * Finds first index where this <code>Chain</code> contains a given <code>Every</code> as a slice.
   * 
   * @param that the <code>Every</code> defining the slice to look for
   * @return the first index such that the elements of this <code>Chain</code> starting at this index match the elements of
   *     <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: Every[U]): Int = underlying.indexOfSlice(that.toVector)

  /**
   * Finds first index where this <code>Chain</code> contains a given <code>Chain</code> as a slice.
   * 
   * @param that the <code>Chain</code> defining the slice to look for
   * @return the first index such that the elements of this <code>Chain</code> starting at this index match the elements of
   *     <code>Chain</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: Chain[U]): Int = underlying.indexOfSlice(that.toList)

  /**
   * Finds first index after or at a start index where this <code>Chain</code> contains a given <code>Every</code> as a slice.
   * 
   * @param that the <code>Every</code> defining the slice to look for
   * @param from the start index
   * @return the first index <code>&gt;=</code> <code>from</code> such that the elements of this <code>Chain</code> starting at this index match the elements of
   *     <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: Every[U], from: Int): Int = underlying.indexOfSlice(that.toVector, from)

  /**
   * Finds first index after or at a start index where this <code>Chain</code> contains a given <code>Chain</code> as a slice.
   * 
   * @param that the <code>Chain</code> defining the slice to look for
   * @param from the start index
   * @return the first index <code>&gt;=</code> <code>from</code> such that the elements of this <code>Chain</code> starting at this index match the elements of
   *     <code>Chain</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def indexOfSlice[U >: T](that: Chain[U], from: Int): Int = underlying.indexOfSlice(that.toList, from)

  /**
   * Finds index of the first element satisfying some predicate.
   *
   * @param p the predicate used to test elements.
   * @return the index of the first element of this <code>Chain</code> that satisfies the predicate <code>p</code>,
   *     or <code>-1</code>, if none exists.
   */ 
  final def indexWhere(p: T => Boolean): Int = underlying.indexWhere(p)

  /**
   * Finds index of the first element satisfying some predicate after or at some start index.
   *
   * @param p the predicate used to test elements.
   * @param from the start index
   * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>Chain</code> that satisfies the predicate <code>p</code>,
   *     or <code>-1</code>, if none exists.
   */ 
  final def indexWhere(p: T => Boolean, from: Int): Int = underlying.indexWhere(p, from)

  /**
   * Produces the range of all indices of this <code>Chain</code>. 
   *
   * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>Chain</code>. 
   */
  final def indices: Range = underlying.indices

  /**
   * Tests whether this <code>Chain</code> contains given index.
   *
   * @param idx the index to test
   * @return true if this <code>Chain</code> contains an element at position <code>idx</code>, <code>false</code> otherwise. 
   */
  final def isDefinedAt(idx: Int): Boolean = underlying.isDefinedAt(idx)

  /**
   * Returns <code>false</code> to indicate this <code>Chain</code>, like all <code>Chain<code>s, is non-empty.
   *
   * @return false
   */
  final def isEmpty: Boolean = false

  /**
   * Returns <code>true</code> to indicate this <code>Chain</code>, like all <code>Chain</code>s, can be traversed repeatedly.
   *
   * @return true
   */
  final def isTraversableAgain: Boolean = true

  /**
   * Creates and returns a new iterator over all elements contained in this <code>Chain</code>.
   *
   * @return the new iterator
   */
  final def iterator: Iterator[T] = underlying.iterator

  /**
   * Selects the last element of this <code>Chain</code>. 
   *
   * @return the last element of this <code>Chain</code>.
   */
  final def last: T = underlying.last

  /**
   * Finds the index of the last occurrence of some value in this <code>Chain</code>.
   *
   * @param elem the element value to search for.
   * @return the index of the last element of this <code>Chain</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
   *     or <code>-1</code>, if none exists.
   */
  final def lastIndexOf[U >: T](elem: U): Int = underlying.lastIndexOf(elem)

  /**
   * Finds the index of the last occurrence of some value in this <code>Chain</code> before or at a given <code>end</code> index.
   *
   * @param elem the element value to search for.
   * @param end the end index. 
   * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>Chain</code> that is equal (as determined by <code>==</code>)
   *     to <code>elem</code>, or <code>-1</code>, if none exists.
   */
  final def lastIndexOf[U >: T](elem: U, end: Int): Int = underlying.lastIndexOf(elem, end)

  /**
   * Finds the last index where this <code>Chain</code> contains a given <code>GenSeq</code> as a slice. 
   *
   * @param that the <code>GenSeq</code> defining the slice to look for
   * @return the last index at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: GenSeq[U]): Int = underlying.lastIndexOfSlice(that)

  /**
   * Finds the last index before or at a given end index where this <code>Chain</code> contains a given <code>GenSeq</code> as a slice. 
   *
   * @param that the <code>GenSeq</code> defining the slice to look for
   * @param end the end index
   * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>GenSeq</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: GenSeq[U], end: Int): Int = underlying.lastIndexOfSlice(that, end)

  /**
   * Finds the last index where this <code>Chain</code> contains a given <code>Every</code> as a slice. 
   *
   * @param that the <code>Every</code> defining the slice to look for
   * @return the last index at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: Every[U]): Int = underlying.lastIndexOfSlice(that.toVector)

  /**
   * Finds the last index where this <code>Chain</code> contains a given <code>Chain</code> as a slice. 
   *
   * @param that the <code>Chain</code> defining the slice to look for
   * @return the last index at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>Chain</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: Chain[U]): Int = underlying.lastIndexOfSlice(that.toList)

  /**
   * Finds the last index before or at a given end index where this <code>Chain</code> contains a given <code>Every</code> as a slice. 
   *
   * @param that the <code>Every</code> defining the slice to look for
   * @param end the end index
   * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: Every[U], end: Int): Int = underlying.lastIndexOfSlice(that.toVector, end)

  /**
   * Finds the last index before or at a given end index where this <code>Chain</code> contains a given <code>Chain</code> as a slice. 
   *
   * @param that the <code>Chain</code> defining the slice to look for
   * @param end the end index
   * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>Chain</code> starting at that index match the elements of
   *    <code>Chain</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
   */
  final def lastIndexOfSlice[U >: T](that: Chain[U], end: Int): Int = underlying.lastIndexOfSlice(that.toList, end)

  /**
   * Finds index of last element satisfying some predicate.
   *
   * @param p the predicate used to test elements.
   * @return the index of the last element of this <code>Chain</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists. 
   */
  final def lastIndexWhere(p: T => Boolean): Int = underlying.lastIndexWhere(p)

  /**
   * Finds index of last element satisfying some predicate before or at given end index.
   *
   * @param p the predicate used to test elements.
   * @param end the end index
   * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>Chain</code> that satisfies the predicate <code>p</code>,
   *     or <code>-1</code>, if none exists. 
   */
  final def lastIndexWhere(p: T => Boolean, end: Int): Int = underlying.lastIndexWhere(p, end)

  /**
   * Returns the last element of this <code>Chain</code>, wrapped in a <code>Some</code>. 
   *
   * @return the last element, wrapped in a <code>Some</code>. 
   */
  final def lastOption: Option[T] = underlying.lastOption // Will always return a Some

  /**
   * The length of this <code>Chain</code>.
   *
   * <p>
   * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
   * </p>
   *
   * @return the number of elements in this <code>Chain</code>. 
   */
  final def length: Int = underlying.length

  /**
   * Compares the length of this <code>Chain</code> to a test value. 
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
  final def lengthCompare(len: Int): Int = underlying.lengthCompare(len)

  /**
   * Builds a new <code>Chain</code> by applying a function to all elements of this <code>Chain</code>.
   *
   * @tparam U the element type of the returned <code>Chain</code>.
   * @param f the function to apply to each element. 
   * @return a new <code>Chain</code> resulting from applying the given function <code>f</code> to each element of this <code>Chain</code> and collecting the results. 
   */
  final def map[U](f: T => U): Chain[U] =
    new Chain(underlying.map(f))

  /**
   * Finds the largest element.
   *
   * @return the largest element of this <code>Chain</code>. 
   */
  final def max[U >: T](implicit cmp: Ordering[U]): T = underlying.max(cmp)

  /**
   * Finds the largest result after applying the given function to every element.
   *
   * @return the largest result of applying the given function to every element of this <code>Chain</code>. 
   */
  final def maxBy[U](f: T => U)(implicit cmp: Ordering[U]): T = underlying.maxBy(f)(cmp)

  /**
   * Finds the smallest element.
   *
   * @return the smallest element of this <code>Chain</code>. 
   */
  final def min[U >: T](implicit cmp: Ordering[U]): T = underlying.min(cmp)

  /**
   * Finds the smallest result after applying the given function to every element.
   *
   * @return the smallest result of applying the given function to every element of this <code>Chain</code>. 
   */
  final def minBy[U](f: T => U)(implicit cmp: Ordering[U]): T = underlying.minBy(f)(cmp)

  /**
   * Displays all elements of this <code>Chain</code> in a string. 
   *
   * @return a string representation of this <code>Chain</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
   *     <code>Chain</code> follow each other without any separator string. 
   */
  final def mkString: String = underlying.mkString

  /**
   * Displays all elements of this <code>Chain</code> in a string using a separator string. 
   *
   * @param sep the separator string
   * @return a string representation of this <code>Chain</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
   *     <code>Chain</code> are separated by the string <code>sep</code>. 
   */
  final def mkString(sep: String): String = underlying.mkString(sep)

  /**
   * Displays all elements of this <code>Chain</code> in a string using start, end, and separator strings. 
   *
   * @param start the starting string.
   * @param sep the separator string.
   * @param end the ending string.
   * @return a string representation of this <code>Chain</code>. The resulting string begins with the string <code>start</code> and ends with the string
   *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>Chain</code> are
   *     separated by the string <code>sep</code>. 
   */
  final def mkString(start: String, sep: String, end: String): String = underlying.mkString(start, sep, end)

  /**
   * Returns <code>true</code> to indicate this <code>Chain</code>, like all <code>Chain</code>s, is non-empty.
   *
   * @return true
   */
  final def nonEmpty: Boolean = true

  /**
   * A copy of this <code>Chain</code> with an element value appended until a given target length is reached.
   *
   * @param len the target length 
   * @param elem he padding value
   * @return a new <code>Chain</code> consisting of all elements of this <code>Chain</code> followed by the minimal number of occurrences
   *     of <code>elem</code> so that the resulting <code>Chain</code> has a length of at least <code>len</code>. 
   */
  final def padTo[U >: T](len: Int, elem: U): Chain[U] =
    new Chain(underlying.padTo(len, elem))

  /**
   * Produces a new <code>Chain</code> where a slice of elements in this <code>Chain</code> is replaced by another <code>Chain</code>
   *
   * @param from the index of the first replaced element 
   * @param that the <code>Chain</code> whose elements should replace a slice in this <code>Chain</code>
   * @param replaced the number of elements to drop in the original <code>Chain</code>
   */
  final def patch[U >: T](from: Int, that: Chain[U], replaced: Int): Chain[U] =
    new Chain(underlying.patch(from, that.toVector, replaced))

  /**
   * Iterates over distinct permutations. 
   *
   * <p>
   * Here's an example:
   * </p>
   *
   * <pre class="stHighlight">
   * Chain('a', 'b', 'b').permutations.toList = List(Chain(a, b, b), Chain(b, a, b), Chain(b, b, a))
   * </pre>
   *
   * @return an iterator that traverses the distinct permutations of this <code>Chain</code>.
   */
  final def permutations: Iterator[Chain[T]] = {
    val it = underlying.permutations
    it map { list => new Chain(list) }
  }

  /**
   * Returns the length of the longest prefix whose elements all satisfy some predicate.
   *
   * @param p the predicate used to test elements.
   * @return the length of the longest prefix of this <code>Chain</code> such that every element
   *     of the segment satisfies the predicate <code>p</code>. 
   */
  final def prefixLength(p: T => Boolean): Int = underlying.prefixLength(p)

  /**
   * The result of multiplying all the elements of this <code>Chain</code>.
   *
   * <p>
   * This method can be invoked for any <code>Chain[T]</code> for which an implicit <code>Numeric[T]</code> exists.
   * </p>
   *
   * @return the product of all elements
   */
  final def product[U >: T](implicit num: Numeric[U]): U = underlying.product(num)

  /**
   * Reduces the elements of this <code>Chain</code> using the specified associative binary operator.
   *
   * <p>
   * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
   * </p>
   *
   * @tparam U a type parameter for the binary operator, a supertype of T.
   * @param op a binary operator that must be associative.
   * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>Chain</code>.
   */
  final def reduce[U >: T](op: (U, U) => U): U = underlying.reduce(op)

  /**
   * Applies a binary operator to all elements of this <code>Chain</code>, going left to right.
   *
   * @tparam U the result type of the binary operator.
   * @param op the binary operator.
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going left to right:
   *
   * <pre>
   * op(...op(op(x_1, x_2), x_3), ..., x_n)
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */
  final def reduceLeft[U >: T](op: (U, T) => U): U = underlying.reduceLeft(op)

  /**
   * Applies a binary operator to all elements of this <code>Chain</code>, going left to right, returning the result in a <code>Some</code>.
   *
   * @tparam U the result type of the binary operator.
   * @param op the binary operator.
   * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
   * </p>
   */
  final def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = underlying.reduceLeftOption(op)

  final def reduceOption[U >: T](op: (U, U) => U): Option[U] = underlying.reduceOption(op)

  /**
   * Applies a binary operator to all elements of this <code>Chain</code>, going right to left.
   *
   * @tparam U the result of the binary operator
   * @param op the binary operator
   * @return the result of inserting <code>op</code> between consecutive elements of this <code>Chain</code>, going right to left:
   *
   * <pre>
   * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
   * </pre>
   *
   * <p>
   * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>Chain</code>. 
   * </p>
   */
  final def reduceRight[U >: T](op: (T, U) => U): U = underlying.reduceRight(op)

  /**
   * Applies a binary operator to all elements of this <code>Chain</code>, going right to left, returning the result in a <code>Some</code>.
   *
   * @tparam U the result of the binary operator
   * @param op the binary operator
   * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
   */
  final def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = underlying.reduceRightOption(op)

  /**
   * Returns new <code>Chain</code> with elements in reverse order.
   *
   * @return a new <code>Chain</code> with all elements of this <code>Chain</code> in reversed order. 
   */
  final def reverse: Chain[T] =
    new Chain(underlying.reverse)

  /**
   * An iterator yielding elements in reverse order.
   *
   * <p>
   * Note: <code>chain.reverseIterator</code> is the same as <code>chain.reverse.iterator</code>, but might be more efficient. 
   * </p>
   *
   * @return an iterator yielding the elements of this <code>Chain</code> in reversed order 
   */
  final def reverseIterator: Iterator[T] = underlying.reverseIterator

  /**
   * Builds a new <code>Chain</code> by applying a function to all elements of this <code>Chain</code> and collecting the results in reverse order.
   *
   * <p>
   * Note: <code>chain.reverseMap(f)</code> is the same as <code>chain.reverse.map(f)</code>, but might be more efficient. 
   * </p>
   *
   * @tparam U the element type of the returned <code>Chain</code>.
   * @param f the function to apply to each element. 
   * @return a new <code>Chain</code> resulting from applying the given function <code>f</code> to each element of this <code>Chain</code>
   *     and collecting the results in reverse order. 
   */
  final def reverseMap[U](f: T => U): Chain[U] =
    new Chain(underlying.reverseMap(f))

  /**
   * Checks if the given <code>GenIterable</code> contains the same elements in the same order as this <code>Chain</code>.
   *
   * @param that the <code>GenIterable</code> with which to compare
   * @return <code>true</code>, if both this <code>Chain</code> and the given <code>GenIterable</code> contain the same elements
   *     in the same order, <code>false</code> otherwise. 
   */
  final def sameElements[U >: T](that: GenIterable[U]): Boolean = underlying.sameElements(that)

  /**
   * Checks if the given <code>Every</code> contains the same elements in the same order as this <code>Chain</code>.
   *
   * @param that the <code>Every</code> with which to compare
   * @return <code>true</code>, if both this and the given <code>Every</code> contain the same elements
   *     in the same order, <code>false</code> otherwise. 
   */
  final def sameElements[U >: T](that: Every[U]): Boolean = underlying.sameElements(that.toVector)

  /**
   * Checks if the given <code>Chain</code> contains the same elements in the same order as this <code>Chain</code>.
   *
   * @param that the <code>Chain</code> with which to compare
   * @return <code>true</code>, if both this and the given <code>Chain</code> contain the same elements
   *     in the same order, <code>false</code> otherwise. 
   */
  final def sameElements[U >: T](that: Chain[U]): Boolean = underlying.sameElements(that.toList)

  /**
   * Computes a prefix scan of the elements of this <code>Chain</code>.
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
   * Chain(1, 2, 3).scan(0)(_ + _) == Chain(0, 1, 3, 6)
   * Chain(1, 2, 3).scan("z")(_ + _.toString) == Chain("z", "z1", "z12", "z123")
   * </pre>
   *
   * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>Chain</code>.
   * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
   *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
   *     0 for addition, or 1 for multiplication.)
   * @param op a binary operator that must be associative
   * @return a new <code>Chain</code> containing the prefix scan of the elements in this <code>Chain</code> 
   */
  final def scan[U >: T](z: U)(op: (U, U) => U): Chain[U] = new Chain(underlying.scan(z)(op))

  /**
   * Produces a <code>Chain</code> containing cumulative results of applying the operator going left to right.
   *
   * <p>
   * Here are some examples:
   * </p>
   *
   * <pre class="stHighlight">
   * Chain(1, 2, 3).scanLeft(0)(_ + _) == Chain(0, 1, 3, 6)
   * Chain(1, 2, 3).scanLeft("z")(_ + _) == Chain("z", "z1", "z12", "z123")
   * </pre>
   *
   * @tparam B the result type of the binary operator and type of the resulting <code>Chain</code>
   * @param z the start value.
   * @param op the binary operator.
   * @return a new <code>Chain</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>Chain</code>,
   *     going left to right, with the start value, <code>z</code>, on the left.
   */ 
  final def scanLeft[B](z: B)(op: (B, T) => B): Chain[B] = new Chain(underlying.scanLeft(z)(op))

  /**
   * Produces a <code>Chain</code> containing cumulative results of applying the operator going right to left.
   *
   * <p>
   * Here are some examples:
   * </p>
   *
   * <pre class="stHighlight">
   * Chain(1, 2, 3).scanRight(0)(_ + _) == Chain(6, 5, 3, 0)
   * Chain(1, 2, 3).scanRight("z")(_ + _) == Chain("123z", "23z", "3z", "z")
   * </pre>
   *
   * @tparam B the result of the binary operator and type of the resulting <code>Chain</code>
   * @param z the start value
   * @param op the binary operator
   * @return a new <code>Chain</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>Chain</code>,
   *     going right to left, with the start value, <code>z</code>, on the right.
   */
  final def scanRight[B](z: B)(op: (T, B) => B): Chain[B] = new Chain(underlying.scanRight(z)(op))

  /**
   * Computes length of longest segment whose elements all satisfy some predicate.
   *
   * @param p the predicate used to test elements.
   * @param from the index where the search starts.
   * @param the length of the longest segment of this <code>Chain</code> starting from index <code>from</code> such that every element of the
   *     segment satisfies the predicate <code>p</code>. 
   */
  final def segmentLength(p: T => Boolean, from: Int): Int = underlying.segmentLength(p, from)

  /**
   * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
   *
   * @param size the number of elements per group
   * @return an iterator producing <code>Chain</code>s of size <code>size</code>, except the last and the only element will be truncated
   *     if there are fewer elements than <code>size</code>.
   */
  final def sliding(size: Int): Iterator[Chain[T]] = underlying.sliding(size).map(new Chain(_))

  /**
   * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
   * moving the sliding window by a given <code>step</code> each time.
   *
   * @param size the number of elements per group
   * @param step the distance between the first elements of successive groups
   * @return an iterator producing <code>Chain</code>s of size <code>size</code>, except the last and the only element will be truncated
   *     if there are fewer elements than <code>size</code>.
   */
  final def sliding(size: Int, step: Int): Iterator[Chain[T]] = underlying.sliding(size, step).map(new Chain(_))

  /**
   * The size of this <code>Chain</code>.
   *
   * <p>
   * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
   * </p>
   *
   * @return the number of elements in this <code>Chain</code>. 
   */
  final def size: Int = underlying.size

  /**
   * Sorts this <code>Chain</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
   *
   * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
   * @param f the transformation function mapping elements to some other domain <code>U</code>.
   * @param ord the ordering assumed on domain <code>U</code>.
   * @return a <code>Chain</code> consisting of the elements of this <code>Chain</code> sorted according to the <code>Ordering</code> where
   *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
   */
  final def sortBy[U](f: T => U)(implicit ord: Ordering[U]): Chain[T] = new Chain(underlying.sortBy(f))

  /**
   * Sorts this <code>Chain</code> according to a comparison function.
   *
   * <p>
   * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
   * sorted <code>Chain</code> as in the original. 
   * </p>
   *
   * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
   * @return a <code>Chain</code> consisting of the elements of this <code>Chain</code> sorted according to the comparison function <code>lt</code>.
   */
  final def sortWith(lt: (T, T) => Boolean): Chain[T] = new Chain(underlying.sortWith(lt))

  /**
   * Sorts this <code>Chain</code> according to an <code>Ordering</code>.
   *
   * <p>
   * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
   * sorted <code>Chain</code> as in the original. 
   * </p>
   *
   * @param ord the <code>Ordering</code> to be used to compare elements.
   * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
   * @return a <code>Chain</code> consisting of the elements of this <code>Chain</code> sorted according to the comparison function <code>lt</code>.
   */
  final def sorted[U >: T](implicit ord: Ordering[U]): Chain[U] = new Chain(underlying.sorted(ord))

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>GenSeq</code>. 
   *
   * @param that the <code>GenSeq</code> slice to look for in this <code>Chain</code>
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
   */
  final def startsWith[B](that: GenSeq[B]): Boolean = underlying.startsWith(that)

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>GenSeq</code> at the given index. 
   *
   * @param that the <code>GenSeq</code> slice to look for in this <code>Chain</code>
   * @param offset the index at which this <code>Chain</code> is searched.
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
   */
  final def startsWith[B](that: GenSeq[B], offset: Int): Boolean = underlying.startsWith(that, offset)

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>Every</code>. 
   *
   * @param that the <code>Every</code> to test
   * @return <code>true</code> if this collection has <code>that</code> as a prefix, <code>false</code> otherwise.
   */
  final def startsWith[B](that: Every[B]): Boolean = underlying.startsWith(that.toVector)

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>Chain</code>. 
   *
   * @param that the <code>Chain</code> to test
   * @return <code>true</code> if this collection has <code>that</code> as a prefix, <code>false</code> otherwise.
   */
  final def startsWith[B](that: Chain[B]): Boolean = underlying.startsWith(that.toList)

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>Every</code> at the given index. 
   *
   * @param that the <code>Every</code> slice to look for in this <code>Chain</code>
   * @param offset the index at which this <code>Chain</code> is searched.
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
   */
  final def startsWith[B](that: Every[B], offset: Int): Boolean = underlying.startsWith(that.toVector, offset)

  /**
   * Indicates whether this <code>Chain</code> starts with the given <code>Chain</code> at the given index. 
   *
   * @param that the <code>Chain</code> slice to look for in this <code>Chain</code>
   * @param offset the index at which this <code>Chain</code> is searched.
   * @return <code>true</code> if this <code>Chain</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
   */
  final def startsWith[B](that: Chain[B], offset: Int): Boolean = underlying.startsWith(that.toList, offset)

  /**
   * Returns <code>"Chain"</code>, the prefix of this object's <code>toString</code> representation.
   *
   * @return the string <code>"Chain"</code>
   */
  def stringPrefix: String = "Chain"

  /**
   * The result of summing all the elements of this <code>Chain</code>.
   *
   * <p>
   * This method can be invoked for any <code>Chain[T]</code> for which an implicit <code>Numeric[T]</code> exists.
   * </p>
   *
   * @return the sum of all elements
   */
  final def sum[U >: T](implicit num: Numeric[U]): U = underlying.sum(num)

  import scala.language.higherKinds

  /**
   * Converts this <code>Chain</code> into a collection of type <code>Col</code> by copying all elements.
   *
   * @tparam Col the collection type to build.
   * @return a new collection containing all elements of this <code>Chain</code>. 
   */
  final def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, T, Col[T @uV]]): Col[T @uV] = underlying.to[Col](cbf)

  /**
   * Converts this <code>Chain</code> to an array.
   *
   * @return an array containing all elements of this <code>Chain</code>. A <code>ClassTag</code> must be available for the element type of this <code>Chain</code>. 
   */ 
  final def toArray[U >: T](implicit classTag: ClassTag[U]): Array[U] = underlying.toArray

  /**
   * Converts this <code>Chain</code> to a <code>Vector</code>.
   *
   * @return a <code>Vector</code> containing all elements of this <code>Chain</code>. 
   */ 
  final def toVector: Vector[T] = underlying.toVector

  /**
   * Converts this <code>Chain</code> to a mutable buffer.
   *
   * @return a buffer containing all elements of this <code>Chain</code>. 
   */ 
  final def toBuffer[U >: T]: Buffer[U] = underlying.toBuffer

  /**
   * Converts this <code>Chain</code> to an immutable <code>IndexedSeq</code>.
   *
   * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>Chain</code>. 
   */ 
  final def toIndexedSeq: collection.immutable.IndexedSeq[T] = underlying.toVector

  /**
   * Converts this <code>Chain</code> to an iterable collection.
   *
   * @return an <code>Iterable</code> containing all elements of this <code>Chain</code>. 
   */ 
  final def toIterable: Iterable[T] = underlying.toIterable

  /**
   * Returns an <code>Iterator</code> over the elements in this <code>Chain</code>.
   *
   * @return an <code>Iterator</code> containing all elements of this <code>Chain</code>. 
   */ 
  final def toIterator: Iterator[T] = underlying.toIterator

  /**
   * Converts this <code>Chain</code> to a list.
   *
   * @return a list containing all elements of this <code>Chain</code>. 
   */ 
  final def toList: List[T] = underlying

  /**
   * Converts this <code>Chain</code> to a map.
   *
   * <p>
   * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
   * in the map. Duplicate keys will be overwritten by later keys.
   * </p>
   *
   * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>Chain</code>. 
   */ 
  final def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.toMap

  /**
   * Converts this <code>Chain</code> to an immutable <code>IndexedSeq</code>.
   *
   * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>Chain</code>.
   */ 
  final def toSeq: collection.immutable.Seq[T] = underlying

  /**
   * Converts this <code>Chain</code> to a set.
   *
   * @return a set containing all elements of this <code>Chain</code>. 
   */ 
  final def toSet[U >: T]: Set[U] = underlying.toSet

  /**
   * Converts this <code>Chain</code> to a stream.
   *
   * @return a stream containing all elements of this <code>Chain</code>. 
   */ 
  final def toStream: Stream[T] = underlying.toStream

  /**
   * Returns a string representation of this <code>Chain</code>.
   *
   * @return the string <code>"Chain"</code> followed by the result of invoking <code>toString</code> on
   *   this <code>Chain</code>'s elements, surrounded by parentheses.
   */
  override def toString: String = "Chain(" + toList.mkString(", ") + ")"

  /**
   * Converts this <code>Chain</code> to an unspecified Traversable.
   *
   * @return a <code>Traversable</code> containing all elements of this <code>Chain</code>. 
   */ 
  final def toTraversable: Traversable[T] = underlying.toTraversable

  final def transpose[U](implicit ev: T <:< Chain[U]): Chain[Chain[U]] = {
    val asLists = underlying.map(ev)
    val list = asLists.transpose
    new Chain(list.map(new Chain(_)))
  }

  /**
   * Produces a new <code>Chain</code> that contains all elements of this <code>Chain</code> and also all elements of a given <code>Every</code>.
   *
   * <p>
   * <code>chainX</code> <code>union</code> <code>everyY</code> is equivalent to <code>chainX</code> <code>++</code> <code>everyY</code>.
   * </p>
   *
   * <p>
   * Another way to express this is that <code>chainX</code> <code>union</code> <code>everyY</code> computes the order-presevring multi-set union
   * of <code>chainX</code> and <code>everyY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
   * also work on multi-sets.
   * </p>
   *
   * @param that the <code>Every</code> to add.
   * @return a new <code>Chain</code> that contains all elements of this <code>Chain</code> followed by all elements of <code>that</code> <code>Every</code>.
   */
  final def union[U >: T](that: Every[U]): Chain[U] = new Chain(underlying union that.toVector)

  /**
   * Produces a new <code>Chain</code> that contains all elements of this <code>Chain</code> and also all elements of a given <code>Chain</code>.
   *
   * <p>
   * <code>chainX</code> <code>union</code> <code>chainY</code> is equivalent to <code>chainX</code> <code>++</code> <code>chainY</code>.
   * </p>
   *
   * <p>
   * Another way to express this is that <code>chainX</code> <code>union</code> <code>chainY</code> computes the order-presevring multi-set union
   * of <code>chainX</code> and <code>chainY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
   * also work on multi-sets.
   * </p>
   *
   * @param that the <code>Chain</code> to add.
   * @return a new <code>Chain</code> that contains all elements of this <code>Chain</code> followed by all elements of <code>that</code>.
   */
  final def union[U >: T](that: Chain[U]): Chain[U] = new Chain(underlying union that.toList)

  /**
   * Produces a new <code>Chain</code> that contains all elements of this <code>Chain</code> and also all elements of a given <code>GenSeq</code>.
   *
   * <p>
   * <code>chainX</code> <code>union</code> <code>ys</code> is equivalent to <code>chainX</code> <code>++</code> <code>ys</code>.
   * </p>
   *
   * <p>
   * Another way to express this is that <code>chainX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
   * of <code>chainX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
   * also work on multi-sets.
   * </p>
   *
   * @param that the <code>GenSeq</code> to add.
   * @return a new <code>Chain</code> that contains all elements of this <code>Chain</code> followed by all elements of <code>that</code> <code>GenSeq</code>.
   */
  final def union[U >: T](that: GenSeq[U])(implicit cbf: CanBuildFrom[List[T], U, List[U]]): Chain[U] = new Chain(underlying.union(that)(cbf))

  /**
   * Converts this <code>Chain</code> of pairs into two <code>Chain</code>s of the first and second half of each pair. 
   *
   * @tparam L the type of the first half of the element pairs
   * @tparam R the type of the second half of the element pairs
   * @param asPair an implicit conversion that asserts that the element type of this <code>Chain</code> is a pair.
   * @return a pair of <code>Chain</code>s, containing the first and second half, respectively, of each element pair of this <code>Chain</code>. 
   */
  final def unzip[L, R](implicit asPair: T => (L, R)): (Chain[L], Chain[R]) = {
    val unzipped = underlying.unzip
    (new Chain(unzipped._1), new Chain(unzipped._2))
  }

  /**
   * Converts this <code>Chain</code> of triples into three <code>Chain</code>s of the first, second, and and third element of each triple. 
   *
   * @tparam L the type of the first member of the element triples
   * @tparam R the type of the second member of the element triples
   * @tparam R the type of the third member of the element triples
   * @param asTriple an implicit conversion that asserts that the element type of this <code>Chain</code> is a triple.
   * @return a triple of <code>Chain</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>Chain</code>. 
   */
  final def unzip3[L, M, R](implicit asTriple: T => (L, M, R)): (Chain[L], Chain[M], Chain[R]) = {
    val unzipped = underlying.unzip3
    (new Chain(unzipped._1), new Chain(unzipped._2), new Chain(unzipped._3))
  }

  /**
   * A copy of this <code>Chain</code> with one single replaced element.
   *
   * @param idx the position of the replacement
   * @param elem the replacing element
   * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>Chain<code>
   * @return a copy of this <code>Chain</code> with the element at position <code>idx</code> replaced by <code>elem</code>. 
   */
  final def updated[U >: T](idx: Int, elem: U): Chain[U] =
    try new Chain(underlying.updated(idx, elem))
    catch { case _: UnsupportedOperationException => throw new IndexOutOfBoundsException(idx.toString) } // This is needed for 2.10 support. Can drop after.
                                                                                                         // Because 2.11 throws IndexOutOfBoundsException.

  /**
   * Returns a <code>Chain</code> formed from this <code>Chain</code> and an iterable collection by combining corresponding
   * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
   * shorter collection to the length of the longer.
   *
   * @tparm O the type of the second half of the returned pairs
   * @tparm U the type of the first half of the returned pairs
   * @param other the <code>Iterable</code> providing the second half of each result pair
   * @param thisElem the element to be used to fill up the result if this <code>Chain</code> is shorter than <code>that</code> <code>Iterable</code>.
   * @param thatElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>Chain</code>.
   * @return a new <code>Chain</code> containing pairs consisting of corresponding elements of this <code>Chain</code> and <code>that</code>. The
   *     length of the returned collection is the maximum of the lengths of this <code>Chain</code> and <code>that</code>. If this <code>Chain</code>
   *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
   *     <code>Chain</code>, <code>thatElem</code> values are used to pad the result. 
   */
  final def zipAll[O, U >: T](other: collection.Iterable[O], thisElem: U, otherElem: O): Chain[(U, O)] =
    new Chain(underlying.zipAll(other, thisElem, otherElem))

  /**
   * Zips this <code>Chain</code>  with its indices.
   *
   * @return A new <code>Chain</code> containing pairs consisting of all elements of this <code>Chain</code> paired with their index. Indices start at 0.
   */
  final def zipWithIndex: Chain[(T, Int)] = new Chain(underlying.zipWithIndex)
}

/**
 * Companion object for class <code>Chain</code>.
 */
object Chain {

  /**
   * Constructs a new <code>Chain</code> given at least one element.
   *
   * @tparam T the type of the element contained in the new <code>Chain</code>
   * @param firstElement the first element (with index 0) contained in this <code>Chain</code>
   * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>Chain</code>
   */
  def apply[T](firstElement: T, otherElements: T*): Chain[T] = new Chain(firstElement :: otherElements.toList)

  /**
   * Variable argument extractor for <code>Chain</code>s.
   *
   * @param chain: the <code>Chain</code> containing the elements to extract
   * @return an <code>Seq</code> containing this <code>Chain</code>s elements, wrapped in a <code>Some</code> 
   */
  def unapplySeq[T](chain: Chain[T]): Option[Seq[T]] = Some(chain.toList)

/*
  // TODO: Figure out how to get case Chain() to not compile
  def unapplySeq[T](chain: Chain[T]): Option[(T, Seq[T])] = Some(chain.head, chain.tail)
*/

  /**
   * Optionally construct a <code>Chain</code> containing the elements, if any, of a given <code>GenSeq</code>.
   *
   * @param seq the <code>GenSeq</code> with which to construct a <code>Chain</code>
   * @return a <code>Chain</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
   *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
   */
  def from[T](seq: GenSeq[T]): Option[Chain[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some(new Chain(first :: seq.tail.toList))
    }

  import scala.language.implicitConversions

  /**
   * Implicit conversion from <code>Chain</code> to <code>List</code>.
   *
   * <p>
   * One use case for this implicit conversion is to enable <code>GenSeq[Chain]</code>s to be flattened.
   * Here's an example:
   * </p>
   *
   * <pre class="stREPL">
   * scala&gt; Vector(Chain(1, 2, 3), Chain(3, 4), Chain(5, 6, 7, 8)).flatten
   * res0: scala.collection.immutable.Vector[Int] = Vector(1, 2, 3, 3, 4, 5, 6, 7, 8)
   * </pre>
   *
   * @param chain the <code>Chain</code> to convert to a <code>List</code>
   * @return a <code>List</code> containing the elements, in order, of this <code>Chain</code>
   */
  implicit def chainToList[E](chain: Chain[E]): scala.collection.immutable.List[E] = chain.toList
}

/**
 * Object that can be used as an endpoint for <code>Chain</code> construction expressions
 * that use the cons (<code>::</code>) operator.
 *
 * <p>
 * Here's an example:
 * </p>
 * 
 * <pre class="REPL">
 * scala> 1 :: 2 :: 3 :: End
 * res0: org.scalactic.Chain[Int] = Chain(1, 2, 3)
 * </pre>
 *
 * <p>
 * Note that unlike <code>Nil</code>, which is an instance of <code>List[Nothing]</code>,
 * <code>End</code> is not an instance of <code>Chain[Nothing]</code>, because there is
 * no empty <code>Chain</code>:
 * </p>
 * 
 * <pre class="REPL">
 * scala> Nil.isInstanceOf[List[_]]
 * res0: Boolean = true
 *
 * scala> End.isInstanceOf[Chain[_]]
 * res1: Boolean = false
 * </pre>
 */
object End {
  /**
   * A <code>::</code> operator that serves to start a <code>Chain</code> construction
   * expression.
   *
   * <p>
   * The result of calling this method will always be a <code>Chain</code> of length 1.
   * Here's an example:
   * </p>
   *
   * <pre class="REPL">
   * scala> 1 :: End
   * res0: org.scalactic.Chain[Int] = Chain(1)
   * </pre>
   */
  def ::[T](element: T): Chain[T] = Chain(element)

  /**
   * Returns <code>"End"</code>.
   */
  override def toString: String = "End"
}

