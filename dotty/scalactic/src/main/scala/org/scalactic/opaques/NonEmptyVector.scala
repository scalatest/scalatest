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
import scala.collection.mutable.Buffer

/**
  * A non-empty list: an ordered, immutable, non-empty collection of elements with <code>LinearSeq</code> performance characteristics.
  *
  * <p>
  * The purpose of <code>NonEmptyVector</code> is to allow you to express in a type that a <code>Vector</code> is non-empty, thereby eliminating the
  * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty sequence with <code>IndexedSeq</code>
  * performance, see <a href="Every.html"><code>Every</code></a>.
  * </p>
  *
  * <h2>Constructing <code>NonEmptyVector</code>s</h2>
  *
  * <p>
  * You can construct a <code>NonEmptyVector</code> by passing one or more elements to the <code>NonEmptyVector.apply</code> factory method:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; NonEmptyVector(1, 2, 3)
  * res0: org.scalactic.anyvals.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3)
  * </pre>
  *
  * <p>
  * Alternatively you can <em>cons</em> elements onto the <code>End</code> singleton object, similar to making a <code>Vector</code> starting with <code>Nil</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; 1 :: 2 :: 3 :: Nil
  * res0: Vector[Int] = Vector(1, 2, 3)
  *
  * scala&gt; 1 :: 2 :: 3 :: End
  * res1: org.scalactic.NonEmptyVector[Int] = NonEmptyVector(1, 2, 3)
  * </pre>
  *
  * <p>
  * Note that although <code>Nil</code> is a <code>Vector[Nothing]</code>, <code>End</code> is
  * not a <code>NonEmptyVector[Nothing]</code>, because no empty <code>NonEmptyVector</code> exists. (A non-empty list is a series
  * of connected links; if you have no links, you have no non-empty list.)
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; val nil: Vector[Nothing] = Nil
  * nil: Vector[Nothing] = Vector()
  *
  * scala&gt; val nada: NonEmptyVector[Nothing] = End
  * &lt;console&gt;:16: error: type mismatch;
  * found   : org.scalactic.anyvals.End.type
  * required: org.scalactic.anyvals.NonEmptyVector[Nothing]
  *        val nada: NonEmptyVector[Nothing] = End
  *                                          ^
  * </pre>
  *
  * <h2>Working with <code>NonEmptyVector</code>s</h2>
  *
  * <p>
  * <code>NonEmptyVector</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
  * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
  * you'll get an empty <code>Seq</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Vector(1).tail
  * res6: Vector[Int] = Vector()
  * </pre>
  *
  * <p>
  * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
  * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptyVector</code> defines a method corresponding to every such <code>Seq</code>
  * method. Here are some examples:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyVector(1, 2, 3).map(_ + 1)                        // Result: NonEmptyVector(2, 3, 4)
  * NonEmptyVector(1).map(_ + 1)                              // Result: NonEmptyVector(2)
  * NonEmptyVector(1, 2, 3).containsSlice(NonEmptyVector(2, 3)) // Result: true
  * NonEmptyVector(1, 2, 3).containsSlice(NonEmptyVector(3, 4)) // Result: false
  * NonEmptyVector(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
  * </pre>
  *
  * <p>
  * <code>NonEmptyVector</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
  * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptyVector</code> to <code>Vector</code>
  * is defined in the <code>NonEmptyVector</code> companion object that will be applied if you attempt to call one of the missing methods. As a
  * result, you can invoke <code>filter</code> on an <code>NonEmptyVector</code>, even though <code>filter</code> could result
  * in an empty sequence&mdash;but the result type will be <code>Vector</code> instead of <code>NonEmptyVector</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyVector(1, 2, 3).filter(_ &lt; 10) // Result: Vector(1, 2, 3)
  * NonEmptyVector(1, 2, 3).filter(_ &gt; 10) // Result: Vector()
  * </pre>
  *
  *
  * <p>
  * You can use <code>NonEmptyVector</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyVector</code> unless
  * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
  * result type will switch to a <code>Vector</code> at that point. Here are some examples:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; import org.scalactic.anyvals._
  * import org.scalactic.anyvals._
  *
  * scala&gt; for (i &lt;- NonEmptyVector(1, 2, 3)) yield i + 1
  * res0: org.scalactic.anyvals.NonEmptyVector[Int] = NonEmptyVector(2, 3, 4)
  *
  * scala&gt; for (i &lt;- NonEmptyVector(1, 2, 3) if i &lt; 10) yield i + 1
  * res1: Vector[Int] = Vector(2, 3, 4)
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyVector(1, 2, 3)
  *      |   j &lt;- NonEmptyVector('a', 'b', 'c')
  *      | } yield (i, j)
  * res3: org.scalactic.anyvals.NonEmptyVector[(Int, Char)] =
  *         NonEmptyVector((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyVector(1, 2, 3) if i &lt; 10
  *      |   j &lt;- NonEmptyVector('a', 'b', 'c')
  *      | } yield (i, j)
  * res6: Vector[(Int, Char)] =
  *         Vector((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  * </pre>
  *
  * @tparam T the type of elements contained in this <code>NonEmptyVector</code>
  */
opaque type NonEmptyVector[T] = Vector[T]

/**
  * Companion object for class <code>NonEmptyVector</code>.
  */
object NonEmptyVector {
  
  /**
    * Constructs a new <code>NonEmptyVector</code> given at least one element.
    *
    * @tparam T the type of the element contained in the new <code>NonEmptyVector</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptyVector</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptyVector</code>
    */
  def apply[T](firstElement: T, otherElements: T*): NonEmptyVector[T] = Vector(firstElement) ++ otherElements

  /**
    * Variable argument extractor for <code>NonEmptyVector</code>s.
    *
    * @param nonEmptyVector: the <code>NonEmptyVector</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptyVector</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[T](nonEmptyVector: NonEmptyVector[T]): Option[Seq[T]] = Some(nonEmptyVector)

  /**
    * Optionally construct a <code>NonEmptyVector</code> containing the elements, if any, of a given <code>GenSeq</code>.
    *
    * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyVector</code>
    * @return a <code>NonEmptyVector</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
    *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
    */
  def from[T](seq: GenSeq[T]): Option[NonEmptyVector[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some(Vector(first) ++ seq.tail)
    }

  /**
    * Conversion from <code>NonEmptyVector</code> to <code>IterableOnce</code>.
    *
    * @param nonEmptyVector the <code>NonEmptyVector</code> to convert
    * @return the <code>IterableOnce</code>
    */
  given [E]: Conversion[NonEmptyVector[E], IterableOnce[E]] with {
    def apply(nonEmptyVector: NonEmptyVector[E]): IterableOnce[E] = nonEmptyVector
  }

  /**
    * Conversion from <code>NonEmptyVector</code> to <code>PartialFunction</code>.
    *
    * @param nonEmptyVector the <code>NonEmptyVector</code> to convert
    * @return the <code>PartialFunction</code>
    */
  given [E]: Conversion[NonEmptyVector[E], PartialFunction[Int, E]] with {
    def apply(nonEmptyVector: NonEmptyVector[E]): PartialFunction[Int, E] =
      new PartialFunction[Int, E] {
        def apply(i: Int): E = (nonEmptyVector: Vector[E]).apply(i)
        def isDefinedAt(i: Int): Boolean = i >= 0 && i < nonEmptyVector.length
      }
  }

  extension [T](element: T) {
    /**
      * Returns a new <code>NonEmptyVector</code> with the given element prepended.
      *
      * <p>
      * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to prepend to this <code>NonEmptyVector</code>
      * @return a new <code>NonEmptyVector</code> consisting of <code>element</code> followed by all elements of this <code>NonEmptyVector</code>.
      */
    infix def ::[U >: T](nonEmptyVector: NonEmptyVector[U]): NonEmptyVector[U] = 
      NonEmptyVector(element, nonEmptyVector*)

    /**
      * Returns a new <code>NonEmptyVector</code> with the given element prepended.
      *
      * <p>
      * Note that :-ending operators are right associative. A mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to prepend to this <code>NonEmptyVector</code>
      * @return a new <code>NonEmptyVector</code> consisting of <code>element</code> followed by all elements of this <code>NonEmptyVector</code>.
      */
    infix def +:[U >: T](nonEmptyVector: NonEmptyVector[U]): NonEmptyVector[U] = NonEmptyVector(element, nonEmptyVector*)
  }

  extension [T] (nonEmptyVector: NonEmptyVector[T]) {

    /**
      * Returns a new <code>NonEmptyVector</code> containing the elements of this <code>NonEmptyVector</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyVector</code> is the most specific superclass encompassing the element types of this <code>NonEmptyVector</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyVector</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyVector</code> that contains all the elements of this <code>NonEmptyVector</code> followed by all elements of <code>other</code>.
      */
    def ++[U >: T](other: IterableOnce[U]): NonEmptyVector[U] =
      if (other.isEmpty) nonEmptyVector else toVector ++ other

    /**
      * Returns a new <code>NonEmptyVector</code> with the given element appended.
      *
      * <p>
      * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to append to this <code>NonEmptyVector</code>
      * @return a new <code>NonEmptyVector</code> consisting of all elements of this <code>NonEmptyVector</code> followed by <code>element</code>.
      */
    def :+[U >: T](element: U): NonEmptyVector[U] = toVector :+ element

    /**
      * Appends all elements of this <code>NonEmptyVector</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptyVector</code>, without any separator string.
      *
      * @param sb the string builder to which elements will be appended
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder): StringBuilder = toVector.addString(sb)

    /**
      * Appends all elements of this <code>NonEmptyVector</code> to a string builder using a separator string. The written text will consist of a concatenation of the
      * result of invoking <code>toString</code>
      * on of every element of this <code>NonEmptyVector</code>, separated by the string <code>sep</code>.
      *
      * @param sb the string builder to which elements will be appended
      * @param sep the separator string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, sep: String): StringBuilder = toVector.addString(sb, sep)

    /**
      * Appends all elements of this <code>NonEmptyVector</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
      * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptyVector</code>,
      * separated by the string <code>sep</code>; and the string <code>end</code>
      *
      * @param sb the string builder to which elements will be appended
      * @param start the starting string
      * @param sep the separator string
      * @param start the ending string
      * @return the string builder, <code>sb</code>, to which elements were appended.
      */
    def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = toVector.addString(sb, start, sep, end)  

    /**
      * Finds the first element of this <code>NonEmptyVector</code> for which the given partial function is defined, if any, and applies the partial function to it.
      *
      * @param pf the partial function
      * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
      *    the partial function was not defined for any element.
      */
    def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = toVector.collectFirst(pf)

    /**
      * Indicates whether this <code>NonEmptyVector</code> contains a given value as an element.
      *
      * @param elem the element to look for
      * @return true if this <code>NonEmptyVector</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
      */
    def contains(elem: Any): Boolean = toVector.contains(elem)

    /**
      * Indicates whether this <code>NonEmptyVector</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> slice to look for
      * @return true if this <code>NonEmptyVector</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
      */
    def containsSlice[B](that: IterableOnce[B]): Boolean = toVector.toIndexedSeq.containsSlice(that.toSeq)

    /**
      * Copies values of this <code>NonEmptyVector</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyVector</code>. Copying
      * will stop once either the end of the current <code>NonEmptyVector</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      */
    def copyToArray[U >: T](arr: Array[U]): Unit = toVector.copyToArray(arr)

    /**
      * Copies values of this <code>NonEmptyVector</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyVector</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyVector</code> is reached, or the end of the array is reached.
      *
      * @param arr the array to fill
      * @param start the starting index
      */
    def copyToArray[U >: T](arr: Array[U], start: Int): Unit = toVector.copyToArray(arr, start)

    /**
      * Copies values of this <code>NonEmptyVector</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptyVector</code>, beginning at
      * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyVector</code> is reached, the end of the array is reached, or
      * <code>len</code> elements have been copied.
      *
      * @param arr the array to fill
      * @param start the starting index
      * @param len the maximum number of elements to copy
      */
    def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = toVector.copyToArray(arr, start, len)

    /**
      * Copies all elements of this <code>NonEmptyVector</code> to a buffer. 
      *
      * @param buf the buffer to which elements are copied
      */
    def copyToBuffer[U >: T](buf: Buffer[U]): Unit = toVector.copyToBuffer(buf)

    /**
      * Indicates whether every element of this <code>NonEmptyVector</code> relates to the corresponding element of a given <code>IterableOnce</code> by satisfying a given predicate. 
      *
      * @tparam B the type of the elements of <code>that</code>
      * @param that the <code>IterableOnce</code> to compare for correspondence
      * @param p the predicate, which relates elements from this <code>NonEmptyVector</code> and the passed <code>IterableOnce</code>
      * @return true if this <code>NonEmptyVector</code> and the passed <code>IterableOnce</code> have the same length and <code>p(x, y)</code> is <code>true</code>
      *     for all corresponding elements <code>x</code> of this <code>NonEmptyVector</code> and <code>y</code> of that, otherwise <code>false</code>.
      */
    def corresponds[B](that: IterableOnce[B])(p: (T, B) => Boolean): Boolean = toVector.corresponds(that)(p)

    /**
      * Counts the number of elements in this <code>NonEmptyVector</code> that satisfy a predicate. 
      *
      * @param p the predicate used to test elements.
      * @return the number of elements satisfying the predicate <code>p</code>. 
      */
    def count(p: T => Boolean): Int = toVector.count(p)

    /**
      * The length of this <code>NonEmptyVector</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyVector</code>. 
      */
    def length: Int = toVector.length

    /**
      * Converts this <code>NonEmptyVector</code> to a list.
      *
      * @return a list containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toVector: Vector[T] = nonEmptyVector

  }

}