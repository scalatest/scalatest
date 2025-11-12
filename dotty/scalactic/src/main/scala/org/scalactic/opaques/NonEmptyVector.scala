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
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.language.higherKinds
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.reflect.ClassTag
import scala.collection.generic.CanBuildFrom

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
      * Builds a new <code>NonEmptyVector</code> from this <code>NonEmptyVector</code> without any duplicate elements.
      *
      * @return A new <code>NonEmptyVector</code> that contains the first occurrence of every element of this <code>NonEmptyVector</code>. 
      */
    def distinct: NonEmptyVector[T] = toVector.distinct

    /**
      * Indicates whether this <code>NonEmptyVector</code> ends with the given <code>GenSeq</code>.
      *
      * @param that the sequence to test
      * @return <code>true</code> if this <code>NonEmptyVector</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
      */
    def endsWith[B](that: IterableOnce[B]): Boolean = toVector.toIndexedSeq.endsWith(that.toIterable)

    /**
      * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptyVector</code>.
      *
      * @param the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyVector</code>, otherwise <code>false</code>. 
      */
    def exists(p: T => Boolean): Boolean = toVector.exists(p)

    /**
      * Finds the first element of this <code>NonEmptyVector</code> that satisfies the given predicate, if any.
      *
      * @param p the predicate used to test elements
      * @return an <code>Some</code> containing the first element in this <code>NonEmptyVector</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
      */
    def find(p: T => Boolean): Option[T] = toVector.find(p)

    /**
      * Builds a new <code>NonEmptyVector</code> by applying a function to all elements of this <code>NonEmptyVector</code> and using the elements of the resulting <code>NonEmptyVector</code>s.
      *
      * @tparam U the element type of the returned <code>NonEmptyVector</code>
      * @param f the function to apply to each element.
      * @return a new <code>NonEmptyVector</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptyVector</code> and concatenating
      *    the elements of resulting <code>NonEmptyVector</code>s. 
      */
    def flatMap[U](f: T => NonEmptyVector[U]): NonEmptyVector[U] = toVector.flatMap(f)

    /**
      * Converts this <code>NonEmptyVector</code> of <code>NonEmptyVector</code>s into a <code>NonEmptyVector</code>
      * formed by the elements of the nested <code>NonEmptyVector</code>s.
      *
      * <p>
      * Note: You cannot use this <code>flatten</code> method on a <code>NonEmptyVector</code> that contains a <code>IterableOnce</code>s, because 
      * if all the nested <code>IterableOnce</code>s were empty, you'd end up with an empty <code>NonEmptyVector</code>.
      * </p>
      *
      * @tparm B the type of the elements of each nested <code>NonEmptyVector</code>
      * @return a new <code>NonEmptyVector</code> resulting from concatenating all nested <code>NonEmptyVector</code>s.
      */
    def flatten[B](using ev: T <:< NonEmptyVector[B]): NonEmptyVector[B] = flatMap(ev)

    /**
      * Folds the elements of this <code>NonEmptyVector</code> using the specified associative binary operator.
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
    def fold[U >: T](z: U)(op: (U, U) => U): U = toVector.fold(z)(op)

    /**
      * Applies a binary operator to a start value and all elements of this <code>NonEmptyVector</code>, going left to right.
      *
      * @tparam B the result type of the binary operator.
      * @param z the start value.
      * @param op the binary operator.
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>, going left to right, with the start value,
      *     <code>z</code>, on the left:
      *
      * <pre>
      * op(...op(op(z, x_1), x_2), ..., x_n)
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyVector</code>. 
      * </p>
      */
    def foldLeft[B](z: B)(op: (B, T) => B): B = toVector.foldLeft(z)(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyVector</code> and a start value, going right to left.
      *
      * @tparam B the result of the binary operator
      * @param z the start value
      * @param op the binary operator
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>, going right to left, with the start value,
      *     <code>z</code>, on the right:
      *
      * <pre>
      * op(x_1, op(x_2, ... op(x_n, z)...))
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyVector</code>. 
      * </p>
      */
    def foldRight[B](z: B)(op: (T, B) => B): B = toVector.foldRight(z)(op)

    /**
      * Indicates whether a predicate holds for all elements of this <code>NonEmptyVector</code>.
      *
      * @param p the predicate used to test elements.
      * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptyVector</code>, otherwise <code>false</code>. 
      */
    def forall(p: T => Boolean): Boolean = toVector.forall(p)

    /**
      * Applies a function <code>f</code> to all elements of this <code>NonEmptyVector</code>.
      *
      * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
      */
    def foreach(f: T => Unit): Unit = toVector.foreach(f)

    /**
      * Partitions this <code>NonEmptyVector</code> into a map of <code>NonEmptyVector</code>s according to some discriminator function.
      *
      * @tparam K the type of keys returned by the discriminator function.
      * @param f the discriminator function.
      * @return A map from keys to <code>NonEmptyVector</code>s such that the following invariant holds:
      *
      * <pre>
      * (nonEmptyVector.toVector partition f)(k) = xs filter (x =&gt; f(x) == k)
      * </pre>
      *
      * <p>
      * That is, every key <code>k</code> is bound to a <code>NonEmptyVector</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
      * </p>
      */
    def groupBy[K](f: T => K): Map[K, NonEmptyVector[T]] = toVector.groupBy(f)

    /**
      * Partitions elements into fixed size <code>NonEmptyVector</code>s.
      *
      * @param size the number of elements per group
      * @return An iterator producing <code>NonEmptyVector</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
      */
    def grouped(size: Int): Iterator[NonEmptyVector[T]] = toVector.grouped(size)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyVector</code> has a definite size, since all <code>NonEmptyVector</code>s are strict collections.
      */
    def hasDefiniteSize: Boolean = true

    // override def hashCode: Int = toVector.hashCode

    /**
      * Selects the first element of this <code>NonEmptyVector</code>. 
      *
      * @return the first element of this <code>NonEmptyVector</code>.
      */
    def head: T = toVector.head

    /**
      * Selects the first element of this <code>NonEmptyVector</code> and returns it wrapped in a <code>Some</code>. 
      *
      * @return the first element of this <code>NonEmptyVector</code>, wrapped in a <code>Some</code>.
      */
    def headOption: Option[T] = toVector.headOption

    /**
      * Finds index of first occurrence of some value in this <code>NonEmptyVector</code>.
      *
      * @param elem the element value to search for. 
      * @return the index of the first element of this <code>NonEmptyVector</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexOf[U >: T](elem: U): Int = toVector.indexOf(elem, 0)

    /**
      * Finds index of first occurrence of some value in this <code>NonEmptyVector</code> after or at some start index.
      *
      * @param elem the element value to search for. 
      * @param from the start index
      * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyVector</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexOf[U >: T](elem: U, from: Int): Int = toVector.indexOf(elem, from)


    /**
      * Finds first index where this <code>NonEmptyVector</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @return the first index at which the elements of this <code>NonEmptyVector</code> starting at that index match the elements of
      *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def indexOfSlice[U >: T](that: IterableOnce[U]): Int = toVector.toIndexedSeq.indexOfSlice(that.toSeq)

    /**
      * Finds first index after or at a start index where this <code>NonEmptyVector</code> contains a given <code>IterableOnce</code> as a slice.
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @param from the start index
      * @return the first index <code>&gt;=</code> <code>from</code> at which the elements of this <code>NonEmptyVector</code> starting at that index match the elements of
      *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def indexOfSlice[U >: T](that: IterableOnce[U], from: Int): Int = toVector.toIndexedSeq.indexOfSlice(that.toSeq, from)

    /**
      * Finds index of the first element satisfying some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the index of the first element of this <code>NonEmptyVector</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexWhere(p: T => Boolean): Int = toVector.indexWhere(p)

    /**
      * Produces the range of all indices of this <code>NonEmptyVector</code>. 
      *
      * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyVector</code>. 
      */
    def indices: Range = toVector.indices

    /**
      * Returns <code>false</code> to indicate this <code>NonEmptyVector</code>, like all <code>NonEmptyVector</code>s, is non-empty.
      *
      * @return false
      */
    def isEmpty: Boolean = false

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyVector</code>, like all <code>NonEmptyVector</code>s, can be traversed repeatedly.
      *
      * @return true
      */
    def isTraversableAgain: Boolean = true

    /**
      * Finds index of the first element satisfying some predicate after or at some start index.
      *
      * @param p the predicate used to test elements.
      * @param from the start index
      * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyVector</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists.
      */
    def indexWhere(p: T => Boolean, from: Int): Int = toVector.indexWhere(p, from)

    /**
      * Selects the last element of this <code>NonEmptyVector</code>. 
      *
      * @return the last element of this <code>NonEmptyVector</code>.
      */
    def last: T = toVector.last

    /**
      * Finds the index of the last occurrence of some value in this <code>NonEmptyVector</code>.
      *
      * @param elem the element value to search for.
      * @return the index of the last element of this <code>NonEmptyVector</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
      *     or <code>-1</code>, if none exists.
      */
    def lastIndexOf[U >: T](elem: U): Int = toVector.lastIndexOf(elem)

    /**
      * Finds the index of the last occurrence of some value in this <code>NonEmptyVector</code> before or at a given <code>end</code> index.
      *
      * @param elem the element value to search for.
      * @param end the end index. 
      * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyVector</code> that is equal (as determined by <code>==</code>)
      *     to <code>elem</code>, or <code>-1</code>, if none exists.
      */
    def lastIndexOf[U >: T](elem: U, end: Int): Int = toVector.lastIndexOf(elem, end)

    /**
      * Finds the last index where this <code>NonEmptyVector</code> contains a given <code>IterableOnce</code> as a slice. 
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @return the last index at which the elements of this <code>NonEmptyVector</code> starting at that index match the elements of
      *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def lastIndexOfSlice[U >: T](that: IterableOnce[U]): Int = toVector.toIndexedSeq.lastIndexOfSlice(that.toSeq)

    /**
      * Finds the last index before or at a given end index where this <code>NonEmptyVector</code> contains a given <code>IterableOnce</code> as a slice. 
      *
      * @param that the <code>IterableOnce</code> defining the slice to look for
      * @param end the end index
      * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyVector</code> starting at that index match the elements of
      *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
      */
    def lastIndexOfSlice[U >: T](that: IterableOnce[U], end: Int): Int = toVector.toIndexedSeq.lastIndexOfSlice(that.toSeq, end)

    /**
      * Finds index of last element satisfying some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the index of the last element of this <code>NonEmptyVector</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists. 
      */
    def lastIndexWhere(p: T => Boolean): Int = toVector.lastIndexWhere(p)

    /**
      * Finds index of last element satisfying some predicate before or at given end index.
      *
      * @param p the predicate used to test elements.
      * @param end the end index
      * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyVector</code> that satisfies the predicate <code>p</code>,
      *     or <code>-1</code>, if none exists. 
      */
    def lastIndexWhere(p: T => Boolean, end: Int): Int = toVector.lastIndexWhere(p, end)

    /**
      * Returns the last element of this <code>NonEmptyVector</code>, wrapped in a <code>Some</code>. 
      *
      * @return the last element, wrapped in a <code>Some</code>. 
      */
    def lastOption: Option[T] = toVector.lastOption // Will always return a Some

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
      * Compares the length of this <code>NonEmptyVector</code> to a test value. 
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
    def lengthCompare(len: Int): Int = toVector.lengthCompare(len)

    /**
      * Builds a new <code>NonEmptyVector</code> by applying a function to all elements of this <code>NonEmptyVector</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyVector</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyVector</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyVector</code> and collecting the results. 
      */
    def map[U](f: T => U): NonEmptyVector[U] = toVector.map(f)

    /**
      * Finds the largest element.
      *
      * @return the largest element of this <code>NonEmptyVector</code>. 
      */
    def max[U >: T](using cmp: Ordering[U]): T = toVector.max(cmp)

    /**
      * Finds the largest result after applying the given function to every element.
      *
      * @return the largest result of applying the given function to every element of this <code>NonEmptyVector</code>. 
      */
    def maxBy[U](f: T => U)(using cmp: Ordering[U]): T = toVector.maxBy(f)(cmp)

    /**
      * Finds the smallest element.
      *
      * @return the smallest element of this <code>NonEmptyVector</code>. 
      */
    def min[U >: T](using cmp: Ordering[U]): T = toVector.min(cmp)

    /**
      * Finds the smallest result after applying the given function to every element.
      *
      * @return the smallest result of applying the given function to every element of this <code>NonEmptyVector</code>. 
      */
    def minBy[U](f: T => U)(using cmp: Ordering[U]): T = toVector.minBy(f)(cmp)

    /**
      * Displays all elements of this <code>NonEmptyVector</code> in a string. 
      *
      * @return a string representation of this <code>NonEmptyVector</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptyVector</code> follow each other without any separator string. 
      */
    def mkString: String = toVector.mkString

    /**
      * Displays all elements of this <code>NonEmptyVector</code> in a string using a separator string. 
      *
      * @param sep the separator string
      * @return a string representation of this <code>NonEmptyVector</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptyVector</code> are separated by the string <code>sep</code>. 
      */
    def mkString(sep: String): String = toVector.mkString(sep)

    /**
      * Displays all elements of this <code>NonEmptyVector</code> in a string using start, end, and separator strings. 
      *
      * @param start the starting string.
      * @param sep the separator string.
      * @param end the ending string.
      * @return a string representation of this <code>NonEmptyVector</code>. The resulting string begins with the string <code>start</code> and ends with the string
      *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>NonEmptyVector</code> are
      *     separated by the string <code>sep</code>. 
      */
    def mkString(start: String, sep: String, end: String): String = toVector.mkString(start, sep, end)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptyVector</code>, like all <code>NonEmptyVector</code>s, is non-empty.
      *
      * @return true
      */
    def nonEmpty: Boolean = true

    /**
      * A copy of this <code>NonEmptyVector</code> with an element value appended until a given target length is reached.
      *
      * @param len the target length 
      * @param elem he padding value
      * @return a new <code>NonEmptyVector</code> consisting of all elements of this <code>NonEmptyVector</code> followed by the minimal number of occurrences
      *     of <code>elem</code> so that the resulting <code>NonEmptyVector</code> has a length of at least <code>len</code>. 
      */
    def padTo[U >: T](len: Int, elem: U): NonEmptyVector[U] = toVector.padTo(len, elem)

    /**
      * Produces a new <code>NonEmptyVector</code> where a slice of elements in this <code>NonEmptyVector</code> is replaced by another <code>NonEmptyVector</code>
      *
      * @param from the index of the first replaced element 
      * @param that the <code>NonEmptyVector</code> whose elements should replace a slice in this <code>NonEmptyVector</code>
      * @param replaced the number of elements to drop in the original <code>NonEmptyVector</code>
      */
    def patch[U >: T](from: Int, that: NonEmptyVector[U], replaced: Int): NonEmptyVector[U] = toVector.patch(from, that.toVector, replaced)

    /**
      * Iterates over distinct permutations. 
      *
      * <p>
      * Here's an example:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyVector('a', 'b', 'b').permutations.toVector = Vector(NonEmptyVector(a, b, b), NonEmptyVector(b, a, b), NonEmptyVector(b, b, a))
      * </pre>
      *
      * @return an iterator that traverses the distinct permutations of this <code>NonEmptyVector</code>.
      */
    def permutations: Iterator[NonEmptyVector[T]] = toVector.permutations

    /**
      * Returns the length of the longest prefix whose elements all satisfy some predicate.
      *
      * @param p the predicate used to test elements.
      * @return the length of the longest prefix of this <code>NonEmptyVector</code> such that every element
      *     of the segment satisfies the predicate <code>p</code>. 
      */
    def prefixLength(p: T => Boolean): Int = toVector.prefixLength(p)

    /**
      * The result of multiplying all the elements of this <code>NonEmptyVector</code>.
      *
      * <p>
      * This method can be invoked for any <code>NonEmptyVector[T]</code> for which a given <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the product of all elements
      */
    def product[U >: T](using num: Numeric[U]): U = toVector.product(num)

    /**
      * Reduces the elements of this <code>NonEmptyVector</code> using the specified associative binary operator.
      *
      * <p>
      * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
      * </p>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T.
      * @param op a binary operator that must be associative.
      * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptyVector</code>.
      */
    def reduce[U >: T](op: (U, U) => U): U = toVector.reduce(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyVector</code>, going left to right.
      *
      * @tparam U the result type of the binary operator.
      * @param op the binary operator.
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>, going left to right:
      *
      * <pre>
      * op(...op(op(x_1, x_2), x_3), ..., x_n)
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyVector</code>. 
      * </p>
      */
    def reduceLeft[U >: T](op: (U, T) => U): U = toVector.reduceLeft(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyVector</code>, going left to right, returning the result in a <code>Some</code>.
      *
      * @tparam U the result type of the binary operator.
      * @param op the binary operator.
      * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
      * </p>
      */
    def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = toVector.reduceLeftOption(op)

    def reduceOption[U >: T](op: (U, U) => U): Option[U] = toVector.reduceOption(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyVector</code>, going right to left.
      *
      * @tparam U the result of the binary operator
      * @param op the binary operator
      * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>, going right to left:
      *
      * <pre>
      * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
      * </pre>
      *
      * <p>
      * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyVector</code>. 
      * </p>
      */
    def reduceRight[U >: T](op: (T, U) => U): U = toVector.reduceRight(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptyVector</code>, going right to left, returning the result in a <code>Some</code>.
      *
      * @tparam U the result of the binary operator
      * @param op the binary operator
      * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
      */
    def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = toVector.reduceRightOption(op)

    /**
      * Returns new <code>NonEmptyVector</code> with elements in reverse order.
      *
      * @return a new <code>NonEmptyVector</code> with all elements of this <code>NonEmptyVector</code> in reversed order. 
      */
    def reverse: NonEmptyVector[T] = toVector.reverse

    /**
      * An iterator yielding elements in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyVector.reverseIterator</code> is the same as <code>nonEmptyVector.reverse.iterator</code>, but might be more efficient. 
      * </p>
      *
      * @return an iterator yielding the elements of this <code>NonEmptyVector</code> in reversed order 
      */
    def reverseIterator: Iterator[T] = toVector.reverseIterator

    /**
      * Builds a new <code>NonEmptyVector</code> by applying a function to all elements of this <code>NonEmptyVector</code> and collecting the results in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyVector.reverseMap(f)</code> is the same as <code>nonEmptyVector.reverse.map(f)</code>, but might be more efficient. 
      * </p>
      *
      * @tparam U the element type of the returned <code>NonEmptyVector</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyVector</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyVector</code>
      *     and collecting the results in reverse order. 
      */
    def reverseMap[U](f: T => U): NonEmptyVector[U] = toVector.reverseMap(f)

    /**
      * Checks if the given <code>IterableOnce</code> contains the same elements in the same order as this <code>NonEmptyVector</code>.
      *
      * @param that the <code>IterableOnce</code> with which to compare
      * @return <code>true</code>, if both this <code>NonEmptyVector</code> and the given <code>IterableOnce</code> contain the same elements
      *     in the same order, <code>false</code> otherwise. 
      */
    def sameElements[U >: T](that: IterableOnce[U]): Boolean = toVector.sameElements(that)

    /**
      * Computes a prefix scan of the elements of this <code>NonEmptyVector</code>.
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
      * NonEmptyVector(1, 2, 3).scan(0)(_ + _) == NonEmptyVector(0, 1, 3, 6)
      * NonEmptyVector(1, 2, 3).scan("z")(_ + _.toString) == NonEmptyVector("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>NonEmptyVector</code>.
      * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return a new <code>NonEmptyVector</code> containing the prefix scan of the elements in this <code>NonEmptyVector</code> 
      */
    def scan[U >: T](z: U)(op: (U, U) => U): NonEmptyVector[U] = toVector.scan(z)(op)

    /**
      * Produces a <code>NonEmptyVector</code> containing cumulative results of applying the operator going left to right.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyVector(1, 2, 3).scanLeft(0)(_ + _) == NonEmptyVector(0, 1, 3, 6)
      * NonEmptyVector(1, 2, 3).scanLeft("z")(_ + _) == NonEmptyVector("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyVector</code>
      * @param z the start value.
      * @param op the binary operator.
      * @return a new <code>NonEmptyVector</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>,
      *     going left to right, with the start value, <code>z</code>, on the left.
      */
    def scanLeft[B](z: B)(op: (B, T) => B): NonEmptyVector[B] = toVector.scanLeft(z)(op)

    /**
      * Produces a <code>NonEmptyVector</code> containing cumulative results of applying the operator going right to left.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyVector(1, 2, 3).scanRight(0)(_ + _) == NonEmptyVector(6, 5, 3, 0)
      * NonEmptyVector(1, 2, 3).scanRight("z")(_ + _) == NonEmptyVector("123z", "23z", "3z", "z")
      * </pre>
      *
      * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyVector</code>
      * @param z the start value
      * @param op the binary operator
      * @return a new <code>NonEmptyVector</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyVector</code>,
      *     going right to left, with the start value, <code>z</code>, on the right.
      */
    def scanRight[B](z: B)(op: (T, B) => B): NonEmptyVector[B] = toVector.scanRight(z)(op)

    /**
      * Computes length of longest segment whose elements all satisfy some predicate.
      *
      * @param p the predicate used to test elements.
      * @param from the index where the search starts.
      * @param the length of the longest segment of this <code>NonEmptyVector</code> starting from index <code>from</code> such that every element of the
      *     segment satisfies the predicate <code>p</code>. 
      */
    def segmentLength(p: T => Boolean, from: Int): Int = toVector.segmentLength(p, from)

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
      *
      * @param size the number of elements per group
      * @return an iterator producing <code>NonEmptyVector</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int): Iterator[NonEmptyVector[T]] = toVector.sliding(size)

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
      * moving the sliding window by a given <code>step</code> each time.
      *
      * @param size the number of elements per group
      * @param step the distance between the first elements of successive groups
      * @return an iterator producing <code>NonEmptyVector</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int, step: Int): Iterator[NonEmptyVector[T]] = toVector.sliding(size, step)

    /**
      * The size of this <code>NonEmptyVector</code>.
      *
      * <p>
      * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
      * </p>
      *
      * @return the number of elements in this <code>NonEmptyVector</code>. 
      */
    def size: Int = toVector.size

    /**
      * Sorts this <code>NonEmptyVector</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
      *
      * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
      * @param f the transformation function mapping elements to some other domain <code>U</code>.
      * @param ord the ordering assumed on domain <code>U</code>.
      * @return a <code>NonEmptyVector</code> consisting of the elements of this <code>NonEmptyVector</code> sorted according to the <code>Ordering</code> where
      *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
      */
    def sortBy[U](f: T => U)(using ord: Ordering[U]): NonEmptyVector[T] = toVector.sortBy(f)

    /**
      * Sorts this <code>NonEmptyVector</code> according to a comparison function.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyVector</code> as in the original. 
      * </p>
      *
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyVector</code> consisting of the elements of this <code>NonEmptyVector</code> sorted according to the comparison function <code>lt</code>.
      */
    def sortWith(lt: (T, T) => Boolean): NonEmptyVector[T] = toVector.sortWith(lt)

    /**
      * Sorts this <code>NonEmptyVector</code> according to an <code>Ordering</code>.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyVector</code> as in the original. 
      * </p>
      *
      * @param ord the <code>Ordering</code> to be used to compare elements.
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyVector</code> consisting of the elements of this <code>NonEmptyVector</code> sorted according to the comparison function <code>lt</code>.
      */
    def sorted[U >: T](using ord: Ordering[U]): NonEmptyVector[U] = toVector.sorted(ord)

    /**
      * Indicates whether this <code>NonEmptyVector</code> starts with the given <code>IterableOnce</code>. 
      *
      * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyVector</code>
      * @return <code>true</code> if this <code>NonEmptyVector</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
      */
    def startsWith[B](that: IterableOnce[B]): Boolean = toVector.startsWith(that)

    /**
      * Indicates whether this <code>NonEmptyVector</code> starts with the given <code>IterableOnce</code> at the given index. 
      *
      * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyVector</code>
      * @param offset the index at which this <code>NonEmptyVector</code> is searched.
      * @return <code>true</code> if this <code>NonEmptyVector</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
      */
    def startsWith[B](that: IterableOnce[B], offset: Int): Boolean = toVector.startsWith(that, offset)

    /**
      * Returns <code>"NonEmptyVector"</code>, the prefix of this object's <code>toString</code> representation.
      *
      * @return the string <code>"NonEmptyVector"</code>
      */
    def stringPrefix: String = "NonEmptyVector"

    /**
      * The result of summing all the elements of this <code>NonEmptyVector</code>.
      *
      * <p>
      * This method can be invoked for any <code>NonEmptyVector[T]</code> for which a given <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the sum of all elements
      */
    def sum[U >: T](using num: Numeric[U]): U = toVector.sum(num)

    /**
      * Converts this <code>NonEmptyVector</code> into a collection of type <code>Col</code> by copying all elements.
      *
      * @tparam Col the collection type to build.
      * @return a new collection containing all elements of this <code>NonEmptyVector</code>. 
      */
    def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] =
      toVector.to(factory)

    /**
      * Converts this <code>NonEmptyVector</code> to an array.
      *
      * @return an array containing all elements of this <code>NonEmptyVector</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyVector</code>. 
      */
    def toArray[U >: T](using classTag: ClassTag[U]): Array[U] = toVector.toArray

    /**
      * Converts this <code>NonEmptyVector</code> to a <code>Vector</code>.
      *
      * @return a <code>Vector</code> containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toList: List[T] = toVector.toList

    /**
      * Converts this <code>NonEmptyVector</code> to a mutable buffer.
      *
      * @return a buffer containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toBuffer[U >: T]: Buffer[U] = toVector.toBuffer

    /**
      * Converts this <code>NonEmptyVector</code> to a list.
      *
      * @return a list containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toVector: Vector[T] = nonEmptyVector

    /**
      * Converts this <code>NonEmptyVector</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toIndexedSeq: collection.immutable.IndexedSeq[T] = toVector.toVector

    /**
      * Converts this <code>NonEmptyVector</code> to an iterable collection.
      *
      * @return an <code>Iterable</code> containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toIterable: scala.collection.Iterable[T] = toVector.toIterable

    /**
      * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyVector</code>.
      *
      * @return an <code>Iterator</code> containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toIterator: Iterator[T] = toVector.toIterator

    /**
      * Converts this <code>NonEmptyVector</code> to a map.
      *
      * <p>
      * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
      * in the map. Duplicate keys will be overwritten by later keys.
      * </p>
      *
      * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>NonEmptyVector</code>. 
      */
    def toMap[K, V](using ev: T <:< (K, V)): Map[K, V] = toVector.toMap

    /**
      * Converts this <code>NonEmptyVector</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyVector</code>.
      */
    def toSeq: collection.immutable.Seq[T] = toVector

    /**
      * Converts this <code>NonEmptyVector</code> to a set.
      *
      * @return a set containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toSet[U >: T]: Set[U] = toVector.toSet

    /**
      * Converts this <code>NonEmptyVector</code> to a stream.
      *
      * @return a stream containing all elements of this <code>NonEmptyVector</code>. 
      */
    def toStream: Stream[T] = toVector.toStream

    def transpose[U](using ev: T <:< NonEmptyVector[U]): NonEmptyVector[NonEmptyVector[U]] = toVector.transpose(ev)

    /**
      * Produces a new <code>NonEmptyVector</code> that contains all elements of this <code>NonEmptyVector</code> and also all elements of a given <code>IterableOnce</code>.
      *
      * <p>
      * <code>nonEmptyVectorX</code> <code>union</code> <code>ys</code> is equivalent to <code>nonEmptyVectorX</code> <code>++</code> <code>ys</code>.
      * </p>
      *
      * <p>
      * Another way to express this is that <code>nonEmptyVectorX</code> <code>union</code> <code>ys</code> computes the order-presevring multi-set union
      * of <code>nonEmptyVectorX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
      * also work on multi-sets.
      * </p>
      *
      * @param that the <code>IterableOnce</code> to add.
      * @return a new <code>NonEmptyVector</code> that contains all elements of this <code>NonEmptyVector</code> followed by all elements of <code>that</code> <code>IterableOnce</code>.
      */
    def union[U >: T](that: IterableOnce[U])(using cbf: CanBuildFrom[Vector[T], U, Vector[U]]): NonEmptyVector[U] = toVector.toIndexedSeq.union(that.toSeq).toVector

    /**
      * Converts this <code>NonEmptyVector</code> of pairs into two <code>NonEmptyVector</code>s of the first and second half of each pair. 
      *
      * @tparam L the type of the first half of the element pairs
      * @tparam R the type of the second half of the element pairs
      * @param asPair a given conversion that asserts that the element type of this <code>NonEmptyVector</code> is a pair.
      * @return a pair of <code>NonEmptyVector</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyVector</code>. 
      */
    def unzip[L, R](using asPair: T => (L, R)): (NonEmptyVector[L], NonEmptyVector[R]) = toVector.unzip(asPair)

    /**
      * Converts this <code>NonEmptyVector</code> of triples into three <code>NonEmptyVector</code>s of the first, second, and and third element of each triple. 
      *
      * @tparam L the type of the first member of the element triples
      * @tparam R the type of the second member of the element triples
      * @tparam R the type of the third member of the element triples
      * @param asTriple a given conversion that asserts that the element type of this <code>NonEmptyVector</code> is a triple.
      * @return a triple of <code>NonEmptyVector</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptyVector</code>. 
      */
    def unzip3[L, M, R](using asTriple: T => (L, M, R)): (NonEmptyVector[L], NonEmptyVector[M], NonEmptyVector[R]) = toVector.unzip3(asTriple)

    /**
      * A copy of this <code>NonEmptyVector</code> with one single replaced element.
      *
      * @param idx the position of the replacement
      * @param elem the replacing element
      * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyVector</code>
      * @return a copy of this <code>NonEmptyVector</code> with the element at position <code>idx</code> replaced by <code>elem</code>. 
      */
    def updated[U >: T](idx: Int, elem: U): NonEmptyVector[U] = toVector.updated(idx, elem)

    /**
      * Returns a <code>NonEmptyVector</code> formed from this <code>NonEmptyVector</code> and an iterable collection by combining corresponding
      * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
      * shorter collection to the length of the longer.
      *
      * @tparm O the type of the second half of the returned pairs
      * @tparm U the type of the first half of the returned pairs
      * @param other the <code>Iterable</code> providing the second half of each result pair
      * @param thisElem the element to be used to fill up the result if this <code>NonEmptyVector</code> is shorter than <code>that</code> <code>Iterable</code>.
      * @param thatElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptyVector</code>.
      * @return a new <code>NonEmptyVector</code> containing pairs consisting of corresponding elements of this <code>NonEmptyVector</code> and <code>that</code>. The
      *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyVector</code> and <code>that</code>. If this <code>NonEmptyVector</code>
      *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
      *     <code>NonEmptyVector</code>, <code>thatElem</code> values are used to pad the result. 
      */
    def zipAll[O, U >: T](other: IterableOnce[O], thisElem: U, otherElem: O): NonEmptyVector[(U, O)] = toVector.toIndexedSeq.zipAll(other.toIterable, thisElem, otherElem).toVector

    /**
      * Zips this <code>NonEmptyVector</code>  with its indices.
      *
      * @return A new <code>NonEmptyVector</code> containing pairs consisting of all elements of this <code>NonEmptyVector</code> paired with their index. Indices start at 0.
      */
    def zipWithIndex: NonEmptyVector[(T, Int)] = toVector.zipWithIndex
  }

}