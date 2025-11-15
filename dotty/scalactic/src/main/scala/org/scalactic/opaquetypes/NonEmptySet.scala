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
package org.scalactic.opaquetypes

import scala.collection.GenSet
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.language.higherKinds
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.reflect.ClassTag
import org.scalactic.Resources

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
   *
   * A factory/assertion method that produces a <code>NonEmptySet</code>
   * given a valid <code>Set</code> value, or throws
   * <code>AssertionError</code>, if given an invalid <code>Set</code> value.
   *
   * Note: you should use this method only when you are convinced that it will
   * always succeed, i.e., never throw an exception. It is good practice to
   * add a comment near the invocation of this method indicating ''why'' you
   * think it will always succeed to document your reasoning. If you are not
   * sure an `ensuringValid` call will always succeed, you should use one of
   * the other factory or validation methods provided on this object instead:
   * `from'.
   *
   * @param set the <code>Set</code> to check to see if it is a valid.
   * @return the <code>NonEmptySet</code> if the passed set is valid..
   * @throws AssertionError if the passed array is not valid.
   */
  def ensuringValid[T](set: Set[T]): NonEmptySet[T] =
    if (set.size == 0)
      throw new AssertionError(Resources.nonEmptySetEmpty)
    else
      set

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
    def groupBy[K](f: T => K): Map[K, NonEmptySet[T]] = toSet.groupBy(f)

    /**
      * Partitions elements into fixed size <code>NonEmptySet</code>s.
      *
      * @param size the number of elements per group
      * @return An iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
      */
    def grouped(size: Int): Iterator[NonEmptySet[T]] = toSet.grouped(size)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptySet</code> has a definite size, since all <code>NonEmptySet</code>s are strict collections.
      */
    def hasDefiniteSize: Boolean = true

    // override def hashCode: Int = toSet.hashCode

    /**
      * Selects the first element of this <code>NonEmptySet</code>. 
      *
      * @return the first element of this <code>NonEmptySet</code>.
      */
    def head: T = toSet.head

    /**
      * Selects the first element of this <code>NonEmptySet</code> and returns it wrapped in a <code>Some</code>. 
      *
      * @return the first element of this <code>NonEmptySet</code>, wrapped in a <code>Some</code>.
      */
    def headOption: Option[T] = toSet.headOption

    /**
      * Returns <code>false</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, is non-empty.
      *
      * @return false
      */
    def isEmpty: Boolean = false

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, can be traversed repeatedly.
      *
      * @return true
      */
    def isTraversableAgain: Boolean = true

    /**
      * Selects the last element of this <code>NonEmptySet</code>. 
      *
      * @return the last element of this <code>NonEmptySet</code>.
      */
    def last: T = toSet.last

    /**
      * Returns the last element of this <code>NonEmptySet</code>, wrapped in a <code>Some</code>. 
      *
      * @return the last element, wrapped in a <code>Some</code>. 
      */
    def lastOption: Option[T] = toSet.lastOption // Will always return a Some

    /**
      * Finds the largest element.
      *
      * @return the largest element of this <code>NonEmptySet</code>. 
      */
    def max[U >: T](using cmp: Ordering[U]): T = toSet.max(cmp)

    /**
      * Finds the largest result after applying the given function to every element.
      *
      * @return the largest result of applying the given function to every element of this <code>NonEmptySet</code>. 
      */
    def maxBy[U](f: T => U)(using cmp: Ordering[U]): T = toSet.maxBy(f)(cmp)

    /**
      * Finds the smallest element.
      *
      * @return the smallest element of this <code>NonEmptySet</code>. 
      */
    def min[U >: T](using cmp: Ordering[U]): T = toSet.min(cmp)

    /**
      * Finds the smallest result after applying the given function to every element.
      *
      * @return the smallest result of applying the given function to every element of this <code>NonEmptySet</code>. 
      */
    def minBy[U](f: T => U)(using cmp: Ordering[U]): T = toSet.minBy(f)(cmp)

    /**
      * Displays all elements of this <code>NonEmptySet</code> in a string. 
      *
      * @return a string representation of this <code>NonEmptySet</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptySet</code> follow each other without any separator string. 
      */
    def mkString: String = toSet.mkString

    /**
      * Displays all elements of this <code>NonEmptySet</code> in a string using a separator string. 
      *
      * @param sep the separator string
      * @return a string representation of this <code>NonEmptySet</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
      *     <code>NonEmptySet</code> are separated by the string <code>sep</code>. 
      */
    def mkString(sep: String): String = toSet.mkString(sep)

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
    def mkString(start: String, sep: String, end: String): String = toSet.mkString(start, sep, end)

    /**
      * Builds a new <code>NonEmptySet</code> by applying a function to all elements of this <code>NonEmptySet</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptySet</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptySet</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptySet</code> and collecting the results. 
      */
    def map[U](f: T => U): NonEmptySet[U] = toSet.map(f)

    /**
      * Returns <code>true</code> to indicate this <code>NonEmptySet</code>, like all <code>NonEmptySet</code>s, is non-empty.
      *
      * @return true
      */
    def nonEmpty: Boolean = true

    /**
      * The result of multiplying all the elements of this <code>NonEmptySet</code>.
      *
      * <p>
      * This method can be invoked for any <code>NonEmptySet[T]</code> for which an using <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the product of all elements
      */
    def product[U >: T](using num: Numeric[U]): U = toSet.product(num)

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
    def reduce[U >: T](op: (U, U) => U): U = toSet.reduce(op)

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
    def reduceLeft[U >: T](op: (U, T) => U): U = toSet.reduceLeft(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going left to right, returning the result in a <code>Some</code>.
      *
      * @tparam U the result type of the binary operator.
      * @param op the binary operator.
      * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
      * </p>
      */
    def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = toSet.reduceLeftOption(op)

    /**
      * Reduces the elements of this `NonEmptySet` using the specified binary operator, 
      * returning an `Option` containing the result.
      *
      * This method applies the binary operator `op` to combine all elements of the set
      * into a single value. Since a `NonEmptySet` is guaranteed to have at least one element,
      * the result will always be `Some(result)` if the underlying set is non-empty.
      * However, because it delegates to `Set#reduceOption`, the return type is `Option[U]`
      * for consistency with the standard library.
      *
      * @param op the associative binary operator used to reduce the elements.
      * @tparam U the result type of the reduction, which must be a supertype of `T`.
      * @return an `Option` containing the result of reducing this `NonEmptySet` with `op`.
      *         It will never be `None` since this set cannot be empty.
      *
      * @example
      * {{{
      * val s = NonEmptySet(1, 2, 3)
      * val sum = s.reduceOption(_ + _)   // Some(6)
      * val max = s.reduceOption(_ max _) // Some(3)
      * }}}
      *
      * @see [[scala.collection.SetOps.reduceOption]] for the underlying operation.
      */
    def reduceOption[U >: T](op: (U, U) => U): Option[U] = toSet.reduceOption(op)

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
    def reduceRight[U >: T](op: (T, U) => U): U = toSet.reduceRight(op)

    /**
      * Applies a binary operator to all elements of this <code>NonEmptySet</code>, going right to left, returning the result in a <code>Some</code>.
      *
      * @tparam U the result of the binary operator
      * @param op the binary operator
      * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
      */
    def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = toSet.reduceRightOption(op)

    /**
      * Checks if the given <code>IterableOnce</code> contains the same elements in the same order as this <code>NonEmptySet</code>.
      *
      * @param that the <code>IterableOnce</code> with which to compare
      * @return <code>true</code>, if both this <code>NonEmptySet</code> and the given <code>IterableOnce</code> contain the same elements
      *     in the same order, <code>false</code> otherwise. 
      */
    def sameElements[U >: T](that: IterableOnce[U]): Boolean = toSet.toIndexedSeq.sameElements(that)

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
    def scan[U >: T](z: U)(op: (U, U) => U): NonEmptySet[U] = toSet.scan(z)(op)

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
    def scanLeft[B](z: B)(op: (B, T) => B): NonEmptySet[B] = toSet.scanLeft(z)(op)

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
    def scanRight[B](z: B)(op: (T, B) => B): NonEmptySet[B] = toSet.scanRight(z)(op)

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
      *
      * @param size the number of elements per group
      * @return an iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int): Iterator[NonEmptySet[T]] = toSet.sliding(size)

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
      * moving the sliding window by a given <code>step</code> each time.
      *
      * @param size the number of elements per group
      * @param step the distance between the first elements of successive groups
      * @return an iterator producing <code>NonEmptySet</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    def sliding(size: Int, step: Int): Iterator[NonEmptySet[T]] = toSet.sliding(size, step)

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
      * This method can be invoked for any <code>NonEmptySet[T]</code> for which an using <code>Numeric[T]</code> exists.
      * </p>
      *
      * @return the sum of all elements
      */
    def sum[U >: T](using num: Numeric[U]): U = toSet.sum(num)

    /**
      * Converts this <code>NonEmptySet</code> into a collection of type <code>Col</code> by copying all elements.
      *
      * @tparam Col the collection type to build.
      * @return a new collection containing all elements of this <code>NonEmptySet</code>. 
      */
    def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] =
      toSet.to(factory)

    /**
      * Converts this <code>NonEmptySet</code> to an array.
      *
      * @return an array containing all elements of this <code>NonEmptySet</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptySet</code>. 
      */
    def toArray[U >: T](using classTag: ClassTag[U]): Array[U] = toSet.toArray

    /**
      * Converts this <code>NonEmptySet</code> to a mutable buffer.
      *
      * @return a buffer containing all elements of this <code>NonEmptySet</code>. 
      */
    def toBuffer[U >: T]: Buffer[U] = toSet.toBuffer

    /**
      * Converts this <code>NonEmptySet</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptySet</code>. 
      */
    def toIndexedSeq: collection.immutable.IndexedSeq[T] = toSet.toVector

    /**
      * Converts this <code>NonEmptySet</code> to an iterable collection.
      *
      * @return an <code>Iterable</code> containing all elements of this <code>NonEmptySet</code>. 
      */
    def toIterable: scala.collection.Iterable[T] = toSet.toIterable

    /**
      * Returns an <code>Iterator</code> over the elements in this <code>NonEmptySet</code>.
      *
      * @return an <code>Iterator</code> containing all elements of this <code>NonEmptySet</code>. 
      */
    def toIterator: Iterator[T] = toSet.toIterator

    /**
      * Converts this <code>NonEmptySet</code> to a standard Scala <code>Set</code>.
      *
      * @return a <code>Set</code> containing all elements of this <code>NonEmptySet</code>
      */
    def toSet: Set[T] = nonEmptySet

    /**
      * Converts this <code>NonEmptySet</code> to a <code>Vector</code>.
      *
      * @return a <code>Vector</code> containing all elements of this <code>NonEmptySet</code>. 
      */
    def toVector: Vector[T] = toSet.toVector

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
    def toMap[K, V](using ev: T <:< (K, V)): Map[K, V] = toSet.toMap

    /**
      * Converts this <code>NonEmptySet</code> to an immutable <code>IndexedSeq</code>.
      *
      * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptySet</code>.
      */
    def toSeq: Seq[T] = toSet.toSeq

    /**
      * Converts this <code>NonEmptySet</code> to a set.
      *
      * @return a set containing all elements of this <code>NonEmptySet</code>.
      */
    def toList: collection.immutable.List[T] = toSet.toList

    /**
      * Converts this <code>NonEmptySet</code> to a stream.
      *
      * @return a stream containing all elements of this <code>NonEmptySet</code>. 
      */
    def toStream: Stream[T] = toSet.toStream

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

    def transpose[U](using ev: T <:< NonEmptySet[U]): NonEmptySet[NonEmptySet[U]] = toSet.transpose(ev)

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
    def union(that: IterableOnce[T]): NonEmptySet[T] = toSet.union(that.toSet)

    /**
      * Converts this <code>NonEmptySet</code> of pairs into two <code>NonEmptySet</code>s of the first and second half of each pair. 
      *
      * @tparam L the type of the first half of the element pairs
      * @tparam R the type of the second half of the element pairs
      * @param asPair a given conversion that asserts that the element type of this <code>NonEmptySet</code> is a pair.
      * @return a pair of <code>NonEmptySet</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptySet</code>. 
      */
    def unzip[L, R](using asPair: T => (L, R)): (NonEmptySet[L], NonEmptySet[R]) = toSet.unzip(asPair)

    /**
      * Converts this <code>NonEmptySet</code> of triples into three <code>NonEmptySet</code>s of the first, second, and and third element of each triple. 
      *
      * @tparam L the type of the first member of the element triples
      * @tparam M the type of the second member of the element triples
      * @tparam R the type of the third member of the element triples
      * @param asTriple a given conversion that asserts that the element type of this <code>NonEmptySet</code> is a triple.
      * @return a triple of <code>NonEmptySet</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptySet</code>. 
      */
    def unzip3[L, M, R](using asTriple: T => (L, M, R)): (NonEmptySet[L], NonEmptySet[M], NonEmptySet[R]) = toSet.unzip3(asTriple)

    /**
      * Returns a <code>NonEmptySet</code> formed from this <code>NonEmptySet</code> and an iterable collection by combining corresponding
      * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
      * shorter collection to the length of the longer.
      *
      * @tparm O the type of the second half of the returned pairs
      * @tparm U the type of the first half of the returned pairs
      * @param other the <code>IterableOnce</code> providing the second half of each result pair
      * @param thisElem the element to be used to fill up the result if this <code>NonEmptySet</code> is shorter than <code>that</code> <code>IterableOnce</code>.
      * @param otherElem the element to be used to fill up the result if <code>that</code> <code>IterableOnce</code> is shorter than this <code>NonEmptySet</code>.
      * @return a new <code>NonEmptySet</code> containing pairs consisting of corresponding elements of this <code>NonEmptySet</code> and <code>that</code>. The
      *     length of the returned collection is the maximum of the lengths of this <code>NonEmptySet</code> and <code>that</code>. If this <code>NonEmptySet</code>
      *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
      *     <code>NonEmptySet</code>, <code>thatElem</code> values are used to pad the result. 
      */
    def zipAll[O, U >: T](other: IterableOnce[O], thisElem: U, otherElem: O): NonEmptySet[(U, O)] = toSet.zipAll(other.toIterable, thisElem, otherElem)

    /**
      * Zips this <code>NonEmptySet</code>  with its indices.
      *
      * @return A new <code>NonEmptySet</code> containing pairs consisting of all elements of this <code>NonEmptySet</code> paired with their index. Indices start at 0.
      */
    def zipWithIndex: NonEmptySet[(T, Int)] = toSet.zipWithIndex

  }

}