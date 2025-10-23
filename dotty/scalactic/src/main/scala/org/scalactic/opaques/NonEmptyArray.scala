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

import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.GenSeq
import org.scalactic.ColCompatHelper.{Iterable, IterableOnce, GenIterable}
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.reflect.ClassTag
import scala.collection.immutable
import scala.collection.mutable.ArrayBuffer
import scala.collection.ArrayOps
import org.scalactic.{Every, Resources}
import scala.language.higherKinds

// Can't be a LinearSeq[T] because Builder would be able to create an empty one.
/**
  * A non-empty array: an ordered, mutable, non-empty collection of elements with <code>IndexedSeq</code> performance characteristics.
  *
  * <p>
  * The purpose of <code>NonEmptyArray</code> is to allow you to express in a type that an <code>Array</code> is non-empty, thereby eliminating the
  * need for (and potential exception from) a run-time check for non-emptiness. For a non-empty immutable sequence with <code>IndexedSeq</code>
  * performance, see <a href="Every.html"><code>Every</code></a>.
  * </p>
  *
  * <h2>Constructing <code>NonEmptyArray</code>s</h2>
  *
  * <p>
  * You can construct a <code>NonEmptyArray</code> by passing one or more elements to the <code>NonEmptyArray.apply</code> factory method:
  * </p>
  *
  * <pre class="stHighlight">
  * scala&gt; NonEmptyArray(1, 2, 3)
  * res0: org.scalactic.opaques.NonEmptyArray[Int] = NonEmptyArray(1, 2, 3)
  * </pre>
  *
  * <h2>Working with <code>NonEmptyArray</code>s</h2>
  *
  * <p>
  * <code>NonEmptyArray</code> does not extend Scala's <code>Seq</code> or <code>Traversable</code> traits because these require that
  * implementations may be empty. For example, if you invoke <code>tail</code> on a <code>Seq</code> that contains just one element,
  * you'll get an empty <code>Seq</code>:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; Array(1).tail
  * res6: Array[Int] = Array()
  * </pre>
  *
  * <p>
  * On the other hand, many useful methods exist on <code>Seq</code> that when invoked on a non-empty <code>Seq</code> are guaranteed
  * to not result in an empty <code>Seq</code>. For convenience, <code>NonEmptyArray</code> defines a method corresponding to every such <code>Seq</code>
  * method. Here are some examples:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyArray(1, 2, 3).map(_ + 1)                        // Result: NonEmptyArray(2, 3, 4)
  * NonEmptyArray(1).map(_ + 1)                              // Result: NonEmptyArray(2)
  * NonEmptyArray(1, 2, 3).containsSlice(NonEmptyArray(2, 3)) // Result: true
  * NonEmptyArray(1, 2, 3).containsSlice(NonEmptyArray(3, 4)) // Result: false
  * NonEmptyArray(-1, -2, 3, 4, 5).minBy(_.abs)              // Result: -1
  * </pre>
  *
  * <p>
  * <code>NonEmptyArray</code> does <em>not</em> currently define any methods corresponding to <code>Seq</code> methods that could result in
  * an empty <code>Seq</code>. However, an implicit converison from <code>NonEmptyArray</code> to <code>Array</code>
  * is defined in the <code>NonEmptyArray</code> companion object that will be applied if you attempt to call one of the missing methods. As a
  * result, you can invoke <code>filter</code> on an <code>NonEmptyArray</code>, even though <code>filter</code> could result
  * in an empty sequence&mdash;but the result type will be <code>Array</code> instead of <code>NonEmptyArray</code>:
  * </p>
  *
  * <pre class="stHighlight">
  * NonEmptyArray(1, 2, 3).filter(_ &lt; 10) // Result: Array(1, 2, 3)
  * NonEmptyArray(1, 2, 3).filter(_ &gt; 10) // Result: Array()
  * </pre>
  *
  *
  * <p>
  * You can use <code>NonEmptyArray</code>s in <code>for</code> expressions. The result will be an <code>NonEmptyArray</code> unless
  * you use a filter (an <code>if</code> clause). Because filters are desugared to invocations of <code>filter</code>, the
  * result type will switch to a <code>Array</code> at that point. Here are some examples:
  * </p>
  *
  * <pre class="stREPL">
  * scala&gt; import org.scalactic.opaques._
  * import org.scalactic.opaques._
  *
  * scala&gt; for (i &lt;- NonEmptyArray(1, 2, 3)) yield i + 1
  * res0: org.scalactic.opaques.NonEmptyArray[Int] = NonEmptyArray(2, 3, 4)
  *
  * scala&gt; for (i &lt;- NonEmptyArray(1, 2, 3) if i &lt; 10) yield i + 1
  * res1: Array[Int] = Array(2, 3, 4)
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyArray(1, 2, 3)
  *      |   j &lt;- NonEmptyArray('a', 'b', 'c')
  *      | } yield (i, j)
  * res3: org.scalactic.opaques.NonEmptyArray[(Int, Char)] =
  *         NonEmptyArray((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  *
  * scala&gt; for {
  *      |   i &lt;- NonEmptyArray(1, 2, 3) if i &lt; 10
  *      |   j &lt;- NonEmptyArray('a', 'b', 'c')
  *      | } yield (i, j)
  * res6: Array[(Int, Char)] =
  *         Array((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))
  * </pre>
  *
  * @tparam T the type of elements contained in this <code>NonEmptyArray</code>
  */

opaque type NonEmptyArray[T] = Array[T] & { def length: Int & (1 | Int) }

/**
  * Companion object for class <code>NonEmptyArray</code>.
  */
object NonEmptyArray {

  /**
    * Constructs a new <code>NonEmptyArray</code> given at least one element.
    *
    * @tparam T the type of the element contained in the new <code>NonEmptyArray</code>
    * @param firstElement the first element (with index 0) contained in this <code>NonEmptyArray</code>
    * @param otherElements a varargs of zero or more other elements (with index 1, 2, 3, ...) contained in this <code>NonEmptyArray</code>
    */
  def apply[T](firstElement: T, otherElements: T*)(implicit classTag: ClassTag[T]): NonEmptyArray[T] = (firstElement +: otherElements).toArray

  /**
    * Variable argument extractor for <code>NonEmptyArray</code>s.
    *
    * @param nonEmptyArray: the <code>NonEmptyArray</code> containing the elements to extract
    * @return an <code>Seq</code> containing this <code>NonEmptyArray</code>s elements, wrapped in a <code>Some</code> 
    */
  def unapplySeq[T](nonEmptyArray: NonEmptyArray[T]): Option[Seq[T]] = Some(nonEmptyArray)

  //def unapply[A](nea: NonEmptyArray[A]): Some[Array[A]] = Some(nea)

  /*
    // TODO: Figure out how to get case NonEmptyArray() to not compile
    def unapplySeq[T](nonEmptyArray: NonEmptyArray[T]): Option[(T, Seq[T])] = Some(nonEmptyArray.head, nonEmptyArray.tail)
  */

  /**
    * Optionally construct a <code>NonEmptyArray</code> containing the elements, if any, of a given <code>GenSeq</code>.
    *
    * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyArray</code>
    * @return a <code>NonEmptyArray</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
    *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
    */
  def from[T](seq: GenSeq[T])(implicit classTag: ClassTag[T]): Option[NonEmptyArray[T]] =
    seq.headOption match {
      case None => None
      case Some(first) => Some((first +: seq.tail).toArray)
    }

  import scala.language.implicitConversions  

  /**
    * Implicit conversion from <code>NonEmptyArray</code> to <code>GenSeq</code>.
    *
    * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
    * @return the <code>GenSeq</code>
    */
  implicit def nonEmptyArrayToGenSeq[E](nonEmptyArray: NonEmptyArray[E]): scala.collection.GenSeq[E] = // given Conversion just won't work!
    new scala.collection.IndexedSeq[E] {
      def apply(i: Int): E = nonEmptyArray(i)
      def length: Int = nonEmptyArray.length
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
    infix def +:[U >: T](array: NonEmptyArray[U])(using ClassTag[U]): NonEmptyArray[U] = {
      ArrayOps(array).prepended(element)
    }
  }

  extension [T] (nonEmptyArray: NonEmptyArray[T]) {
    /**
      * Returns a new <code>NonEmptyArray</code> containing the elements of this <code>NonEmptyArray</code> followed by the elements of the passed <code>IterableOnce</code>.
      * The element type of the resulting <code>NonEmptyArray</code> is the most specific superclass encompassing the element types of this <code>NonEmptyArray</code>
      * and the passed <code>IterableOnce</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>
      * @param other the <code>IterableOnce</code> to append
      * @return a new <code>NonEmptyArray</code> that contains all the elements of this <code>NonEmptyArray</code> followed by all elements of <code>other</code>.
      */
    infix final def ++[U >: T](other: IterableOnce[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
      ArrayOps(nonEmptyArray).appendedAll(other)
    }

    /**
      * Returns a new <code>NonEmptyArray</code> with the given element appended.
      *
      * <p>
      * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
      * </p>
      *
      * @param element the element to append to this <code>NonEmptyArray</code>
      * @return a new <code>NonEmptyArray</code> consisting of all elements of this <code>NonEmptyArray</code> followed by <code>element</code>.
      */
    def :+[U >: T](element: U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] = { 
      ArrayOps(nonEmptyArray).appended(element)
    }

    /**
      * Indicates whether this <code>NonEmptyArray</code> contains a given value as an element.
      *
      * @param elem the element to look for
      * @return true if this <code>NonEmptyArray</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
      */
    final def contains(elem: T): Boolean = {
      val arrOps = new ArrayOps(nonEmptyArray)
      arrOps.contains(elem)
    }

    /**
    * Builds a new <code>NonEmptyArray</code> from this <code>NonEmptyArray</code> without any duplicate elements.
    *
    * @return A new <code>NonEmptyArray</code> that contains the first occurrence of every element of this <code>NonEmptyArray</code>. 
    */
    final def distinct: NonEmptyArray[T] = {
      val arrOps = new ArrayOps(nonEmptyArray)
      arrOps.distinct
    }

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code> and using the elements of the resulting <code>NonEmptyArray</code>s.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>
      * @param f the function to apply to each element.
      * @return a new <code>NonEmptyArray</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and concatenating
      *    the elements of resulting <code>NonEmptyArray</code>s. 
      */
    final def flatMap[U](f: T => NonEmptyArray[U])(implicit classTag: ClassTag[U]): NonEmptyArray[U] = {
      val buf = new ArrayBuffer[U]
      for (ele <- nonEmptyArray)
        buf ++= f(ele).toArray
      buf.toArray
    }

    /**
      * Converts this <code>NonEmptyArray</code> of <code>NonEmptyArray</code>s into a <code>NonEmptyArray</code>
      * formed by the elements of the nested <code>NonEmptyArray</code>s.
      *
      * <p>
      * Note: You cannot use this <code>flatten</code> method on a <code>NonEmptyArray</code> that contains a <code>IterableOnce</code>s, because 
      * if all the nested <code>IterableOnce</code>s were empty, you'd end up with an empty <code>NonEmptyArray</code>.
      * </p>
      *
      * @tparm B the type of the elements of each nested <code>NonEmptyArray</code>
      * @return a new <code>NonEmptyArray</code> resulting from concatenating all nested <code>NonEmptyArray</code>s.
      */
    final def flatten[B](implicit ev: T <:< NonEmptyArray[B], classTag: ClassTag[B]): NonEmptyArray[B] = flatMap(ev)

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code>.
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyArray</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and collecting the results. 
      */
    final def map[U](f: T => U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] ={
      val buf = new ArrayBuffer[U]
      for (ele <- nonEmptyArray)
        buf += f(ele)
      buf.toArray
    }

    /**
      * Partitions this <code>NonEmptyArray</code> into a map of <code>NonEmptyArray</code>s according to some discriminator function.
      *
      * @tparam K the type of keys returned by the discriminator function.
      * @param f the discriminator function.
      * @return A map from keys to <code>NonEmptyArray</code>s such that the following invariant holds:
      *
      * <pre>
      * (nonEmptyArray.toArray partition f)(k) = xs filter (x =&gt; f(x) == k)
      * </pre>
      *
      * <p>
      * That is, every key <code>k</code> is bound to a <code>NonEmptyArray</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
      * </p>
      */
    final def groupBy[K](f: T => K)(implicit classTag: ClassTag[T]): Map[K, NonEmptyArray[T]] = {
      val mapKToArray = (new ArrayOps(nonEmptyArray)).groupBy(f)
      (mapKToArray.mapValues{ list => NonEmptyArray(list.head, list.tail.toList*) }).toMap
    }

    /**
      * Partitions elements into fixed size <code>NonEmptyArray</code>s.
      *
      * @param size the number of elements per group
      * @return An iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
      */
    final def grouped(size: Int)(implicit classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = {
      if (size < 1)
        throw new IllegalArgumentException(Resources.invalidSize(size))
      val itOfArray = (new ArrayOps(nonEmptyArray)).grouped(size)
      itOfArray.map { list => NonEmptyArray(list.head, list.tail.toList*) }
    }

    /**
      * A copy of this <code>NonEmptyArray</code> with an element value appended until a given target length is reached.
      *
      * @param len the target length 
      * @param elem he padding value
      * @return a new <code>NonEmptyArray</code> consisting of all elements of this <code>NonEmptyArray</code> followed by the minimal number of occurrences
      *     of <code>elem</code> so that the resulting <code>NonEmptyArray</code> has a length of at least <code>len</code>. 
      */
    final def padTo[U >: T](len: Int, elem: U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] = {
      (new ArrayOps(nonEmptyArray)).padTo(len, elem)
    }

    /**
      * Produces a new <code>NonEmptyArray</code> where a slice of elements in this <code>NonEmptyArray</code> is replaced by another <code>NonEmptyArray</code>
      *
      * @param from the index of the first replaced element 
      * @param that the <code>NonEmptyArray</code> whose elements should replace a slice in this <code>NonEmptyArray</code>
      * @param replaced the number of elements to drop in the original <code>NonEmptyArray</code>
      */
    final def patch[U >: T](from: Int, that: NonEmptyArray[U], replaced: Int)(implicit classTag: ClassTag[U]): NonEmptyArray[U] =
      (new ArrayOps(nonEmptyArray)).patch(from, that.toArray, replaced)

    /**
      * Iterates over distinct permutations. 
      *
      * <p>
      * Here's an example:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray('a', 'b', 'b').permutations.toArray = Array(NonEmptyArray(a, b, b), NonEmptyArray(b, a, b), NonEmptyArray(b, b, a))
      * </pre>
      *
      * @return an iterator that traverses the distinct permutations of this <code>NonEmptyArray</code>.
      */
    final def permutations: Iterator[NonEmptyArray[T]] =
      (new ArrayOps(nonEmptyArray)).permutations

    /**
      * Returns new <code>NonEmptyArray</code> with elements in reverse order.
      *
      * @return a new <code>NonEmptyArray</code> with all elements of this <code>NonEmptyArray</code> in reversed order. 
      */
    final def reverse: NonEmptyArray[T] =
      (new ArrayOps(nonEmptyArray)).reverse

    /**
      * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code> and collecting the results in reverse order.
      *
      * <p>
      * Note: <code>nonEmptyArray.reverseMap(f)</code> is the same as <code>nonEmptyArray.reverse.map(f)</code>, but might be more efficient. 
      * </p>
      *
      * @tparam U the element type of the returned <code>NonEmptyArray</code>.
      * @param f the function to apply to each element. 
      * @return a new <code>NonEmptyArray</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code>
      *     and collecting the results in reverse order. 
      */
    final def reverseMap[U](f: T => U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] =
      nonEmptyArrayToGenSeq(nonEmptyArray).reverseMap(f).toArray

    /**
      * Computes a prefix scan of the elements of this <code>NonEmptyArray</code>.
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
      * NonEmptyArray(1, 2, 3).scan(0)(_ + _) == NonEmptyArray(0, 1, 3, 6)
      * NonEmptyArray(1, 2, 3).scan("z")(_ + _.toString) == NonEmptyArray("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam U a type parameter for the binary operator, a supertype of T, and the type of the resulting <code>NonEmptyArray</code>.
      * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
      *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
      *     0 for addition, or 1 for multiplication.)
      * @param op a binary operator that must be associative
      * @return a new <code>NonEmptyArray</code> containing the prefix scan of the elements in this <code>NonEmptyArray</code> 
      */
    final def scan[U >: T](z: U)(op: (U, U) => U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] = new ArrayOps(nonEmptyArray).scan(z)(op)

    /**
      * Produces a <code>NonEmptyArray</code> containing cumulative results of applying the operator going left to right.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray(1, 2, 3).scanLeft(0)(_ + _) == NonEmptyArray(0, 1, 3, 6)
      * NonEmptyArray(1, 2, 3).scanLeft("z")(_ + _) == NonEmptyArray("z", "z1", "z12", "z123")
      * </pre>
      *
      * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyArray</code>
      * @param z the start value.
      * @param op the binary operator.
      * @return a new <code>NonEmptyArray</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>,
      *     going left to right, with the start value, <code>z</code>, on the left.
      */
    final def scanLeft[B](z: B)(op: (B, T) => B)(implicit classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanLeft(z)(op)

    /**
      * Produces a <code>NonEmptyArray</code> containing cumulative results of applying the operator going right to left.
      *
      * <p>
      * Here are some examples:
      * </p>
      *
      * <pre class="stHighlight">
      * NonEmptyArray(1, 2, 3).scanRight(0)(_ + _) == NonEmptyArray(6, 5, 3, 0)
      * NonEmptyArray(1, 2, 3).scanRight("z")(_ + _) == NonEmptyArray("123z", "23z", "3z", "z")
      * </pre>
      *
      * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyArray</code>
      * @param z the start value
      * @param op the binary operator
      * @return a new <code>NonEmptyArray</code> containing the intermediate results of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>,
      *     going right to left, with the start value, <code>z</code>, on the right.
      */
    final def scanRight[B](z: B)(op: (T, B) => B)(implicit classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanRight(z)(op)

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
      *
      * @param size the number of elements per group
      * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    final def sliding(size: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size).map(l => NonEmptyArray(l.head, l.tail.toList*))

    /**
      * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
      * moving the sliding window by a given <code>step</code> each time.
      *
      * @param size the number of elements per group
      * @param step the distance between the first elements of successive groups
      * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
      *     if there are fewer elements than <code>size</code>.
      */
    final def sliding(size: Int, step: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size, step).map(l => NonEmptyArray(l.head, l.tail.toList*))

    /**
      * Sorts this <code>NonEmptyArray</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
      *
      * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
      * @param f the transformation function mapping elements to some other domain <code>U</code>.
      * @param ord the ordering assumed on domain <code>U</code>.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the <code>Ordering</code> where
      *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
      */
    final def sortBy[U](f: T => U)(using ord: Ordering[U]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortBy(f)

    /**
      * Sorts this <code>NonEmptyArray</code> according to a comparison function.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyArray</code> as in the original. 
      * </p>
      *
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the comparison function <code>lt</code>.
      */
    final def sortWith(lt: (T, T) => Boolean): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortWith(lt)

    /**
      * Sorts this <code>NonEmptyArray</code> according to an <code>Ordering</code>.
      *
      * <p>
      * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
      * sorted <code>NonEmptyArray</code> as in the original. 
      * </p>
      *
      * @param ord the <code>Ordering</code> to be used to compare elements.
      * @param the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
      * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the comparison function <code>lt</code>.
      */
    final def sorted(using ord: Ordering[T]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sorted(ord)

    /**
      * Returns <code>"NonEmptyArray"</code>, the prefix of this object's <code>toString</code> representation.
      *
      * @return the string <code>"NonEmptyArray"</code>
      */
    def stringPrefix: String = "NonEmptyArray"

    /**
      * Converts this <code>NonEmptyArray</code> of pairs into two <code>NonEmptyArray</code>s of the first and second half of each pair. 
      *
      * @tparam L the type of the first half of the element pairs
      * @tparam R the type of the second half of the element pairs
      * @param asPair an implicit conversion that asserts that the element type of this <code>NonEmptyArray</code> is a pair.
      * @return a pair of <code>NonEmptyArray</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyArray</code>. 
      */
    final def unzip[L, R](implicit asPair: T => (L, R), classTagL: ClassTag[L], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[R]) = {
      val unzipped = new ArrayOps(nonEmptyArray).unzip
      val left: NonEmptyArray[L] = unzipped._1.toArray
      val right: NonEmptyArray[R] = unzipped._2.toArray
      (left, right)
    }

    /**
      * Converts this <code>NonEmptyArray</code> of triples into three <code>NonEmptyArray</code>s of the first, second, and and third element of each triple. 
      *
      * @tparam L the type of the first member of the element triples
      * @tparam R the type of the second member of the element triples
      * @tparam R the type of the third member of the element triples
      * @param asTriple an implicit conversion that asserts that the element type of this <code>NonEmptyArray</code> is a triple.
      * @return a triple of <code>NonEmptyArray</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptyArray</code>. 
      */
    final def unzip3[L, M, R](implicit asTriple: T => (L, M, R), classTagL: ClassTag[L], classTagM: ClassTag[M], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[M], NonEmptyArray[R]) = {
      val unzipped = new ArrayOps(nonEmptyArray).unzip3
      val left: NonEmptyArray[L] = unzipped._1.toArray
      val middle: NonEmptyArray[M] = unzipped._2.toArray
      val right: NonEmptyArray[R] = unzipped._3.toArray
      (left, middle, right)
    }

    /**
      * A copy of this <code>NonEmptyArray</code> with one single replaced element.
      *
      * @param idx the position of the replacement
      * @param elem the replacing element
      * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyArray</code>
      * @return a copy of this <code>NonEmptyArray</code> with the element at position <code>idx</code> replaced by <code>elem</code>. 
      */
    final def updated[U >: T](idx: Int, elem: U)(implicit classTag: ClassTag[U]): NonEmptyArray[U] =
      new ArrayOps(nonEmptyArray).updated(idx, elem)

    /**
      * Returns a <code>NonEmptyArray</code> formed from this <code>NonEmptyArray</code> and an iterable collection by combining corresponding
      * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
      * shorter collection to the length of the longer.
      *
      * @tparm O the type of the second half of the returned pairs
      * @tparm U the type of the first half of the returned pairs
      * @param other the <code>Iterable</code> providing the second half of each result pair
      * @param thisElem the element to be used to fill up the result if this <code>NonEmptyArray</code> is shorter than <code>that</code> <code>Iterable</code>.
      * @param thatElem the element to be used to fill up the result if <code>that</code> <code>Iterable</code> is shorter than this <code>NonEmptyArray</code>.
      * @return a new <code>NonEmptyArray</code> containing pairs consisting of corresponding elements of this <code>NonEmptyArray</code> and <code>that</code>. The
      *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyArray</code> and <code>that</code>. If this <code>NonEmptyArray</code>
      *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
      *     <code>NonEmptyArray</code>, <code>thatElem</code> values are used to pad the result. 
      */
    final def zipAll[O, U >: T](other: collection.Iterable[O], thisElem: U, otherElem: O): NonEmptyArray[(U, O)] =
      new ArrayOps(nonEmptyArray).zipAll(other, thisElem, otherElem)

    /**
      * Zips this <code>NonEmptyArray</code>  with its indices.
      *
      * @return A new <code>NonEmptyArray</code> containing pairs consisting of all elements of this <code>NonEmptyArray</code> paired with their index. Indices start at 0.
      */
    final def zipWithIndex: NonEmptyArray[(T, Int)] = new ArrayOps(nonEmptyArray).zipWithIndex  
  }
}
