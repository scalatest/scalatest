/*
 * Copyright 2001-2026 Artima, Inc.
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

import org.scalactic.Every
import org.scalactic.Resources
import org.scalactic.{Every, Resources}
import scala.annotation.targetName
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.collection.ArrayOps
import scala.collection.GenSeq
import scala.collection.GenSet
import scala.collection.generic.CanBuildFrom
import scala.collection.mutable.Buffer
import scala.collection.mutable.{ArrayBuffer, Buffer}
import scala.collection.{GenSeq, StringOps}
import scala.compiletime.{ constValueOpt, error }
import scala.language.higherKinds
import scala.reflect.ClassTag

object NonEmpties {

  opaque type NonEmptyArray[T] = Array[T]

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
    def apply[T](firstElement: T, otherElements: T*)(using classTag: ClassTag[T]): NonEmptyArray[T] = (firstElement +: otherElements).toArray

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
     *
     * A factory/assertion method that produces a <code>NonEmptyArray</code>
     * given a valid <code>Array</code> value, or throws
     * <code>AssertionError</code>, if given an invalid <code>Array</code> value.
     *
     * Note: you should use this method only when you are convinced that it will
     * always succeed, i.e., never throw an exception. It is good practice to
     * add a comment near the invocation of this method indicating ''why'' you
     * think it will always succeed to document your reasoning. If you are not
     * sure an `ensuringValid` call will always succeed, you should use one of
     * the other factory or validation methods provided on this object instead:
     * `from'.
     *
     * @param array the <code>Array</code> to check to see if it is a valid.
     * @return the <code>NonEmptyArray</code> if the passed array is valid..
     * @throws AssertionError if the passed array is not valid.
     */
    def ensuringValid[T](array: Array[T]): NonEmptyArray[T] =
      if (array.length == 0)
        throw new AssertionError(Resources.nonEmptyArrayEmpty)
      else
        array

    /**
      * Optionally construct a <code>NonEmptyArray</code> containing the elements, if any, of a given <code>GenSeq</code>.
      *
      * @param seq the <code>GenSeq</code> with which to construct a <code>NonEmptyArray</code>
      * @return a <code>NonEmptyArray</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
      *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
      */
    def from[T](seq: GenSeq[T])(using classTag: ClassTag[T]): Option[NonEmptyArray[T]] =
      seq.headOption match {
        case None => None
        case Some(first) => Some((first +: seq.tail).toArray)
      } 

    /**
      * Conversion from <code>NonEmptyArray</code> to <code>IterableOnce</code>.
      *
      * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
      * @return the <code>IterableOnce</code>
      */
    given [E]: Conversion[NonEmptyArray[E], IterableOnce[E]] with {
      def apply(nonEmptyArray: NonEmptyArray[E]): IterableOnce[E] = 
        new IterableOnce[E] {
          def iterator: Iterator[E] = new ArrayOps(nonEmptyArray).iterator
        }
    }
    /**
      * Conversion from <code>NonEmptyArray</code> to <code>PartialFunction</code>.
      *
      * @param nonEmptyArray the <code>NonEmptyArray</code> to convert
      * @return the <code>PartialFunction</code>
      */
    given [E]: Conversion[NonEmptyArray[E], PartialFunction[Int, E]] with {
      def apply(nonEmptyArray: NonEmptyArray[E]): PartialFunction[Int, E] =
        new PartialFunction[Int, E] {
          def apply(i: Int): E = {
            if (i < 0 || i >= nonEmptyArray.length)
              throw new IndexOutOfBoundsException(Resources.indexOutOfBounds(i, nonEmptyArray.length))
            nonEmptyArray.toArray.apply(i)
          }
          def isDefinedAt(i: Int): Boolean = i >= 0 && i < nonEmptyArray.length
        }
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
      infix def ++[U >: T](other: IterableOnce[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
        ArrayOps(nonEmptyArray) ++ other
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
      infix def :+[U >: T](element: U)(using classTag: ClassTag[U]): NonEmptyArray[U] = { 
        ArrayOps(nonEmptyArray).appended(element)
      }

      /**
        * Appends all elements of this <code>NonEmptyArray</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyArray</code>, without any separator string.
        *
        * @param sb the string builder to which elements will be appended
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb)

      /**
        * Appends all elements of this <code>NonEmptyArray</code> to a string builder using a separator string. The written text will consist of a concatenation of the
        * result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyArray</code>, separated by the string <code>sep</code>.
        *
        * @param sb the string builder to which elements will be appended
        * @param sep the separator string
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder, sep: String): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb, sep)

      /**
        * Appends all elements of this <code>NonEmptyArray</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
        * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptyArray</code>,
        * separated by the string <code>sep</code>; and the string <code>end</code>
        *
        * @param sb the string builder to which elements will be appended
        * @param start the starting string
        * @param sep the separator string
        * @param start the ending string
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = nonEmptyArray.toIndexedSeq.addString(sb, start, sep, end)

      /**
        * Finds the first element of this <code>NonEmptyArray</code> for which the given partial function is defined, if any, and applies the partial function to it.
        *
        * @param pf the partial function
        * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
        *    the partial function was not defined for any element.
        */
      def collectFirst[U](pf: PartialFunction[T, U])(using classTagOfU: ClassTag[U]): Option[U] = new ArrayOps(nonEmptyArray).collectFirst(pf)

      /**
        * Indicates whether this <code>NonEmptyArray</code> contains a given value as an element.
        *
        * @param elem the element to look for
        * @return true if this <code>NonEmptyArray</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
        */
      def contains(elem: T): Boolean = {
        val arrOps = new ArrayOps(nonEmptyArray)
        arrOps.contains(elem)
      }

      /**
        * Indicates whether this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
        *
        * @param that the <code>IterableOnce</code> slice to look for
        * @return true if this <code>NonEmptyArray</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
        */
      def containsSlice[B](that: IterableOnce[B]): Boolean = nonEmptyArray.toIndexedSeq.containsSlice(that.toList)

      /**
      * Builds a new <code>NonEmptyArray</code> from this <code>NonEmptyArray</code> without any duplicate elements.
      *
      * @return A new <code>NonEmptyArray</code> that contains the first occurrence of every element of this <code>NonEmptyArray</code>. 
      */
      def distinct: NonEmptyArray[T] = {
        val arrOps = new ArrayOps(nonEmptyArray)
        arrOps.distinct
      }

      /**
        * Indicates whether this <code>NonEmptyArray</code> ends with the given <code>IterableOnce</code>.
        *
        * @param that the sequence to test
        * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
        */
      def endsWith[B](that: IterableOnce[B]): Boolean = new ArrayOps(nonEmptyArray).endsWith(that.toList)

      /**
        * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptyArray</code>.
        *
        * @param the predicate used to test elements.
        * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyArray</code>, otherwise <code>false</code>. 
        */
      def exists(p: T => Boolean): Boolean = new ArrayOps(nonEmptyArray).exists(p)

      /**
        * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code> and using the elements of the resulting <code>NonEmptyArray</code>s.
        *
        * @tparam U the element type of the returned <code>NonEmptyArray</code>
        * @param f the function to apply to each element.
        * @return a new <code>NonEmptyArray</code> containing elements obtained by applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and concatenating
        *    the elements of resulting <code>NonEmptyArray</code>s. 
        */
      def flatMap[U](f: T => NonEmptyArray[U])(using classTag: ClassTag[U]): NonEmptyArray[U] = {
        val buf = new ArrayBuffer[U]
        for (ele <- nonEmptyArray)
          buf ++= f(ele).toArray
        buf.toArray
      }

      /**
        * Finds the first element of this <code>NonEmptyArray</code> that satisfies the given predicate, if any.
        *
        * @param p the predicate used to test elements
        * @return an <code>Some</code> containing the first element in this <code>NonEmptyArray</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
        */
      def find(p: T => Boolean): Option[T] = new ArrayOps(nonEmptyArray).find(p)

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
      def flatten[B](using ev: T <:< NonEmptyArray[B], classTag: ClassTag[B]): NonEmptyArray[B] = flatMap(ev)

      /**
        * Folds the elements of this <code>NonEmptyArray</code> using the specified associative binary operator.
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
      def fold[U >: T](z: U)(op: (U, U) => U): U = new ArrayOps(nonEmptyArray).fold(z)(op)

      /**
        * Applies a binary operator to a start value and all elements of this <code>NonEmptyArray</code>, going left to right.
        *
        * @tparam B the result type of the binary operator.
        * @param z the start value.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going left to right, with the start value,
        *     <code>z</code>, on the left:
        *
        * <pre>
        * op(...op(op(z, x_1), x_2), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
        * </p>
        */
      def foldLeft[B](z: B)(op: (B, T) => B): B = new ArrayOps(nonEmptyArray).foldLeft(z)(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyArray</code> and a start value, going right to left.
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going right to left, with the start value,
        *     <code>z</code>, on the right:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_n, z)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
        * </p>
        */
      def foldRight[B](z: B)(op: (T, B) => B): B = new ArrayOps(nonEmptyArray).foldRight(z)(op)

      /**
        * Indicates whether a predicate holds for all elements of this <code>NonEmptyArray</code>.
        *
        * @param p the predicate used to test elements.
        * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptyArray</code>, otherwise <code>false</code>. 
        */
      def forall(p: T => Boolean): Boolean = new ArrayOps(nonEmptyArray).forall(p)

      /**
        * Applies a function <code>f</code> to all elements of this <code>NonEmptyArray</code>.
        *
        * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
        */
      def foreach(f: T => Unit): Unit = new ArrayOps(nonEmptyArray).foreach(f)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyArray</code> has a definite size, since all <code>NonEmptyArray</code>s are strict collections.
        */
      def hasDefiniteSize: Boolean = true

      /**
        * Selects the first element of this <code>NonEmptyArray</code>. 
        *
        * @return the first element of this <code>NonEmptyArray</code>.
        */
      def head: T = new ArrayOps(nonEmptyArray).head

      /**
        * Selects the first element of this <code>NonEmptyArray</code> and returns it wrapped in a <code>Some</code>. 
        *
        * @return the first element of this <code>NonEmptyArray</code>, wrapped in a <code>Some</code>.
        */
      def headOption: Option[T] = new ArrayOps(nonEmptyArray).headOption

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyArray</code>.
        *
        * @param elem the element value to search for. 
        * @return the index of the first element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(elem: T): Int = new ArrayOps(nonEmptyArray).indexOf(elem, 0)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyArray</code> after or at some start index.
        *
        * @param elem the element value to search for. 
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(elem: T, from: Int): Int = new ArrayOps(nonEmptyArray).indexOf(elem, from)


      /**
        * Finds first index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @return the first index at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def indexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyArray.toIndexedSeq.indexOfSlice(that.toList)

      /**
        * Finds first index after or at a start index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice.
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @param from the start index
        * @return the first index <code>&gt;=</code> <code>from</code> at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def indexOfSlice[U >: T](that: IterableOnce[U], from: Int): Int = nonEmptyArray.toIndexedSeq.indexOfSlice(that.toList, from)

      /**
        * Finds index of the first element satisfying some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the index of the first element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: T => Boolean): Int = new ArrayOps(nonEmptyArray).indexWhere(p)

      /**
        * Finds index of the first element satisfying some predicate after or at some start index.
        *
        * @param p the predicate used to test elements.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: T => Boolean, from: Int): Int = new ArrayOps(nonEmptyArray).indexWhere(p, from)

      /**
        * Produces the range of all indices of this <code>NonEmptyArray</code>. 
        *
        * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyArray</code>. 
        */
      def indices: Range = new ArrayOps(nonEmptyArray).indices

      /**
        * Returns <code>false</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, is non-empty.
        *
        * @return false
        */
      def isEmpty: Boolean = false

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, can be traversed repeatedly.
        *
        * @return true
        */
      def isTraversableAgain: Boolean = true

      /**
        * Selects the last element of this <code>NonEmptyArray</code>. 
        *
        * @return the last element of this <code>NonEmptyArray</code>.
        */
      def last: T = new ArrayOps(nonEmptyArray).last

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyArray</code>.
        *
        * @param elem the element value to search for.
        * @return the index of the last element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def lastIndexOf(elem: T): Int = new ArrayOps(nonEmptyArray).lastIndexOf(elem)

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyArray</code> before or at a given <code>end</code> index.
        *
        * @param elem the element value to search for.
        * @param end the end index. 
        * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyArray</code> that is equal (as determined by <code>==</code>)
        *     to <code>elem</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexOf(elem: T, end: Int): Int = new ArrayOps(nonEmptyArray).lastIndexOf(elem, end)

      /**
        * Finds the last index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice. 
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @return the last index at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
        *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyArray.toIndexedSeq.lastIndexOfSlice(that.toList)

      /**
        * Finds the last index before or at a given end index where this <code>NonEmptyArray</code> contains a given <code>IterableOnce</code> as a slice. 
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @param end the end index
        * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyArray</code> starting at that index match the elements of
        *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice[U >: T](that: IterableOnce[U], end: Int): Int = nonEmptyArray.toIndexedSeq.lastIndexOfSlice(that.toList, end)

      /**
        * Finds index of last element satisfying some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the index of the last element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: T => Boolean): Int = nonEmptyArray.toIndexedSeq.lastIndexWhere(p)

      /**
        * Finds index of last element satisfying some predicate before or at given end index.
        *
        * @param p the predicate used to test elements.
        * @param end the end index
        * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyArray</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: T => Boolean, end: Int): Int = nonEmptyArray.toIndexedSeq.lastIndexWhere(p, end)

      /**
        * Returns the last element of this <code>NonEmptyArray</code>, wrapped in a <code>Some</code>. 
        *
        * @return the last element, wrapped in a <code>Some</code>. 
        */
      def lastOption: Option[T] = new ArrayOps(nonEmptyArray).lastOption // Will always return a Some

      /**
        * The length of this <code>NonEmptyArray</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of elements in this <code>NonEmptyArray</code>. 
        */
      def length: Int = nonEmptyArray.toArray.length

      /**
        * Compares the length of this <code>NonEmptyArray</code> to a test value. 
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
      def lengthCompare(len: Int): Int = new ArrayOps(nonEmptyArray).lengthCompare(len)

      /**
        * Builds a new <code>NonEmptyArray</code> by applying a function to all elements of this <code>NonEmptyArray</code>.
        *
        * @tparam U the element type of the returned <code>NonEmptyArray</code>.
        * @param f the function to apply to each element. 
        * @return a new <code>NonEmptyArray</code> resulting from applying the given function <code>f</code> to each element of this <code>NonEmptyArray</code> and collecting the results. 
        */
      def map[U](f: T => U)(using classTag: ClassTag[U]): NonEmptyArray[U] ={
        val buf = new ArrayBuffer[U]
        for (ele <- nonEmptyArray)
          buf += f(ele)
        buf.toArray
      }

      /**
        * Finds the largest element.
        *
        * @return the largest element of this <code>NonEmptyArray</code>. 
        */
      def max[U >: T](using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.max(cmp)

      /**
        * Finds the largest result after applying the given function to every element.
        *
        * @return the largest result of applying the given function to every element of this <code>NonEmptyArray</code>. 
        */
      def maxBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.maxBy(f)(cmp)

      /**
        * Finds the smallest element.
        *
        * @return the smallest element of this <code>NonEmptyArray</code>. 
        */
      def min[U >: T](using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.min(cmp)

      /**
        * Finds the smallest result after applying the given function to every element.
        *
        * @return the smallest result of applying the given function to every element of this <code>NonEmptyArray</code>. 
        */
      def minBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyArray.toIndexedSeq.minBy(f)(cmp)

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
      def groupBy[K](f: T => K)(using classTag: ClassTag[T]): Map[K, NonEmptyArray[T]] = {
        val mapKToArray = (new ArrayOps(nonEmptyArray)).groupBy(f)
        (mapKToArray.mapValues{ list => NonEmptyArray(list.head, list.tail.toList*) }).toMap
      }

      /**
        * Partitions elements into fixed size <code>NonEmptyArray</code>s.
        *
        * @param size the number of elements per group
        * @return An iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last will be truncated if the elements don't divide evenly. 
        */
      def grouped(size: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = {
        if (size < 1)
          throw new IllegalArgumentException(Resources.invalidSize(size))
        val itOfArray = (new ArrayOps(nonEmptyArray)).grouped(size)
        itOfArray.map { list => NonEmptyArray(list.head, list.tail.toList*) }
      }

      /**
        * Displays all elements of this <code>NonEmptyArray</code> in a string. 
        *
        * @return a string representation of this <code>NonEmptyArray</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyArray</code> follow each other without any separator string. 
        */
      def mkString: String = nonEmptyArray.toIndexedSeq.mkString

      /**
        * Displays all elements of this <code>NonEmptyArray</code> in a string using a separator string. 
        *
        * @param sep the separator string
        * @return a string representation of this <code>NonEmptyArray</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyArray</code> are separated by the string <code>sep</code>. 
        */
      def mkString(sep: String): String = nonEmptyArray.toIndexedSeq.mkString(sep)

      /**
        * Displays all elements of this <code>NonEmptyArray</code> in a string using start, end, and separator strings. 
        *
        * @param start the starting string.
        * @param sep the separator string.
        * @param end the ending string.
        * @return a string representation of this <code>NonEmptyArray</code>. The resulting string begins with the string <code>start</code> and ends with the string
        *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>NonEmptyArray</code> are
        *     separated by the string <code>sep</code>. 
        */
      def mkString(start: String, sep: String, end: String): String = nonEmptyArray.toIndexedSeq.mkString(start, sep, end)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyArray</code>, like all <code>NonEmptyArray</code>s, is non-empty.
        *
        * @return true
        */
      def nonEmpty: Boolean = true

      /**
        * A copy of this <code>NonEmptyArray</code> with an element value appended until a given target length is reached.
        *
        * @param len the target length 
        * @param elem he padding value
        * @return a new <code>NonEmptyArray</code> consisting of all elements of this <code>NonEmptyArray</code> followed by the minimal number of occurrences
        *     of <code>elem</code> so that the resulting <code>NonEmptyArray</code> has a length of at least <code>len</code>. 
        */
      def padTo[U >: T](len: Int, elem: U)(using classTag: ClassTag[U]): NonEmptyArray[U] = {
        (new ArrayOps(nonEmptyArray)).padTo(len, elem)
      }

      /**
        * Produces a new <code>NonEmptyArray</code> where a slice of elements in this <code>NonEmptyArray</code> is replaced by another <code>NonEmptyArray</code>
        *
        * @param from the index of the first replaced element 
        * @param that the <code>NonEmptyArray</code> whose elements should replace a slice in this <code>NonEmptyArray</code>
        * @param replaced the number of elements to drop in the original <code>NonEmptyArray</code>
        */
      def patch[U >: T](from: Int, that: NonEmptyArray[U], replaced: Int)(using classTag: ClassTag[U]): NonEmptyArray[U] =
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
      def permutations: Iterator[NonEmptyArray[T]] =
        (new ArrayOps(nonEmptyArray)).permutations

      /**
        * Returns the length of the longest prefix whose elements all satisfy some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the length of the longest prefix of this <code>NonEmptyArray</code> such that every element
        *     of the segment satisfies the predicate <code>p</code>. 
        */
      def prefixLength(p: T => Boolean): Int = nonEmptyArray.toIndexedSeq.prefixLength(p)  

      /**
        * The result of multiplying all the elements of this <code>NonEmptyArray</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyArray[T]</code> for which an given <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the product of all elements
        */
      def product[U >: T](using num: Numeric[U]): U = nonEmptyArray.toIndexedSeq.product(num)

      /**
        * Reduces the elements of this <code>NonEmptyArray</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
        * </p>
        *
        * @tparam U a type parameter for the binary operator, a supertype of T.
        * @param op a binary operator that must be associative.
        * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptyArray</code>.
        */
      def reduce[U >: T](op: (U, U) => U): U = nonEmptyArray.toIndexedSeq.reduce(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going left to right.
        *
        * @tparam U the result type of the binary operator.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going left to right:
        *
        * <pre>
        * op(...op(op(x_1, x_2), x_3), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
        * </p>
        */
      def reduceLeft[U >: T](op: (U, T) => U): U = nonEmptyArray.toIndexedSeq.reduceLeft(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going left to right, returning the result in a <code>Some</code>.
        *
        * @tparam U the result type of the binary operator.
        * @param op the binary operator.
        * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
        * </p>
        */
      def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceLeftOption(op)

      def reduceOption[U >: T](op: (U, U) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceOption(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going right to left.
        *
        * @tparam U the result of the binary operator
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyArray</code>, going right to left:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyArray</code>. 
        * </p>
        */
      def reduceRight[U >: T](op: (T, U) => U): U = nonEmptyArray.toIndexedSeq.reduceRight(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyArray</code>, going right to left, returning the result in a <code>Some</code>.
        *
        * @tparam U the result of the binary operator
        * @param op the binary operator
        * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
        */
      def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = nonEmptyArray.toIndexedSeq.reduceRightOption(op)

      /**
        * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyArray</code>. Copying
        * will stop once either the end of the current <code>NonEmptyArray</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        */
      def copyToArray[U >: T](arr: Array[U]): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, 0)

      /**
        * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyArray</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyArray</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        * @param start the starting index
        */
      def copyToArray[U >: T](arr: Array[U], start: Int): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, start)

      /**
        * Copies values of this <code>NonEmptyArray</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptyArray</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyArray</code> is reached, the end of the array is reached, or
        * <code>len</code> elements have been copied.
        *
        * @param arr the array to fill
        * @param start the starting index
        * @param len the maximum number of elements to copy
        */
      def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = new ArrayOps(nonEmptyArray).copyToArray(arr, start, len)

      /**
        * Copies all elements of this <code>NonEmptyArray</code> to a buffer. 
        *
        * @param buf the buffer to which elements are copied
        */
      def copyToBuffer[U >: T](buf: Buffer[U]): Unit = nonEmptyArray.toIndexedSeq.copyToBuffer(buf)

      /**
        * Indicates whether every element of this <code>NonEmptyArray</code> relates to the corresponding element of a given <code>IterableOnce</code> by satisfying a given predicate. 
        *
        * @tparam B the type of the elements of <code>that</code>
        * @param that the <code>IterableOnce</code> to compare for correspondence
        * @param p the predicate, which relates elements from this <code>NonEmptyArray</code> and the passed <code>IterableOnce</code>
        * @return true if this <code>NonEmptyArray</code> and the passed <code>IterableOnce</code> have the same length and <code>p(x, y)</code> is <code>true</code>
        *     for all corresponding elements <code>x</code> of this <code>NonEmptyArray</code> and <code>y</code> of that, otherwise <code>false</code>.
        */
      def corresponds[B](that: IterableOnce[B])(p: (T, B) => Boolean): Boolean = nonEmptyArray.toIndexedSeq.corresponds(that)(p)

      /**
        * Counts the number of elements in this <code>NonEmptyArray</code> that satisfy a predicate. 
        *
        * @param p the predicate used to test elements.
        * @return the number of elements satisfying the predicate <code>p</code>. 
        */
      def count(p: T => Boolean): Int = new ArrayOps(nonEmptyArray).count(p)

      /**
        * Returns new <code>NonEmptyArray</code> with elements in reverse order.
        *
        * @return a new <code>NonEmptyArray</code> with all elements of this <code>NonEmptyArray</code> in reversed order. 
        */
      def reverse: NonEmptyArray[T] =
        (new ArrayOps(nonEmptyArray)).reverse

      /**
        * An iterator yielding elements in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyArray.reverseIterator</code> is the same as <code>nonEmptyArray.reverse.iterator</code>, but might be more efficient. 
        * </p>
        *
        * @return an iterator yielding the elements of this <code>NonEmptyArray</code> in reversed order 
        */
      def reverseIterator: Iterator[T] = new ArrayOps(nonEmptyArray).reverseIterator  

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
      def reverseMap[U](f: T => U)(using classTag: ClassTag[U]): NonEmptyArray[U] =
        nonEmptyArray.toIndexedSeq.reverseMap(f).toArray

      /**
        * Checks if the given <code>IterableOnce</code> contains the same elements in the same order as this <code>NonEmptyArray</code>.
        *
        * @param that the <code>IterableOnce</code> with which to compare
        * @return <code>true</code>, if both this <code>NonEmptyArray</code> and the given <code>IterableOnce</code> contain the same elements
        *     in the same order, <code>false</code> otherwise. 
        */
      def sameElements[U >: T](that: IterableOnce[U]): Boolean = nonEmptyArray.toIndexedSeq.sameElements(that)

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
      def scan[U >: T](z: U)(op: (U, U) => U)(using classTag: ClassTag[U]): NonEmptyArray[U] = new ArrayOps(nonEmptyArray).scan(z)(op)

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
      def scanLeft[B](z: B)(op: (B, T) => B)(using classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanLeft(z)(op)

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
      def scanRight[B](z: B)(op: (T, B) => B)(using classTag: ClassTag[B]): NonEmptyArray[B] = new ArrayOps(nonEmptyArray).scanRight(z)(op)

      /**
        * Computes length of longest segment whose elements all satisfy some predicate.
        *
        * @param p the predicate used to test elements.
        * @param from the index where the search starts.
        * @param the length of the longest segment of this <code>NonEmptyArray</code> starting from index <code>from</code> such that every element of the
        *     segment satisfies the predicate <code>p</code>. 
        */
      def segmentLength(p: T => Boolean, from: Int): Int = nonEmptyArray.toIndexedSeq.segmentLength(p, from)

      /**
        * The size of this <code>NonEmptyArray</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of elements in this <code>NonEmptyArray</code>. 
        */
      def size: Int = new ArrayOps(nonEmptyArray).size

      /**
        * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
        *
        * @param size the number of elements per group
        * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer elements than <code>size</code>.
        */
      def sliding(size: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size).map(l => NonEmptyArray(l.head, l.tail.toList*))

      /**
        * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
        * moving the sliding window by a given <code>step</code> each time.
        *
        * @param size the number of elements per group
        * @param step the distance between the first elements of successive groups
        * @return an iterator producing <code>NonEmptyArray</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer elements than <code>size</code>.
        */
      def sliding(size: Int, step: Int)(using classTag: ClassTag[T]): Iterator[NonEmptyArray[T]] = new ArrayOps(nonEmptyArray).sliding(size, step).map(l => NonEmptyArray(l.head, l.tail.toList*))

      /**
        * Sorts this <code>NonEmptyArray</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
        *
        * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
        * @param f the transformation function mapping elements to some other domain <code>U</code>.
        * @param ord the ordering assumed on domain <code>U</code>.
        * @return a <code>NonEmptyArray</code> consisting of the elements of this <code>NonEmptyArray</code> sorted according to the <code>Ordering</code> where
        *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
        */
      def sortBy[U](f: T => U)(using ord: Ordering[U]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortBy(f)

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
      def sortWith(lt: (T, T) => Boolean): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sortWith(lt)

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
      def sorted(using ord: Ordering[T]): NonEmptyArray[T] = new ArrayOps(nonEmptyArray).sorted(ord)

      /**
        * The result of summing all the elements of this <code>NonEmptyArray</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyArray[T]</code> for which a given <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the sum of all elements
        */
      def sum[U >: T](using num: Numeric[U]): U = nonEmptyArray.toIndexedSeq.sum(num)

      /**
        * Indicates whether this <code>NonEmptyArray</code> starts with the given <code>IterableOnce</code>. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyArray</code>
        * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
        */
      def startsWith[B](that: IterableOnce[B]): Boolean = new ArrayOps(nonEmptyArray).startsWith(that)

      /**
        * Indicates whether this <code>NonEmptyArray</code> starts with the given <code>IterableOnce</code> at the given index. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyArray</code>
        * @param offset the index at which this <code>NonEmptyArray</code> is searched.
        * @return <code>true</code> if this <code>NonEmptyArray</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
        */
      def startsWith[B](that: IterableOnce[B], offset: Int): Boolean = new ArrayOps(nonEmptyArray).startsWith(that, offset)

      /**
        * Returns <code>"NonEmptyArray"</code>, the prefix of this object's <code>toString</code> representation.
        *
        * @return the string <code>"NonEmptyArray"</code>
        */
      def stringPrefix: String = "NonEmptyArray"

      def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] = 
        nonEmptyArray.toIndexedSeq.to(factory)

      /**
        * Converts this <code>NonEmptyArray</code> to a list.
        *
        * @return a list containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toArray: Array[T] = nonEmptyArray

      /**
        * Converts this <code>NonEmptyArray</code> to a list.
        *
        * @return a list containing all elements of this <code>NonEmptyArray</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyArray</code>.
        */
      def toList[U >: T]: List[U] = List(nonEmptyArray*)

      /**
        * Converts this <code>NonEmptyArray</code> to a <code>Vector</code>.
        *
        * @return a <code>Vector</code> containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toVector: Vector[T] = nonEmptyArray.toIndexedSeq.toVector

      /**
        * Converts this <code>NonEmptyArray</code> to a mutable buffer.
        *
        * @return a buffer containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toBuffer[U >: T]: Buffer[U] = nonEmptyArray.toIndexedSeq.toBuffer

      /**
        * Converts this <code>NonEmptyArray</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toIndexedSeq: collection.immutable.IndexedSeq[T] = 
        new collection.immutable.IndexedSeq[T] {
          def length: Int = nonEmptyArray.length
          def apply(idx: Int): T = nonEmptyArray(idx)
        }

      /**
        * Converts this <code>NonEmptyArray</code> to an iterable collection.
        *
        * @return an <code>Iterable</code> containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toIterable: scala.collection.Iterable[T] = nonEmptyArray.toIndexedSeq

      /**
        * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyArray</code>.
        *
        * @return an <code>Iterator</code> containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toIterator: Iterator[T] = new ArrayOps(nonEmptyArray).iterator

      /**
        * Converts this <code>NonEmptyArray</code> to a map.
        *
        * <p>
        * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
        * in the map. Duplicate keys will be overwritten by later keys.
        * </p>
        *
        * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>NonEmptyArray</code>. 
        */
      def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = nonEmptyArray.toIndexedSeq.toMap

      /**
        * Converts this <code>NonEmptyArray</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyArray</code>.
        */
      def toSeq: collection.immutable.Seq[T] = new ArrayOps(nonEmptyArray).toSeq

      /**
        * Converts this <code>NonEmptyArray</code> to a set.
        *
        * @return a set containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toSet[U >: T]: Set[U] = nonEmptyArray.toIndexedSeq.toSet

      /**
        * Converts this <code>NonEmptyArray</code> to a stream.
        *
        * @return a stream containing all elements of this <code>NonEmptyArray</code>. 
        */
      def toStream: Stream[T] = nonEmptyArray.toIndexedSeq.toStream

      /**
        * Converts this <code>NonEmptyArray</code> of pairs into two <code>NonEmptyArray</code>s of the first and second half of each pair. 
        *
        * @tparam L the type of the first half of the element pairs
        * @tparam R the type of the second half of the element pairs
        * @param asPair an given conversion that asserts that the element type of this <code>NonEmptyArray</code> is a pair.
        * @return a pair of <code>NonEmptyArray</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyArray</code>. 
        */
      def unzip[L, R](using asPair: T => (L, R), classTagL: ClassTag[L], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[R]) = {
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
        * @param asTriple an given conversion that asserts that the element type of this <code>NonEmptyArray</code> is a triple.
        * @return a triple of <code>NonEmptyArray</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptyArray</code>. 
        */
      def unzip3[L, M, R](using asTriple: T => (L, M, R), classTagL: ClassTag[L], classTagM: ClassTag[M], classTagR: ClassTag[R]): (NonEmptyArray[L], NonEmptyArray[M], NonEmptyArray[R]) = {
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
      def updated[U >: T](idx: Int, elem: U)(using classTag: ClassTag[U]): NonEmptyArray[U] =
        new ArrayOps(nonEmptyArray).updated(idx, elem)

      /**
        * Returns a <code>NonEmptyArray</code> formed from this <code>NonEmptyArray</code> and an iterable collection by combining corresponding
        * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
        * shorter collection to the length of the longer.
        *
        * @tparm O the type of the second half of the returned pairs
        * @tparm U the type of the first half of the returned pairs
        * @param other the <code>IterableOnce</code> providing the second half of each result pair
        * @param thisElem the element to be used to fill up the result if this <code>NonEmptyArray</code> is shorter than <code>that</code> <code>Iterable</code>.
        * @param thatElem the element to be used to fill up the result if <code>that</code> <code>IterableOnce</code> is shorter than this <code>NonEmptyArray</code>.
        * @return a new <code>NonEmptyArray</code> containing pairs consisting of corresponding elements of this <code>NonEmptyArray</code> and <code>that</code>. The
        *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyArray</code> and <code>that</code>. If this <code>NonEmptyArray</code>
        *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
        *     <code>NonEmptyArray</code>, <code>thatElem</code> values are used to pad the result. 
        */
      def zipAll[O, U >: T](other: IterableOnce[O], thisElem: U, otherElem: O): NonEmptyArray[(U, O)] =
        new ArrayOps(nonEmptyArray).zipAll(other.toIterable, thisElem, otherElem)

      /**
        * Zips this <code>NonEmptyArray</code>  with its indices.
        *
        * @return A new <code>NonEmptyArray</code> containing pairs consisting of all elements of this <code>NonEmptyArray</code> paired with their index. Indices start at 0.
        */
      def zipWithIndex: NonEmptyArray[(T, Int)] = new ArrayOps(nonEmptyArray).zipWithIndex  
    }
  }

  opaque type NonEmptyList[T] = List[T]

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
     *
     * A factory/assertion method that produces a <code>NonEmptyList</code>
     * given a valid <code>List</code> value, or throws
     * <code>AssertionError</code>, if given an invalid <code>List</code> value.
     *
     * Note: you should use this method only when you are convinced that it will
     * always succeed, i.e., never throw an exception. It is good practice to
     * add a comment near the invocation of this method indicating ''why'' you
     * think it will always succeed to document your reasoning. If you are not
     * sure an `ensuringValid` call will always succeed, you should use one of
     * the other factory or validation methods provided on this object instead:
     * `from'.
     *
     * @param list the <code>List</code> to check to see if it is a valid.
     * @return the <code>NonEmptyList</code> if the passed list is valid..
     * @throws AssertionError if the passed list is not valid.
     */
    def ensuringValid[T](list: List[T]): NonEmptyList[T] =
      if (list.length == 0)
        throw new AssertionError(Resources.nonEmptyListEmpty)
      else
        list

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

    /**
      * Conversion from <code>NonEmptyList</code> to <code>IterableOnce</code>.
      *
      * @param nonEmptyList the <code>NonEmptyList</code> to convert
      * @return the <code>IterableOnce</code>
      */
    given [E]: Conversion[NonEmptyList[E], IterableOnce[E]] with {
      def apply(nonEmptyList: NonEmptyList[E]): IterableOnce[E] = nonEmptyList
    }

    /**
      * Conversion from <code>NonEmptyList</code> to <code>PartialFunction</code>.
      *
      * @param nonEmptyList the <code>NonEmptyList</code> to convert
      * @return the <code>PartialFunction</code>
      */
    given [E]: Conversion[NonEmptyList[E], PartialFunction[Int, E]] with {
      def apply(nonEmptyList: NonEmptyList[E]): PartialFunction[Int, E] =
        new PartialFunction[Int, E] {
          def apply(i: Int): E = (nonEmptyList: List[E]).apply(i)
          def isDefinedAt(i: Int): Boolean = i >= 0 && i < nonEmptyList.length
        }
    }

    extension [T](element: T) {
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
      infix def ::[U >: T](nonEmptyList: NonEmptyList[U]): NonEmptyList[U] = 
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

      /**
        * <strong>The <code>/:</code> method has been deprecated and will be removed in a future version of Scalactic. Please use
        * <code>foldLeft</code> instead.</strong>
        *
        * <p>This method has been deprecated for consistency with Scala 2.13's collections API.</p>
        *
        * <p>
        * Fold left: applies a binary operator to a start value, <code>z</code>, and all elements of this <code>NonEmptyList</code>, going left to right.
        * </p>
        *
        * <p>
        * Note: <code>/:</code> is alternate syntax for the <code>foldLeft</code> method; <code>z</code> <code>/:</code> <code>non-empty list</code> is the
        * same as <code>non-empty list</code> <code>foldLeft</code> <code>z</code>.
        * </p>
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going left to right, with the start value,
        *     <code>z</code>, on the left:
        *
        * <pre>
        * op(...op(op(z, x_1), x_2), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */
      @deprecated("The /: method has been deprecated and will be removed in a future version of Scalactic. Please use foldLeft instead.", "3.1.x")
      infix def /:[B](nonEmptyList: NonEmptyList[B])(op: (T, B) => T): T = nonEmptyList.toIndexedSeq./:(element)(op)
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
      infix def :+[U >: T](element: U): NonEmptyList[U] = nonEmptyList.appended(element)  

      /**
        * <strong>The <code>:\\</code> method has been deprecated and will be removed in a future version of Scalactic. Please use
        * <code>foldRight</code> instead.</strong>
        *
        * <p>This method has been deprecated for consistency with Scala 2.13's collections API.</p>
        *
        * Fold right: applies a binary operator to all elements of this <code>NonEmptyList</code> and a start value, going right to left.
        *
        * <p>
        * Note: <code>:\</code> is alternate syntax for the <code>foldRight</code> method; <code>non-empty list</code> <code>:\</code> <code>z</code> is the same
        * as <code>non-empty list</code> <code>foldRight</code> <code>z</code>.
        * </p>
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going right to left, with the start value,
        *     <code>z</code>, on the right:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_n, z)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */
      @deprecated("The :\\ method has been deprecated and will be removed in a future version of Scalactic. Please use foldRight instead.", "3.1.x")
      infix def :\[B](z: B)(op: (T, B) => B): B = nonEmptyList.toIndexedSeq.:\(z)(op)

      /**
        * Appends all elements of this <code>NonEmptyList</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyList</code>, without any separator string.
        *
        * @param sb the string builder to which elements will be appended
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder): StringBuilder = nonEmptyList.toIndexedSeq.addString(sb)

        /**
        * Appends all elements of this <code>NonEmptyList</code> to a string builder using a separator string. The written text will consist of a concatenation of the
        * result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyList</code>, separated by the string <code>sep</code>.
        *
        * @param sb the string builder to which elements will be appended
        * @param sep the separator string
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder, sep: String): StringBuilder = nonEmptyList.toIndexedSeq.addString(sb, sep)

        /**
        * Appends all elements of this <code>NonEmptyList</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
        * the string <code>start</code>; the result of invoking <code>toString</code> on all elements of this <code>NonEmptyList</code>,
        * separated by the string <code>sep</code>; and the string <code>end</code>
        *
        * @param sb the string builder to which elements will be appended
        * @param start the starting string
        * @param sep the separator string
        * @param start the ending string
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = nonEmptyList.toIndexedSeq.addString(sb, start, sep, end)

      /**
        * Finds the first element of this <code>NonEmptyList</code> for which the given partial function is defined, if any, and applies the partial function to it.
        *
        * @param pf the partial function
        * @return an <code>Option</code> containing <code>pf</code> applied to the first element for which it is defined, or <code>None</code> if
        *    the partial function was not defined for any element.
        */
      def collectFirst[U](pf: PartialFunction[T, U]): Option[U] = nonEmptyList.toIndexedSeq.collectFirst(pf)

      /**
        * Indicates whether this <code>NonEmptyList</code> contains a given value as an element.
        *
        * @param elem the element to look for
        * @return true if this <code>NonEmptyList</code> has an element that is equal (as determined by <code>==)</code> to <code>elem</code>, false otherwise. 
        */ 
      def contains(elem: Any): Boolean = nonEmptyList.toIndexedSeq.contains(elem)

      /**
        * Indicates whether this <code>NonEmptyList</code> contains a given <code>GenSeq</code> as a slice.
        *
        * @param that the <code>GenSeq</code> slice to look for
        * @return true if this <code>NonEmptyList</code> contains a slice with the same elements as <code>that</code>, otherwise <code>false</code>.
        */
      def containsSlice[B](that: IterableOnce[B]): Boolean = nonEmptyList.toIndexedSeq.containsSlice(that.toSeq)

      /**
        * Copies values of this <code>NonEmptyList</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyList</code>. Copying
        * will stop once either the end of the current <code>NonEmptyList</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        */
      def copyToArray[U >: T](arr: Array[U]): Unit = nonEmptyList.toIndexedSeq.copyToArray(arr)

      /**
        * Copies values of this <code>NonEmptyList</code> to an array. Fills the given array <code>arr</code> with values of this <code>NonEmptyList</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyList</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        * @param start the starting index
        */
      def copyToArray[U >: T](arr: Array[U], start: Int): Unit = nonEmptyList.toIndexedSeq.copyToArray(arr, start)

      /**
        * Copies values of this <code>NonEmptyList</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> elements of this <code>NonEmptyList</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyList</code> is reached, the end of the array is reached, or
        * <code>len</code> elements have been copied.
        *
        * @param arr the array to fill
        * @param start the starting index
        * @param len the maximum number of elements to copy
        */
      def copyToArray[U >: T](arr: Array[U], start: Int, len: Int): Unit = nonEmptyList.toIndexedSeq.copyToArray(arr, start, len)

      /**
        * Copies all elements of this <code>NonEmptyList</code> to a buffer. 
        *
        * @param buf the buffer to which elements are copied
        */
      def copyToBuffer[U >: T](buf: Buffer[U]): Unit = nonEmptyList.toIndexedSeq.copyToBuffer(buf)

      /**
        * Indicates whether every element of this <code>NonEmptyList</code> relates to the corresponding element of a given <code>GenSeq</code> by satisfying a given predicate. 
        *
        * @tparam B the type of the elements of <code>that</code>
        * @param that the <code>GenSeq</code> to compare for correspondence
        * @param p the predicate, which relates elements from this <code>NonEmptyList</code> and the passed <code>GenSeq</code>
        * @return true if this <code>NonEmptyList</code> and the passed <code>GenSeq</code> have the same length and <code>p(x, y)</code> is <code>true</code>
        *     for all corresponding elements <code>x</code> of this <code>NonEmptyList</code> and <code>y</code> of that, otherwise <code>false</code>.
        */
      def corresponds[B](that: IterableOnce[B])(p: (T, B) => Boolean): Boolean = nonEmptyList.toIndexedSeq.corresponds(that)(p)

      /**
        * Counts the number of elements in this <code>NonEmptyList</code> that satisfy a predicate. 
        *
        * @param p the predicate used to test elements.
        * @return the number of elements satisfying the predicate <code>p</code>. 
        */
      def count(p: T => Boolean): Int = nonEmptyList.toIndexedSeq.count(p)

      /**
        * Builds a new <code>NonEmptyList</code> from this <code>NonEmptyList</code> without any duplicate elements.
        *
        * @return A new <code>NonEmptyList</code> that contains the first occurrence of every element of this <code>NonEmptyList</code>. 
        */
      def distinct: NonEmptyList[T] = (nonEmptyList: List[T]).distinct

      /**
        * Indicates whether this <code>NonEmptyList</code> ends with the given <code>GenSeq</code>.
        *
        * @param that the sequence to test
        * @return <code>true</code> if this <code>NonEmptyList</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
        */
      def endsWith[B](that: IterableOnce[B]): Boolean = nonEmptyList.toIndexedSeq.endsWith(that.toIterable)

      /**
        * Indicates whether a predicate holds for at least one of the elements of this <code>NonEmptyList</code>.
        *
        * @param the predicate used to test elements.
        * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyList</code>, otherwise <code>false</code>. 
        */
      def exists(p: T => Boolean): Boolean = nonEmptyList.toIndexedSeq.exists(p)

      /**
        * Finds the first element of this <code>NonEmptyList</code> that satisfies the given predicate, if any.
        *
        * @param p the predicate used to test elements
        * @return an <code>Some</code> containing the first element in this <code>NonEmptyList</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
        */
      def find(p: T => Boolean): Option[T] = nonEmptyList.toIndexedSeq.find(p)

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
      def flatten[B](using ev: T <:< NonEmptyList[B]): NonEmptyList[B] = flatMap(ev)

      /**
        * Folds the elements of this <code>NonEmptyList</code> using the specified associative binary operator.
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
      def fold[U >: T](z: U)(op: (U, U) => U): U = nonEmptyList.toIndexedSeq.fold(z)(op)

        /**
        * Applies a binary operator to a start value and all elements of this <code>NonEmptyList</code>, going left to right.
        *
        * @tparam B the result type of the binary operator.
        * @param z the start value.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going left to right, with the start value,
        *     <code>z</code>, on the left:
        *
        * <pre>
        * op(...op(op(z, x_1), x_2), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */ 
      def foldLeft[B](z: B)(op: (B, T) => B): B = nonEmptyList.toIndexedSeq.foldLeft(z)(op)

        /**
        * Applies a binary operator to all elements of this <code>NonEmptyList</code> and a start value, going right to left.
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going right to left, with the start value,
        *     <code>z</code>, on the right:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_n, z)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */
      def foldRight[B](z: B)(op: (T, B) => B): B = nonEmptyList.toIndexedSeq.foldRight(z)(op)

      /**
        * Indicates whether a predicate holds for all elements of this <code>NonEmptyList</code>.
        *
        * @param p the predicate used to test elements.
        * @return <code>true</code> if the given predicate <code>p</code> holds for all elements of this <code>NonEmptyList</code>, otherwise <code>false</code>. 
        */
      def forall(p: T => Boolean): Boolean = nonEmptyList.toIndexedSeq.forall(p)

      /**
        * Applies a function <code>f</code> to all elements of this <code>NonEmptyList</code>.
        *
        * @param f the function that is applied for its side-effect to every element. The result of function <code>f</code> is discarded.
        */ 
      def foreach(f: T => Unit): Unit = nonEmptyList.toIndexedSeq.foreach(f)

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
        * Returns <code>true</code> to indicate this <code>NonEmptyList</code> has a definite size, since all <code>NonEmptyList</code>s are strict collections.
        */
      def hasDefiniteSize: Boolean = true

        // override def hashCode: Int = toList.hashCode

      /**
        * Selects the first element of this <code>NonEmptyList</code>. 
        *
        * @return the first element of this <code>NonEmptyList</code>.
        */
      def head: T = nonEmptyList.toIndexedSeq.head

      /**
        * Selects the first element of this <code>NonEmptyList</code> and returns it wrapped in a <code>Some</code>. 
        *
        * @return the first element of this <code>NonEmptyList</code>, wrapped in a <code>Some</code>.
        */
      def headOption: Option[T] = Some(head)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyList</code>.
        *
        * @param elem the element value to search for. 
        * @return the index of the first element of this <code>NonEmptyList</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf[U >: T](elem: U): Int = nonEmptyList.toIndexedSeq.indexOf(elem, 0)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyList</code> after or at some start index.
        *
        * @param elem the element value to search for. 
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyList</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf[U >: T](elem: U, from: Int): Int = nonEmptyList.toIndexedSeq.indexOf(elem, from)


      /**
        * Finds first index where this <code>NonEmptyList</code> contains a given <code>IterableOnce</code> as a slice.
        * 
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @return the first index at which the elements of this <code>NonEmptyList</code> starting at that index match the elements of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def indexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyList.toIndexedSeq.indexOfSlice(that.toSeq)

      /**
        * Finds first index after or at a start index where this <code>NonEmptyList</code> contains a given <code>IterableOnce</code> as a slice.
        * 
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @param from the start index
        * @return the first index <code>&gt;=</code> <code>from</code> at which the elements of this <code>NonEmptyList</code> starting at that index match the elements of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def indexOfSlice[U >: T](that: IterableOnce[U], from: Int): Int = nonEmptyList.toList.indexOfSlice(that.toSeq, from)

      /**
        * Finds index of the first element satisfying some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the index of the first element of this <code>NonEmptyList</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */ 
      def indexWhere(p: T => Boolean): Int = nonEmptyList.toIndexedSeq.indexWhere(p)

      /**
        * Finds index of the first element satisfying some predicate after or at some start index.
        *
        * @param p the predicate used to test elements.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyList</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */ 
      def indexWhere(p: T => Boolean, from: Int): Int = nonEmptyList.toIndexedSeq.indexWhere(p, from)

      /**
        * Produces the range of all indices of this <code>NonEmptyList</code>. 
        *
        * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyList</code>. 
        */
      def indices: Range = nonEmptyList.toIndexedSeq.indices

      /**
        * Returns <code>false</code> to indicate this <code>NonEmptyList</code>, like all <code>NonEmptyList</code>s, is non-empty.
        *
        * @return false
        */
      def isEmpty: Boolean = false

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyList</code>, like all <code>NonEmptyList</code>s, can be traversed repeatedly.
        *
        * @return true
        */
      def isTraversableAgain: Boolean = true

      /**
        * Selects the last element of this <code>NonEmptyList</code>. 
        *
        * @return the last element of this <code>NonEmptyList</code>.
        */
      def last: T = nonEmptyList.toIndexedSeq.last

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyList</code>.
        *
        * @param elem the element value to search for.
        * @return the index of the last element of this <code>NonEmptyList</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def lastIndexOf[U >: T](elem: U): Int = nonEmptyList.toIndexedSeq.lastIndexOf(elem)

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyList</code> before or at a given <code>end</code> index.
        *
        * @param elem the element value to search for.
        * @param end the end index. 
        * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyList</code> that is equal (as determined by <code>==</code>)
        *     to <code>elem</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexOf[U >: T](elem: U, end: Int): Int = nonEmptyList.toIndexedSeq.lastIndexOf(elem, end)

      /**
        * Finds the last index where this <code>NonEmptyList</code> contains a given <code>IterableOnce</code> as a slice. 
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @return the last index at which the elements of this <code>NonEmptyList</code> starting at that index match the elements of
        *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice[U >: T](that: IterableOnce[U]): Int = nonEmptyList.toIndexedSeq.lastIndexOfSlice(that.toSeq)

      /**
        * Finds the last index before or at a given end index where this <code>NonEmptyList</code> contains a given <code>IterableOnce</code> as a slice. 
        *
        * @param that the <code>IterableOnce</code> defining the slice to look for
        * @param end the end index
        * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyList</code> starting at that index match the elements of
        *    <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice[U >: T](that: IterableOnce[U], end: Int): Int = nonEmptyList.toIndexedSeq.lastIndexOfSlice(that.toSeq, end)

      /**
        * Finds index of last element satisfying some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the index of the last element of this <code>NonEmptyList</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: T => Boolean): Int = nonEmptyList.toIndexedSeq.lastIndexWhere(p)

      /**
        * Finds index of last element satisfying some predicate before or at given end index.
        *
        * @param p the predicate used to test elements.
        * @param end the end index
        * @return the index <code>&gt;=</code> <code>end</code> of the last element of this <code>NonEmptyList</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: T => Boolean, end: Int): Int = nonEmptyList.toIndexedSeq.lastIndexWhere(p, end)

      /**
        * Returns the last element of this <code>NonEmptyList</code>, wrapped in a <code>Some</code>. 
        *
        * @return the last element, wrapped in a <code>Some</code>. 
        */
      def lastOption: Option[T] = nonEmptyList.toIndexedSeq.lastOption // Will always return a Some

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
        * Compares the length of this <code>NonEmptyList</code> to a test value. 
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
      def lengthCompare(len: Int): Int = nonEmptyList.toIndexedSeq.lengthCompare(len)

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
        * Finds the largest element.
        *
        * @return the largest element of this <code>NonEmptyList</code>. 
        */
      def max[U >: T](using cmp: Ordering[U]): T = nonEmptyList.toIndexedSeq.max(cmp)

      /**
        * Finds the largest result after applying the given function to every element.
        *
        * @return the largest result of applying the given function to every element of this <code>NonEmptyList</code>. 
        */
      def maxBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyList.toIndexedSeq.maxBy(f)(cmp)

      /**
        * Finds the smallest element.
        *
        * @return the smallest element of this <code>NonEmptyList</code>. 
        */
      def min[U >: T](using cmp: Ordering[U]): T = nonEmptyList.toIndexedSeq.min(cmp)

      /**
        * Finds the smallest result after applying the given function to every element.
        *
        * @return the smallest result of applying the given function to every element of this <code>NonEmptyList</code>. 
        */
      def minBy[U](f: T => U)(using cmp: Ordering[U]): T = nonEmptyList.toIndexedSeq.minBy(f)(cmp)  

      /**
        * Displays all elements of this <code>NonEmptyList</code> in a string. 
        *
        * @return a string representation of this <code>NonEmptyList</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyList</code> follow each other without any separator string. 
        */
      def mkString: String = nonEmptyList.toIndexedSeq.mkString

      /**
        * Displays all elements of this <code>NonEmptyList</code> in a string using a separator string. 
        *
        * @param sep the separator string
        * @return a string representation of this <code>NonEmptyList</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyList</code> are separated by the string <code>sep</code>. 
        */
      def mkString(sep: String): String = nonEmptyList.toIndexedSeq.mkString(sep)

      /**
        * Displays all elements of this <code>NonEmptyList</code> in a string using start, end, and separator strings. 
        *
        * @param start the starting string.
        * @param sep the separator string.
        * @param end the ending string.
        * @return a string representation of this <code>NonEmptyList</code>. The resulting string begins with the string <code>start</code> and ends with the string
        *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all elements of this <code>NonEmptyList</code> are
        *     separated by the string <code>sep</code>. 
        */
      def mkString(start: String, sep: String, end: String): String = nonEmptyList.toIndexedSeq.mkString(start, sep, end)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyList</code>, like all <code>NonEmptyList</code>s, is non-empty.
        *
        * @return true
        */
      def nonEmpty: Boolean = true

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
        * Iterates over distinct permutations. 
        *
        * <p>
        * Here's an example:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyList('a', 'b', 'b').permutations.toList = List(NonEmptyList(a, b, b), NonEmptyList(b, a, b), NonEmptyList(b, b, a))
        * </pre>
        *
        * @return an iterator that traverses the distinct permutations of this <code>NonEmptyList</code>.
        */
      def permutations: Iterator[NonEmptyList[T]] = {
        val it = nonEmptyList.toIndexedSeq.permutations
        it map { list => NonEmptyList(list.head, list.tail*) }
      }

      /**
        * Returns the length of the longest prefix whose elements all satisfy some predicate.
        *
        * @param p the predicate used to test elements.
        * @return the length of the longest prefix of this <code>NonEmptyList</code> such that every element
        *     of the segment satisfies the predicate <code>p</code>. 
        */
      def prefixLength(p: T => Boolean): Int = nonEmptyList.toIndexedSeq.prefixLength(p)

      /**
        * The result of multiplying all the elements of this <code>NonEmptyList</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyList[T]</code> for which an given <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the product of all elements
        */
      def product[U >: T](using num: Numeric[U]): U = nonEmptyList.toIndexedSeq.product(num)  

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
        * Reduces the elements of this <code>NonEmptyList</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on elements is unspecified and may be nondeterministic. 
        * </p>
        *
        * @tparam U a type parameter for the binary operator, a supertype of T.
        * @param op a binary operator that must be associative.
        * @return the result of applying reduce operator <code>op</code> between all the elements of this <code>NonEmptyList</code>.
        */
      def reduce[U >: T](op: (U, U) => U): U = nonEmptyList.toIndexedSeq.reduce(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyList</code>, going left to right.
        *
        * @tparam U the result type of the binary operator.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going left to right:
        *
        * <pre>
        * op(...op(op(x_1, x_2), x_3), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */
      def reduceLeft[U >: T](op: (U, T) => U): U = nonEmptyList.toIndexedSeq.reduceLeft(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyList</code>, going left to right, returning the result in a <code>Some</code>.
        *
        * @tparam U the result type of the binary operator.
        * @param op the binary operator.
        * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
        * </p>
        */
      def reduceLeftOption[U >: T](op: (U, T) => U): Option[U] = nonEmptyList.toIndexedSeq.reduceLeftOption(op)

      def reduceOption[U >: T](op: (U, U) => U): Option[U] = nonEmptyList.toIndexedSeq.reduceOption(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyList</code>, going right to left.
        *
        * @tparam U the result of the binary operator
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive elements of this <code>NonEmptyList</code>, going right to left:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyList</code>. 
        * </p>
        */
      def reduceRight[U >: T](op: (T, U) => U): U = nonEmptyList.toIndexedSeq.reduceRight(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyList</code>, going right to left, returning the result in a <code>Some</code>.
        *
        * @tparam U the result of the binary operator
        * @param op the binary operator
        * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
        */
      def reduceRightOption[U >: T](op: (T, U) => U): Option[U] = nonEmptyList.toIndexedSeq.reduceRightOption(op)  

      /**
        * An iterator yielding elements in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyList.reverseIterator</code> is the same as <code>nonEmptyList.reverse.iterator</code>, but might be more efficient. 
        * </p>
        *
        * @return an iterator yielding the elements of this <code>NonEmptyList</code> in reversed order 
        */
      def reverseIterator: Iterator[T] = nonEmptyList.toIndexedSeq.reverseIterator

      /**
        * Checks if the given <code>GenIterable</code> contains the same elements in the same order as this <code>NonEmptyList</code>.
        *
        * @param that the <code>GenIterable</code> with which to compare
        * @return <code>true</code>, if both this <code>NonEmptyList</code> and the given <code>GenIterable</code> contain the same elements
        *     in the same order, <code>false</code> otherwise. 
        */
      def sameElements[U >: T](that: IterableOnce[U]): Boolean = nonEmptyList.toIndexedSeq.sameElements(that)

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
        * Computes length of longest segment whose elements all satisfy some predicate.
        *
        * @param p the predicate used to test elements.
        * @param from the index where the search starts.
        * @param the length of the longest segment of this <code>NonEmptyList</code> starting from index <code>from</code> such that every element of the
        *     segment satisfies the predicate <code>p</code>. 
        */
      def segmentLength(p: T => Boolean, from: Int): Int = nonEmptyList.toIndexedSeq.segmentLength(p, from)

      /**
        * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
        *
        * @param size the number of elements per group
        * @return an iterator producing <code>NonEmptyList</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer elements than <code>size</code>.
        */
      def sliding(size: Int): Iterator[NonEmptyList[T]] = nonEmptyList.toIndexedSeq.sliding(size).map(list => NonEmptyList(list.head, list.tail*))

      /**
        * Groups elements in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
        * moving the sliding window by a given <code>step</code> each time.
        *
        * @param size the number of elements per group
        * @param step the distance between the first elements of successive groups
        * @return an iterator producing <code>NonEmptyList</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer elements than <code>size</code>.
        */
      def sliding(size: Int, step: Int): Iterator[NonEmptyList[T]] = nonEmptyList.toIndexedSeq.sliding(size, step).map(list => NonEmptyList(list.head, list.tail*))

      /**
        * The size of this <code>NonEmptyList</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of elements in this <code>NonEmptyList</code>. 
        */
      def size: Int = nonEmptyList.length

      /**
        * Sorts this <code>NonEmptyList</code> according to the <code>Ordering</code> of the result of applying the given function to every element.
        *
        * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
        * @param f the transformation function mapping elements to some other domain <code>U</code>.
        * @param ord the ordering assumed on domain <code>U</code>.
        * @return a <code>NonEmptyList</code> consisting of the elements of this <code>NonEmptyList</code> sorted according to the <code>Ordering</code> where
        *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
        */
      def sortBy[U](f: T => U)(using ord: Ordering[U]): NonEmptyList[T] = (nonEmptyList: List[T]).sortBy(f)

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
      def sortWith(lt: (T, T) => Boolean): NonEmptyList[T] = (nonEmptyList: List[T]).sortWith(lt)

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
      def sorted[U >: T](using ord: Ordering[U]): NonEmptyList[U] = (nonEmptyList: List[T]).sorted(ord)

      /**
        * Indicates whether this <code>NonEmptyList</code> starts with the given <code>IterableOnce</code>. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyList</code>
        * @return <code>true</code> if this <code>NonEmptyList</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
        */
      def startsWith[B](that: IterableOnce[B]): Boolean = nonEmptyList.toIndexedSeq.startsWith(that)

      /**
        * Indicates whether this <code>NonEmptyList</code> starts with the given <code>IterableOnce</code> at the given index. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyList</code>
        * @param offset the index at which this <code>NonEmptyList</code> is searched.
        * @return <code>true</code> if this <code>NonEmptyList</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
        */
      def startsWith[B](that: IterableOnce[B], offset: Int): Boolean = nonEmptyList.toIndexedSeq.startsWith(that, offset)

      /**
        * The result of summing all the elements of this <code>NonEmptyList</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyList[T]</code> for which a given <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the sum of all elements
        */
      def sum[U >: T](using num: Numeric[U]): U = nonEmptyList.toIndexedSeq.sum(num)
      
      /**
        * Returns <code>"NonEmptyList"</code>, the prefix of this object's <code>toString</code> representation.
        *
        * @return the string <code>"NonEmptyList"</code>
        */
      def stringPrefix: String = "NonEmptyList"

      def transpose[U](using ev: T <:< NonEmptyList[U]): NonEmptyList[NonEmptyList[U]] = 
        (nonEmptyList: List[T]).transpose

      /**
        * Converts this <code>NonEmptyList</code> into a collection of type <code>Col</code> by copying all elements.
        *
        * @tparam Col the collection type to build.
        * @return a new collection containing all elements of this <code>NonEmptyList</code>. 
        */
      def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[T, Col[T @ uV]]): Col[T @ uV] = 
        nonEmptyList.toIndexedSeq.to(factory)

      /**
        * Converts this <code>NonEmptyList</code> to an array.
        *
        * @return an array containing all elements of this <code>NonEmptyList</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyList</code>. 
        */ 
      def toArray[U >: T](using classTag: ClassTag[U]): Array[U] = nonEmptyList.toIndexedSeq.toArray

      /**
        * Converts this <code>NonEmptyList</code> to a mutable buffer.
        *
        * @return a buffer containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toBuffer[U >: T]: Buffer[U] = nonEmptyList.toIndexedSeq.toBuffer

      /**
        * Converts this <code>NonEmptyList</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toIndexedSeq: collection.immutable.IndexedSeq[T] = 
        new collection.immutable.IndexedSeq[T] {
          def length: Int = nonEmptyList.length
          def apply(idx: Int): T = nonEmptyList(idx)
        }  

      /**
        * Converts this <code>NonEmptyList</code> to an iterable collection.
        *
        * @return an <code>Iterable</code> containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toIterable: scala.collection.Iterable[T] = nonEmptyList

      /**
        * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyList</code>.
        *
        * @return an <code>Iterator</code> containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toIterator: Iterator[T] = nonEmptyList.toIndexedSeq.toIterator

      /**
        * Converts this <code>NonEmptyList</code> to a list.
        *
        * @return a list containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toList: List[T] = nonEmptyList

      /**
        * Converts this <code>NonEmptyList</code> to a map.
        *
        * <p>
        * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
        * in the map. Duplicate keys will be overwritten by later keys.
        * </p>
        *
        * @return a map of type <code>immutable.Map[K, V]</code> containing all key/value pairs of type <code>(K, V)</code> of this <code>NonEmptyList</code>. 
        */ 
      def toMap[K, V](using ev: T <:< (K, V)): Map[K, V] = nonEmptyList.toIndexedSeq.toMap

      /**
        * Converts this <code>NonEmptyList</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all elements of this <code>NonEmptyList</code>.
        */ 
      def toSeq: collection.immutable.Seq[T] = nonEmptyList

      /**
        * Converts this <code>NonEmptyList</code> to a set.
        *
        * @return a set containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toSet[U >: T]: Set[U] = nonEmptyList.toIndexedSeq.toSet

      /**
        * Converts this <code>NonEmptyList</code> to a stream.
        *
        * @return a stream containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toStream: Stream[T] = nonEmptyList.toIndexedSeq.toStream

      /**
        * Converts this <code>NonEmptyList</code> to a <code>Vector</code>.
        *
        * @return a <code>Vector</code> containing all elements of this <code>NonEmptyList</code>. 
        */ 
      def toVector: Vector[T] = nonEmptyList.toIndexedSeq.toVector

      /**
        * Produces a new <code>NonEmptyList</code> that contains all elements of this <code>NonEmptyList</code> and also all elements of a given <code>IterableOnce</code>.
        *
        * <p>
        * <code>nonEmptyListX</code> <code>union</code> <code>ys</code> is equivalent to <code>nonEmptyListX</code> <code>++</code> <code>ys</code>.
        * </p>
        *
        * <p>
        * Another way to express this is that <code>nonEmptyListX</code> <code>union</code> <code>ys</code> computes the order-preserving multi-set union
        * of <code>nonEmptyListX</code> and <code>ys</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
        * also work on multi-sets.
        * </p>
        *
        * @param that the <code>IterableOnce</code> to add.
        * @return a new <code>NonEmptyList</code> that contains all elements of this <code>NonEmptyList</code> followed by all elements of <code>that</code> <code>IterableOnce</code>.
        */
      final def union[U >: T](that: IterableOnce[U]): NonEmptyList[U] = { 
        val list = nonEmptyList.toIndexedSeq.union(that.toSeq)
        NonEmptyList(list.head, list.tail*)
      }

      /**
        * Converts this <code>NonEmptyList</code> of pairs into two <code>NonEmptyList</code>s of the first and second half of each pair. 
        *
        * @tparam L the type of the first half of the element pairs
        * @tparam R the type of the second half of the element pairs
        * @param asPair an given conversion that asserts that the element type of this <code>NonEmptyList</code> is a pair.
        * @return a pair of <code>NonEmptyList</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyList</code>. 
        */
      final def unzip[L, R](using asPair: T => (L, R)): (NonEmptyList[L], NonEmptyList[R]) = {
        val unzipped = (nonEmptyList: List[T]).unzip
        (unzipped._1, unzipped._2)
      }

      /**
        * Converts this <code>NonEmptyList</code> of triples into three <code>NonEmptyList</code>s of the first, second, and and third element of each triple. 
        *
        * @tparam L the type of the first member of the element triples
        * @tparam R the type of the second member of the element triples
        * @tparam R the type of the third member of the element triples
        * @param asTriple an given conversion that asserts that the element type of this <code>NonEmptyList</code> is a triple.
        * @return a triple of <code>NonEmptyList</code>s, containing the first, second, and third member, respectively, of each element triple of this <code>NonEmptyList</code>. 
        */
      final def unzip3[L, M, R](using asTriple: T => (L, M, R)): (NonEmptyList[L], NonEmptyList[M], NonEmptyList[R]) = {
        val unzipped = (nonEmptyList: List[T]).unzip3
        (unzipped._1, unzipped._2, unzipped._3)
      }

      /**
        * A copy of this <code>NonEmptyList</code> with one single replaced element.
        *
        * @param idx the position of the replacement
        * @param elem the replacing element
        * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyList</code>
        * @return a copy of this <code>NonEmptyList</code> with the element at position <code>idx</code> replaced by <code>elem</code>. 
        */
      final def updated[U >: T](idx: Int, elem: U): NonEmptyList[U] =
        (nonEmptyList: List[T]).updated(idx, elem)

      /**
        * Returns a <code>NonEmptyList</code> formed from this <code>NonEmptyList</code> and an iterable collection by combining corresponding
        * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
        * shorter collection to the length of the longer.
        *
        * @tparm O the type of the second half of the returned pairs
        * @tparm U the type of the first half of the returned pairs
        * @param other the <code>IterableOnce</code> providing the second half of each result pair
        * @param thisElem the element to be used to fill up the result if this <code>NonEmptyList</code> is shorter than <code>that</code> <code>IterableOnce</code>.
        * @param thatElem the element to be used to fill up the result if <code>that</code> <code>IterableOnce</code> is shorter than this <code>NonEmptyList</code>.
        * @return a new <code>NonEmptyList</code> containing pairs consisting of corresponding elements of this <code>NonEmptyList</code> and <code>that</code>. The
        *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyList</code> and <code>that</code>. If this <code>NonEmptyList</code>
        *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
        *     <code>NonEmptyList</code>, <code>thatElem</code> values are used to pad the result. 
        */
      final def zipAll[O, U >: T](other: collection.IterableOnce[O], thisElem: U, otherElem: O): NonEmptyList[(U, O)] = {
        val zipAllResult = nonEmptyList.toIndexedSeq.zipAll(other.toIterable, thisElem, otherElem)
        NonEmptyList(zipAllResult.head, zipAllResult.tail*)
      }

      /**
        * Zips this <code>NonEmptyList</code>  with its indices.
        *
        * @return A new <code>NonEmptyList</code> containing pairs consisting of all elements of this <code>NonEmptyList</code> paired with their index. Indices start at 0.
        */
      final def zipWithIndex: NonEmptyList[(T, Int)] = (nonEmptyList: List[T]).zipWithIndex  
    }

  }
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
     *
     * A factory/assertion method that produces a <code>NonEmptyVector</code>
     * given a valid <code>Vector</code> value, or throws
     * <code>AssertionError</code>, if given an invalid <code>Vector</code> value.
     *
     * Note: you should use this method only when you are convinced that it will
     * always succeed, i.e., never throw an exception. It is good practice to
     * add a comment near the invocation of this method indicating ''why'' you
     * think it will always succeed to document your reasoning. If you are not
     * sure an `ensuringValid` call will always succeed, you should use one of
     * the other factory or validation methods provided on this object instead:
     * `from'.
     *
     * @param vector the <code>Vector</code> to check to see if it is a valid.
     * @return the <code>NonEmptyVector</code> if the passed vector is valid..
     * @throws AssertionError if the passed vector is not valid.
     */
    def ensuringValid[T](vector: Vector[T]): NonEmptyVector[T] =
      if (vector.length == 0)
        throw new AssertionError(Resources.nonEmptyVectorEmpty)
      else
        vector

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
     * @throws AssertionError if the passed set is not valid.
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
     *
     * A factory/assertion method that produces a <code>NonEmptyMap</code>
     * given a valid <code>Map</code> value, or throws
     * <code>AssertionError</code>, if given an invalid <code>Map</code> value.
     *
     * Note: you should use this method only when you are convinced that it will
     * always succeed, i.e., never throw an exception. It is good practice to
     * add a comment near the invocation of this method indicating ''why'' you
     * think it will always succeed to document your reasoning. If you are not
     * sure an `ensuringValid` call will always succeed, you should use one of
     * the other factory or validation methods provided on this object instead:
     * `from'.
     *
     * @param map the <code>Map</code> to check to see if it is a valid.
     * @return the <code>NonEmptyMap</code> if the passed map is valid..
     * @throws AssertionError if the passed map is not valid.
     */
    def ensuringValid[K, V](map: Map[K, V]): NonEmptyMap[K, V] =
      if (map.size == 0)
        throw new AssertionError(Resources.nonEmptyMapEmpty)
      else
        map

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

    /**
      * Given conversion from <code>NonEmptyMap</code> to <code>Map</code>.
      *
      * @param nonEmptyMap the <code>Map</code> to convert
      * @return the <code>Map</code>
      */
    given [K, V]: Conversion[NonEmptyMap[K, V], IterableOnce[(K, V)]] with {
      def apply(nonEmptyMap: NonEmptyMap[K, V]): IterableOnce[(K, V)] = nonEmptyMap
    }

    /**
      * Conversion from <code>NonEmptyMap</code> to <code>PartialFunction</code>.
      *
      * @param nonEmptyList the <code>NonEmptyMap</code> to convert
      * @return the <code>PartialFunction</code>
      */
    given [K, V]: Conversion[NonEmptyMap[K, V], PartialFunction[K, V]] with {
      def apply(nonEmptyMap: NonEmptyMap[K, V]): PartialFunction[K, V] = nonEmptyMap
    }

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
        * Returns a new <code>NonEmptyMap</code> containing the entries of this <code>NonEmptyMap</code> and the entries of the passed <code>IterableOnce</code>.
        * The entry type of the resulting <code>NonEmptyMap</code> is the most specific superclass encompassing the entry types of this <code>NonEmptyMap</code>
        * and the passed <code>IterableOnce</code>.
        *
        * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
        * @param other the <code>IterableOnce</code> to append
        * @return a new <code>NonEmptyMap</code> that contains all the elements of this <code>NonEmptyMap</code> followed by all elements of <code>other</code>.
        */
      def ++[V1 >: V](other: IterableOnce[(K, V1)]): NonEmptyMap[K, V1] =
        if (other.isEmpty) nonEmptyMap else toMap ++ other.toMap

      /**
        * Appends all entries of this <code>NonEmptyMap</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
        * on of every entry of this <code>NonEmptyMap</code>, without any separator string.
        *
        * @param sb the string builder to which entries will be appended
        * @return the string builder, <code>sb</code>, to which entries were appended.
        */
      def addString(sb: StringBuilder): StringBuilder = toMap.addString(sb)

      /**
        * Appends all entries of this <code>NonEmptyMap</code> to a string builder using a separator string. The written text will consist of a concatenation of the
        * result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyMap</code>, separated by the string <code>sep</code>.
        *
        * @param sb the string builder to which entries will be appended
        * @param sep the separator string
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder, sep: String): StringBuilder = toMap.addString(sb, sep)

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
      def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = toMap.addString(sb, start, sep, end)

      /**
        * Finds the first entry of this <code>NonEmptyMap</code> for which the given partial function is defined, if any, and applies the partial function to it.
        *
        * @param pf the partial function
        * @return an <code>Option</code> containing <code>pf</code> applied to the first entry for which it is defined, or <code>None</code> if
        *    the partial function was not defined for any entry.
        */
      def collectFirst[U](pf: PartialFunction[(K, V), U]): Option[U] = toMap.collectFirst(pf)

      /**
        * Indicates whether this <code>NonEmptyMap</code> contains a binding for given key.
        *
        * @param key the key to look for
        * @return true if this <code>NonEmptyMap</code> has a binding that is equal (as determined by <code>==)</code> to <code>key</code>, false otherwise.
        */
      def contains(key: K): Boolean = toMap.contains(key)

      /**
        * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with entries of this <code>NonEmptyMap</code>. Copying
        * will stop once either the end of the current <code>NonEmptyMap</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        */
      def copyToArray[V1 >: V](arr: Array[(K, V1)]): Unit = toMap.copyToArray(arr)

      /**
        * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with entries of this <code>NonEmptyMap</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyMap</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        * @param start the starting index
        */
      def copyToArray[V1 >: V](arr: Array[(K, V1)], start: Int): Unit = toMap.copyToArray(arr, start)

      /**
        * Copies entries of this <code>NonEmptyMap</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> entries of this <code>NonEmptyMap</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyMap</code> is reached, the end of the array is reached, or
        * <code>len</code> elements have been copied.
        *
        * @param arr the array to fill
        * @param start the starting index
        * @param len the maximum number of elements to copy
        */
      def copyToArray[V1 >: V](arr: Array[(K, V1)], start: Int, len: Int): Unit = toMap.copyToArray(arr, start, len)

      /**
        * Copies all elements of this <code>NonEmptyMap</code> to a buffer. 
        *
        * @param buf the buffer to which elements are copied
        */
      def copyToBuffer[V1 >: V](buf: Buffer[(K, V1)]): Unit = toMap.copyToBuffer(buf)

      /**
        * Counts the number of elements in this <code>NonEmptyMap</code> that satisfy a predicate. 
        *
        * @param p the predicate used to test elements.
        * @return the number of elements satisfying the predicate <code>p</code>. 
        */
      def count(p: ((K, V)) => Boolean): Int = toMap.count(p)

      /**
        * Indicates whether a predicate holds for at least one of the entries of this <code>NonEmptyMap</code>.
        *
        * @param p the predicate used to test entries.
        * @return <code>true</code> if the given predicate <code>p</code> holds for some of the entries of this <code>NonEmptyMap</code>, otherwise <code>false</code>.
        */
      def exists(p: ((K, V)) => Boolean): Boolean = toMap.exists(p)

      /**
        * Finds the first entry of this <code>NonEmptyMap</code> that satisfies the given predicate, if any.
        *
        * @param p the predicate used to test elements
        * @return an <code>Some</code> containing the first element in this <code>NonEmptyMap</code> that satisfies <code>p</code>, or <code>None</code> if none exists. 
        */
      def find(p: ((K, V)) => Boolean): Option[(K, V)] = toMap.find(p)

      /**
        * Builds a new <code>NonEmptyMap</code> by applying a function to all entries of this <code>NonEmptyMap</code> and using the entries of the resulting <code>NonEmptyMap</code>s.
        *
        * @tparam K1 the key type of the returned <code>NonEmptyMap</code>
        * @tparam V1 the value type of the returned <code>NonEmptyMap</code>
        * @param f the function to apply to each entry.
        * @return a new <code>NonEmptyMap</code> containing entries obtained by applying the given function <code>f</code> to each entry of this <code>NonEmptyMap</code> and concatenating
        *    the entries of resulting <code>NonEmptyMap</code>s.
        */
      def flatMap[K1, V1](f: ((K, V)) => IterableOnce[(K1, V1)]): NonEmptyMap[K1, V1] = {
        val buf = new ArrayBuffer[(K1, V1)]
        for (ele <- nonEmptyMap)
          buf ++= f(ele).toMap
        buf.toMap
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
      def fold[U >: (K, V)](z: U)(op: (U, U) => U): U = toMap.fold(z)(op)

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
      def foldLeft[B](z: B)(op: (B, (K, V)) => B): B = toMap.foldLeft(z)(op)

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
      def foldRight[B](z: B)(op: ((K, V), B) => B): B = toMap.foldRight(z)(op)

      /**
        * Indicates whether a predicate holds for all entries of this <code>NonEmptyMap</code>.
        *
        * @param p the predicate used to test entries.
        * @return <code>true</code> if the given predicate <code>p</code> holds for all entries of this <code>NonEmptyMap</code>, otherwise <code>false</code>.
        */
      def forall(p: ((K, V)) => Boolean): Boolean = toMap.forall(p)

      /**
        * Applies a function <code>f</code> to all entries of this <code>NonEmptyMap</code>.
        *
        * @param f the function that is applied for its side-effect to every entry. The result of function <code>f</code> is discarded.
        */
      def foreach(f: ((K, V)) => Unit): Unit = toMap.foreach(f)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyMap</code> has a definite size, since all <code>NonEmptyMap</code>s are strict collections.
        */
      def hasDefiniteSize: Boolean = true

      // override def hashCode: Int = toMap.hashCode

      /**
        * Selects the first element of this <code>NonEmptyMap</code>. 
        *
        * @return the first element of this <code>NonEmptyMap</code>.
        */
      def head: (K, V) = toMap.head

      /**
        * Selects the first element of this <code>NonEmptyMap</code> and returns it wrapped in a <code>Some</code>. 
        *
        * @return the first element of this <code>NonEmptyMap</code>, wrapped in a <code>Some</code>.
        */
      def headOption: Option[(K, V)] = toMap.headOption

      /**
        * Returns <code>false</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, is non-empty.
        *
        * @return false
        */
      def isEmpty: Boolean = false

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, can be traversed repeatedly.
        *
        * @return true
        */
      def isTraversableAgain: Boolean = true

      /**
        * Selects the last entry of this <code>NonEmptyMap</code>.
        *
        * @return the last entry of this <code>NonEmptyMap</code>.
        */
      def last: (K, V) = toMap.last

      /**
        * Returns the last element of this <code>NonEmptyMap</code>, wrapped in a <code>Some</code>. 
        *
        * @return the last element, wrapped in a <code>Some</code>. 
        */
      def lastOption: Option[(K, V)] = toMap.lastOption // Will always return a Some

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
        * Finds the largest entry.
        *
        * @return the largest entry of this <code>NonEmptyMap</code>.
        */
      def max[U >: (K, V)](implicit cmp: Ordering[U]): (K, V) = toMap.max(cmp)

      /**
        * Finds the largest result after applying the given function to every entry.
        *
        * @return the largest result of applying the given function to every entry of this <code>NonEmptyMap</code>.
        */
      def maxBy[U](f: ((K, V)) => U)(implicit cmp: Ordering[U]): (K, V) = toMap.maxBy(f)(cmp)

      /**
        * Finds the smallest entry.
        *
        * @return the smallest entry of this <code>NonEmptyMap</code>.
        */
      def min[U >: (K, V)](implicit cmp: Ordering[U]): (K, V) = toMap.min(cmp)

      /**
        * Finds the smallest result after applying the given function to every entry.
        *
        * @return the smallest result of applying the given function to every entry of this <code>NonEmptyMap</code>.
        */
      def minBy[U](f: ((K, V)) => U)(implicit cmp: Ordering[U]): (K, V) = toMap.minBy(f)(cmp)  

      /**
        * Displays all entries of this <code>NonEmptyMap</code> in a string.
        *
        * @return a string representation of this <code>NonEmptyMap</code>. In the resulting string, the result of invoking <code>toString</code> on all entries of this
        *     <code>NonEmptyMap</code> follow each other without any separator string. 
        */
      def mkString: String = toMap.mkString

      /**
        * Displays all entries of this <code>NonEmptyMap</code> in a string using a separator string.
        *
        * @param sep the separator string
        * @return a string representation of this <code>NonEmptyMap</code>. In the resulting string, the result of invoking <code>toString</code> on all entries of this
        *     <code>NonEmptyMap</code> are separated by the string <code>sep</code>. 
        */
      def mkString(sep: String): String = toMap.mkString(sep)

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
      def mkString(start: String, sep: String, end: String): String = toMap.mkString(start, sep, end)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyMap</code>, like all <code>NonEmptyMap</code>s, is non-empty.
        *
        * @return true
        */
      def nonEmpty: Boolean = true

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
      def reduce[U >: (K, V)](op: (U, U) => U): U = toMap.reduce(op)

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
      def reduceLeft[U >: (K, V)](op: (U, (K, V)) => U): U = toMap.reduceLeft(op)

      /**
        * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going left to right, returning the result in a <code>Some</code>.
        *
        * @tparam U the result type of the binary operator.
        * @param op the binary operator.
        * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
        * </p>
        */
      def reduceLeftOption[U >: (K, V)](op: (U, (K, V)) => U): Option[U] = toMap.reduceLeftOption(op)

      def reduceOption[U >: (K, V)](op: (U, U) => U): Option[U] = toMap.reduceOption(op)

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
      def reduceRight[U >: (K, V)](op: ((K, V), U) => U): U = toMap.reduceRight(op)

      /**
        * Applies a binary operator to all entries of this <code>NonEmptyMap</code>, going right to left, returning the result in a <code>Some</code>.
        *
        * @tparam U the result of the binary operator
        * @param op the binary operator
        * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
        */
      def reduceRightOption[U >: (K, V)](op: ((K, V), U) => U): Option[U] = toMap.reduceRightOption(op)  

      /**
        * Checks if the given <code>Iterable</code> contains the same entries in the same order as this <code>NonEmptyMap</code>.
        *
        * @param that the <code>Iterable</code> with which to compare
        * @return <code>true</code>, if both this <code>NonEmptyMap</code> and the given <code>Iterable</code> contain the same entries
        *     in the same order, <code>false</code> otherwise. 
        */
      def sameElements[U >: (K, V)](that: IterableOnce[U]): Boolean = {
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
        * Produces a collection containing cumulative results of applying the operator going left to right.
        *  The last entry of the NonEmptyMap is the last cumulative result.
        *
        *  @tparam B      the type of the elements in the resulting collection
        *  @param z       the initial value
        *  @param op      the binary operator applied to the intermediate result and the element
        *  @return        collection with intermediate results
        */
      def scanLeft[B](z: B)(op: (B, (K, V)) => B): Iterable[B] = toMap.scanLeft(z)(op)

      /** 
        * Produces a collection containing cumulative results of applying the operator going right to left.
        *  The head entry of the NonEmptyMap is the last cumulative result.
        *
        *  @tparam B      the type of the elements in the resulting collection
        *  @param z       the initial value
        *  @param op      the binary operator applied to the intermediate result and the element
        *  @return        collection with intermediate results
        */
      def scanRight[B](z: B)(op: ((K, V), B) => B): Iterable[B] = toMap.scanRight(z)(op)

      /**
        * Groups entries in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
        *
        * @param size the number of entries per group
        * @return an iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer entries than <code>size</code>.
        */
      def sliding(size: Int): Iterator[NonEmptyMap[K, V]] = toMap.sliding(size)

      /**
        * Groups entries in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
        * moving the sliding window by a given <code>step</code> each time.
        *
        * @param size the number of entries per group
        * @param step the distance between the first entries of successive groups
        * @return an iterator producing <code>NonEmptyMap</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer elements than <code>size</code>.
        */
      def sliding(size: Int, step: Int): Iterator[NonEmptyMap[K, V]] = toMap.sliding(size, step)

      /**
        * The size of this <code>NonEmptyMap</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of elements in this <code>NonEmptyMap</code>. 
        */
      def size: Int = (nonEmptyMap: Map[K, V]).size

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
      def sum[U >: (K, V)](implicit num: Numeric[U]): U = toMap.sum(num)

      /**
        * Converts this <code>NonEmptyMap</code> into a collection of type <code>Col</code> by copying all entries.
        *
        * @tparam Col the collection type to build.
        * @return a new collection containing all entries of this <code>NonEmptyMap</code>.
        */
      def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[(K, V), Col[(K, V) @ uV]]): Col[(K, V) @ uV] =
        toMap.to(factory)

      /**
        * Converts this <code>NonEmptyMap</code> to an array.
        *
        * @return an array containing all entries of this <code>NonEmptyMap</code>. A <code>ClassTag</code> must be available for the entry type of this <code>NonEmptyMap</code>.
        */
      def toArray[U >: (K, V)](implicit classTag: ClassTag[U]): Array[U] = toMap.toArray

      /**
        * Converts this <code>NonEmptyMap</code> to a mutable buffer.
        *
        * @return a buffer containing all entries of this <code>NonEmptyMap</code>.
        */
      def toBuffer[U >: (K, V)]: Buffer[U] = toMap.toBuffer

      /**
        * Converts this <code>NonEmptyMap</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toIndexedSeq: collection.immutable.IndexedSeq[(K, V)] = toMap.toVector

      /**
        * Converts this <code>NonEmptyMap</code> to an iterable collection.
        *
        * @return an <code>Iterable</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toIterable: scala.collection.Iterable[(K, V)] = toMap.toIterable

      /**
        * Returns an <code>Iterator</code> over the entries in this <code>NonEmptyMap</code>.
        *
        * @return an <code>Iterator</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toIterator: Iterator[(K, V)] = toMap.toIterator

      /**
        * Converts this <code>NonEmptyMap</code> to a <code>List</code>.
        *
        * @return a list containing all entries of this <code>NonEmptyMap</code>.
        */
      def toList[U >: (K, V)]: List[U] = toMap.toList

      /**
        * Converts this <code>NonEmptyMap</code> to a <code>Map</code>.
        *
        * @return a <code>Map</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toMap: Map[K, V] = nonEmptyMap

      /**
        * Converts this <code>NonEmptyMap</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toSeq: collection.immutable.Seq[(K, V)] = collection.immutable.Seq.empty[(K, V)] ++ toMap.toSeq

      /**
        * Converts this <code>NonEmptyMap</code> to a set.
        *
        * @return a set containing all entries of this <code>NonEmptyMap</code>.
        */
      def toSet[U >: (K, V)]: Set[U] = toMap.toSet

      /**
        * Converts this <code>NonEmptyMap</code> to a stream.
        *
        * @return a stream containing all entries of this <code>NonEmptyMap</code>.
        */
      def toStream: Stream[(K, V)] = toMap.toStream

      /**
        * Converts this <code>NonEmptyMap</code> to a <code>Vector</code>.
        *
        * @return a <code>Vector</code> containing all entries of this <code>NonEmptyMap</code>.
        */
      def toVector: Vector[(K, V)] = toMap.toVector

      /**
        * Converts this <code>NonEmptyMap</code> of pairs into two <code>Iterable</code>s of the first and second half of each pair.
        *
        * @tparam L the type of the first half of the element pairs
        * @tparam R the type of the second half of the element pairs
        * @param asPair a given conversion that asserts that the element type of this <code>NonEmptyMap</code> is a pair.
        * @return a pair of <code>NonEmptyMap</code>s, containing the first and second half, respectively, of each element pair of this <code>NonEmptyMap</code>. 
        */
      def unzip[L, R](using asPair: ((K, V)) => (L, R)): (Iterable[L], Iterable[R]) = toMap.unzip

      /**
        * Converts this <code>NonEmptyMap</code> of triples into three <code>NonEmptyMap</code>s of the first, second, and and third entry of each triple.
        *
        * @tparam L the type of the first member of the entry triples
        * @tparam R the type of the second member of the entry triples
        * @tparam R the type of the third member of the entry triples
        * @param asTriple a given conversion that asserts that the entry type of this <code>NonEmptyMap</code> is a triple.
        * @return a triple of <code>NonEmptyMap</code>s, containing the first, second, and third member, respectively, of each entry triple of this <code>NonEmptyMap</code>.
        */
      def unzip3[L, M, R](using asTriple: ((K, V)) => (L, M, R)): (Iterable[L], Iterable[M], Iterable[R]) = toMap.unzip3

      /**
        * A copy of this <code>NonEmptyMap</code> with one single replaced entry.
        *
        * @param key the key of the replacement
        * @param value the replacing value
        * @return a copy of this <code>NonEmptyMap</code> with the value at <code>key</code> replaced by the given <code>value</code>.
        */
      def updated[V1 >: V](key: K, value: V1): NonEmptyMap[K, V1] =
        (nonEmptyMap: Map[K, V1]).updated(key, value)

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
      def zipAll[O, V1 >: V](other: IterableOnce[O], thisElem: (K, V1), otherElem: O): NonEmptyMap[(K, V1), O] =
        toMap.zipAll(other.toIterable, thisElem, otherElem).toMap  

      /**
        * Zips this <code>NonEmptyMap</code>  with its indices.
        *
        * @return A new <code>NonEmptyMap</code> containing pairs consisting of all elements of this <code>NonEmptyMap</code> paired with their index. Indices start at 0.
        */
      def zipWithIndex[V1 >: V]: NonEmptyMap[(K, V1), Int] = (nonEmptyMap: Map[K, V1]).zipWithIndex.toMap  
    }

  }
  opaque type NonEmptyString = String

  /**
    * Companion object for class <code>NonEmptyString</code>.
    */
  object NonEmptyString {
    /**
      * Constructs a new <code>NonEmptyString</code> given at least one element.
      *
      * This overload requires a string literal at compile time and rejects empty
      * string literals or non-literals with a compile-time error.
      *
      * @param s the <code>String</code> represented by this <code>NonEmptyString</code>
      */
    inline def apply[S <: String & Singleton](inline s: S): NonEmptyString =
      inline constValueOpt[S] match {
        case Some(v: String) =>
          inline if v == "" then
            error("NonEmptyString cannot be instantiated with an empty string literal")
          else
            v.asInstanceOf[NonEmptyString]
        case None =>
          error("NonEmptyString.apply requires a string literal")
      }

    /**
      * Constructs a new <code>NonEmptyString</code> given at least one character.
      *
      * @param firstChar the first character (with index 0) contained in this <code>NonEmptyString</code>
      * @param otherChars a varargs of zero or more other characters (with index 1, 2, 3, ...) contained in this <code>NonEmptyString</code>
      */
    def apply(firstChar: Char, otherChars: Char*): NonEmptyString = firstChar + otherChars.mkString

    /**
      * Variable argument extractor for <code>NonEmptyString</code>s.
      *
      * @param nonEmptyString: the <code>NonEmptyString</code> containing the elements to extract
      * @return an <code>Seq</code> containing this <code>NonEmptyString</code>s elements, wrapped in a <code>Some</code> 
      */
    def unapplySeq(nonEmptyString: NonEmptyString): Option[Seq[String]] = Some(Seq(nonEmptyString))

    /**
      *
      * A factory/assertion method that produces a <code>NonEmptyString</code>
      * given a valid <code>String</code> value, or throws
      * <code>AssertionError</code>, if given an invalid <code>String</code> value.
      *
      * Note: you should use this method only when you are convinced that it will
      * always succeed, i.e., never throw an exception. It is good practice to
      * add a comment near the invocation of this method indicating ''why'' you
      * think it will always succeed to document your reasoning. If you are not
      * sure an `ensuringValid` call will always succeed, you should use one of
      * the other factory or validation methods provided on this object instead:
      * `from'.
      *
      * @param string the <code>String</code> to check to see if it is a valid.
      * @return the <code>NonEmptyString</code> if the passed string is valid..
      * @throws AssertionError if the passed string is not valid.
      */
    def ensuringValid(string: String): NonEmptyString =
      if (string.length == 0)
        throw new AssertionError(Resources.nonEmptyStringEmpty)
      else
        string

    /**
      * Optionally construct a <code>NonEmptyString</code> containing the characters, if any, of a given <code>GenSeq</code>.
      *
      * @param seq the <code>GenSeq</code> of <code>Char</code> with which to construct a <code>NonEmptyString</code>
      * @return a <code>NonEmptyString</code> containing the elements of the given <code>GenSeq</code>, if non-empty, wrapped in
      *     a <code>Some</code>; else <code>None</code> if the <code>GenSeq</code> is empty
      */
    def from[T](seq: GenSeq[Char]): Option[NonEmptyString] =
      seq.headOption match {
        case None => None
        case Some(first) => Some(seq.mkString)
      }  

    given Conversion[NonEmptyString, PartialFunction[Int, Char]] with {
      def apply(nonEmptyString: NonEmptyString): PartialFunction[Int, Char] =
        new PartialFunction[Int, Char] {
          def isDefinedAt(idx: Int): Boolean = idx >= 0 && idx < nonEmptyString.length
          def apply(idx: Int): Char = nonEmptyString.charAt(idx)
        }
    }

    given Conversion[NonEmptyString, IterableOnce[Char]] with {
      def apply(nonEmptyString: NonEmptyString): IterableOnce[Char] =
        new IterableOnce[Char] {
          def iterator: Iterator[Char] = new StringOps(nonEmptyString).iterator
        }
    }

    extension [T] (other: Char) {
      /**
        * Returns a new <code>NonEmptyString</code> containing the passed <code>Char</code> followed by this <code>NonEmptyString</code>.
        *
        * @param theString the <code>NonEmptyString</code> to append
        * @return a new <code>NonEmptyString</code> that contains <code>other</code> followed by this <code>NonEmptyString</code>.
        */
      def +:(theString: NonEmptyString): NonEmptyString = other.toString ++ theString.toString
    }  

    extension (nonEmptyString: NonEmptyString) {

      /**
        * Returns a new <code>NonEmptyString</code> containing this <code>NonEmptyString</code> followed by the passed <code>NonEmptyString</code>.
        *
        * @param other the <code>NonEmptyString</code> to append
        * @return a new <code>NonEmptyString</code> that contains this <code>NonEmptyString</code> followed by <code>other</code>.
        */
      def ++(other: IterableOnce[Char]): NonEmptyString = nonEmptyString + other.mkString

      /**
        * Returns a new <code>NonEmptyString</code> with the given character appended.
        *
        * <p>
        * Note a mnemonic for <code>+:</code> <em>vs.</em> <code>:+</code> is: the COLon goes on the COLlection side.
        * </p>
        *
        * @param c the character to append to this <code>NonEmptyString</code>
        * @return a new <code>NonEmptyString</code> consisting of all characters of this <code>NonEmptyString</code> followed by the given <code>c</code>.
        */
      def :+(c: Char): NonEmptyString = new NonEmptyString(new StringOps(nonEmptyString) :+ c)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder. The written text will consist of a concatenation of the result of invoking <code>toString</code>
        * on of every element of this <code>NonEmptyString</code>, without any separator string.
        *
        * @param sb the string builder to which characters will be appended
        * @return the string builder, <code>sb</code>, to which elements were appended.
        */
      def addString(sb: StringBuilder): StringBuilder = new StringOps(nonEmptyString).addString(sb)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder using a separator string. The written text will consist of a concatenation of the
        * result of invoking <code>toString</code>
        * on of every character of this <code>NonEmptyString</code>, separated by the string <code>sep</code>.
        *
        * @param sb the string builder to which characters will be appended
        * @param sep the separator string
        * @return the string builder, <code>sb</code>, to which characters were appended.
        */
      def addString(sb: StringBuilder, sep: String): StringBuilder = new StringOps(nonEmptyString).addString(sb, sep)

      /**
        * Appends all characters of this <code>NonEmptyString</code> to a string builder using start, end, and separator strings. The written text will consist of a concatenation of
        * the string <code>start</code>; the result of invoking <code>toString</code> on all characters of this <code>NonEmptyString</code>,
        * separated by the string <code>sep</code>; and the string <code>end</code>
        *
        * @param sb the string builder to which characters will be appended
        * @param start the starting string
        * @param sep the separator string
        * @param end the ending string
        * @return the string builder, <code>sb</code>, to which characters were appended.
        */
      def addString(sb: StringBuilder, start: String, sep: String, end: String): StringBuilder = new StringOps(nonEmptyString).addString(sb, start, sep, end)

      /**
        * Tests whether this <code>NonEmptyString</code> can be compared for equality with the given object.
        *
        * @param that the object to test
        * @return true if this <code>NonEmptyString</code> can be compared for equality with <code>that</code>, false otherwise.
        */
      def canEqual(that: Any): Boolean = that.isInstanceOf[NonEmptyString] || that.isInstanceOf[String]

      /**
        * Gets a character by its index in the <code>NonEmptyString</code>.
        *
        * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
        */
      def charAt(idx: Int): Char = nonEmptyString.charAt(idx)

      /**
        * Finds the first character of this <code>NonEmptyString</code> for which the given partial function is defined, if any, and applies the partial function to it.
        *
        * @param pf the partial function
        * @return an <code>Option</code> containing <code>pf</code> applied to the first character for which it is defined, or <code>None</code> if
        *    the partial function was not defined for any character.
        */
      def collectFirst[U](pf: PartialFunction[Char, U]): Option[U] = 
        nonEmptyString.find(c => pf.isDefinedAt(c)).map(c => pf(c))

      /**
        * Indicates whether this <code>NonEmptyString</code> contains a given value as an character.
        *
        * @param c the element to look for
        * @return true if this <code>NonEmptyString</code> has an character that is equal (as determined by <code>==)</code> to <code>c</code>, false otherwise.
        */
      def contains(c: Char): Boolean = nonEmptyString.indexOf(c) >= 0  

      /**
        * Indicates whether this <code>NonEmptyString</code> contains a given <code>IterableOnce</code> of characters as a slice.
        *
        * @param that the <code>IterableOnce</code> character slice to look for
        * @return true if this <code>NonEmptyString</code> contains a slice with the same characters as <code>that</code>, otherwise <code>false</code>.
        */
      def containsSlice(that: IterableOnce[Char]): Boolean = nonEmptyString.indexOf(that.mkString) >= 0

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>. Copying
        * will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        */
      def copyToArray(arr: Array[Char]): Unit = new StringOps(nonEmptyString).copyToArray(arr, 0)

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with characters of this <code>NonEmptyString</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, or the end of the array is reached.
        *
        * @param arr the array to fill
        * @param start the starting index
        */
      def copyToArray(arr: Array[Char], start: Int): Unit = new StringOps(nonEmptyString).copyToArray(arr, start)

      /**
        * Copies characters of this <code>NonEmptyString</code> to an array. Fills the given array <code>arr</code> with at most <code>len</code> characters of this <code>NonEmptyString</code>, beginning at
        * index <code>start</code>. Copying will stop once either the end of the current <code>NonEmptyString</code> is reached, the end of the array is reached, or
        * <code>len</code> elements have been copied.
        *
        * @param arr the array to fill
        * @param start the starting index
        * @param len the maximum number of elements to copy
        */
      def copyToArray(arr: Array[Char], start: Int, len: Int): Unit = new StringOps(nonEmptyString).copyToArray(arr, start, len)

      /**
        * Copies all characters of this <code>NonEmptyString</code> to a buffer.
        *
        * @param buf the buffer to which characters are copied
        */
      def copyToBuffer(buf: Buffer[Char]): Unit = nonEmptyString.toList.copyToBuffer(buf)

      /**
        * Indicates whether every character of this <code>NonEmptyString</code> relates to the corresponding element of a given <code>IterableOnce</code> by satisfying a given predicate.
        *
        * @tparam B the type of the elements of <code>that</code>
        * @param that the <code>IterableOnce</code> to compare for correspondence
        * @param p the predicate, which relates elements from this <code>NonEmptyString</code> and the passed <code>IterableOnce</code>
        * @return true if this <code>NonEmptyString</code> and the passed <code>IterableOnce</code> have the same length and <code>p(x, y)</code> is <code>true</code>
        *     for all corresponding elements <code>x</code> of this <code>NonEmptyString</code> and <code>y</code> of that, otherwise <code>false</code>.
        */
      def corresponds[B](that: IterableOnce[B])(p: (Char, B) => Boolean): Boolean = nonEmptyString.toList.corresponds(that)(p)

      /**
        * Counts the number of characters in this <code>NonEmptyString</code> that satisfy a predicate.
        *
        * @param p the predicate used to test characters.
        * @return the number of characters satisfying the predicate <code>p</code>.
        */
      def count(p: Char => Boolean): Int = new StringOps(nonEmptyString).count(p)

      /**
        * Builds a new <code>NonEmptyString</code> from this <code>NonEmptyString</code> without any duplicate characters.
        *
        * @return A new <code>NonEmptyString</code> that contains the first occurrence of every character of this <code>NonEmptyString</code>.
        */
      def distinct: NonEmptyString = new StringOps(nonEmptyString).distinct

      /**
        * Selects a character by its index in the <code>NonEmptyString</code>.
        *
        * @return the character of this <code>NonEmptyString</code> at index <code>idx</code>, where 0 indicates the first element.
        */
      //def apply(idx: Int): Char = nonEmptyString(idx)

      /**
        * The length of this <code>NonEmptyString</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of characters in this <code>NonEmptyString</code>.
        */
      def length: Int = nonEmptyString.toString.length

      /**
        * Indicates whether this <code>NonEmptyString</code> ends with the given <code>IterableOnce</code>.
        *
        * @param that the <code>IterableOnce</code> to test
        * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a suffix, <code>false</code> otherwise. 
        */
      def endsWith(that: IterableOnce[Char]): Boolean = nonEmptyString.endsWith(that.mkString)

      /**
        * Indicates whether a predicate holds for at least one of the characters of this <code>NonEmptyString</code>.
        *
        * @param p the predicate used to test characters.
        * @return <code>true</code> if the given predicate <code>p</code> holds for some of the elements of this <code>NonEmptyString</code>, otherwise <code>false</code>. 
        */
      def exists(p: Char => Boolean): Boolean = new StringOps(nonEmptyString).exists(p)

      /**
        * Finds the first character of this <code>NonEmptyString</code> that satisfies the given predicate, if any.
        *
        * @param p the predicate used to test characters
        * @return an <code>Some</code> containing the first character in this <code>NonEmptyString</code> that satisfies <code>p</code>, or <code>None</code> if none exists.
        */
      def find(p: Char => Boolean): Option[Char] = new StringOps(nonEmptyString).find(p)

      /**
        * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code> and using the characters of the resulting <code>NonEmptyString</code>s.
        *
        * @param f the function to apply to each character.
        * @return a new <code>NonEmptyString</code> containing characters obtained by applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and concatenating
        *    the characters of resulting <code>NonEmptyString</code>s.
        */
      def flatMap(f: Char => NonEmptyString): NonEmptyString = new StringOps(nonEmptyString).flatMap(f)

      /**
        * Builds a new <code>NonEmptyString</code> by applying a function to all characters of this <code>NonEmptyString</code>.
        *
        * @tparam U the character type of the returned <code>NonEmptyString</code>.
        * @param f the function to apply to each character.
        * @return a new <code>NonEmptyString</code> resulting from applying the given function <code>f</code> to each character of this <code>NonEmptyString</code> and collecting the results.
        */
      def map[U](f: Char => U): NonEmptyString =
        new StringOps(nonEmptyString).map { c =>
          f(c).toString
        }.mkString

      /**
        * Filters the characters of this <code>NonEmptyString</code> that satisfy the given predicate.
        *
        * @param p the predicate used to test characters
        * @return a new <code>String</code> containing the characters in this <code>NonEmptyString</code> that satisfy <code>p</code>.
        */
      def filter(p: Char => Boolean): String = new StringOps(nonEmptyString).filter(p)

      /**
        * Filters the characters of this <code>NonEmptyString</code> that does not satisfy the given predicate.
        *
        * @param p the predicate used to test characters
        * @return a new <code>String</code> does not contain the characters in this <code>NonEmptyString</code> that satisfy <code>p</code>.
        */
      def filterNot(p: Char => Boolean): String = new StringOps(nonEmptyString).filterNot(p)

      /**
        * Folds the characters of this <code>NonEmptyString</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on characters is unspecified and may be nondeterministic.
        * </p>
        *
        * @param z a neutral character for the fold operation; may be added to the result an arbitrary number of
        *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
        *     0 for addition, or 1 for multiplication.)
        * @param op a binary operator that must be associative
        * @return the result of applying fold operator <code>op</code> between all the elements and <code>z</code>
        */
      def fold(z: Char)(op: (Char, Char) => Char): Char = new StringOps(nonEmptyString).fold(z)(op)

      /**
        * Applies a binary operator to a start value and all characters of this <code>NonEmptyString</code>, going left to right.
        *
        * @tparam B the result type of the binary operator.
        * @param z the start value.
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going left to right, with the start value,
        *     <code>z</code>, on the left:
        *
        * <pre>
        * op(...op(op(z, x_1), x_2), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def foldLeft[B](z: B)(op: (B, Char) => B): B = new StringOps(nonEmptyString).foldLeft(z)(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code> and a start value, going right to left.
        *
        * @tparam B the result of the binary operator
        * @param z the start value
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going right to left, with the start value,
        *     <code>z</code>, on the right:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_n, z)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def foldRight[B](z: B)(op: (Char, B) => B): B = new StringOps(nonEmptyString).foldRight(z)(op)  

      /**
        * Indicates whether a predicate holds for all characters of this <code>NonEmptyString</code>.
        *
        * @param p the predicate used to test characters.
        * @return <code>true</code> if the given predicate <code>p</code> holds for all characters of this <code>NonEmptyString</code>, otherwise <code>false</code>.
        */
      def forall(p: Char => Boolean): Boolean = new StringOps(nonEmptyString).forall(p)

      /**
        * Applies a function <code>f</code> to all characters of this <code>NonEmptyString</code>.
        *
        * @param f the function that is applied for its side-effect to every character. The result of function <code>f</code> is discarded.
        */
      def foreach(f: Char => Unit): Unit = new StringOps(nonEmptyString).foreach(f)

      /**
        * Partitions this <code>NonEmptyString</code> into a map of <code>NonEmptyString</code>s according to some discriminator function.
        *
        * @tparam K the type of keys returned by the discriminator function.
        * @param f the discriminator function.
        * @return A map from keys to <code>NonEmptyString</code>s such that the following invariant holds:
        *
        * <pre>
        * (nonEmptyString.toString partition f)(k) = xs filter (x =&gt; f(x) == k)
        * </pre>
        *
        * <p>
        * That is, every key <code>k</code> is bound to a <code>NonEmptyString</code> of those elements <code>x</code> for which <code>f(x)</code> equals <code>k</code>.
        * </p>
        */
      def groupBy[K](f: Char => K): Map[K, NonEmptyString] = {
        val mapKToString = new StringOps(nonEmptyString).groupBy(f)
        mapKToString.mapValues { list => new NonEmptyString(list) }.toMap
      }

      /**
        * Partitions characters into fixed size <code>NonEmptyString</code>s.
        *
        * @param size the number of characters per group
        * @return An iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last will be truncated if the characters don't divide evenly.
        */
      def grouped(size: Int): Iterator[NonEmptyString] = {
        if (size > 0) {
          val itOfString = new StringOps(nonEmptyString).grouped(size)
          itOfString.map { list => new NonEmptyString(list) }
        }
        else
          throw new IllegalArgumentException(Resources.invalidSize(size))
      }

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code> has a definite size, since all <code>NonEmptyString</code>s are strict collections.
        */
      def hasDefiniteSize: Boolean = true

      // override def hashCode: Int = toString.hashCode

      /**
        * Selects the first character of this <code>NonEmptyString</code>.
        *
        * @return the first character of this <code>NonEmptyString</code>.
        */
      def head: Char = nonEmptyString.charAt(0) // Can never be empty, so safe

      /**
        * Selects the first character of this <code>NonEmptyString</code> and returns it wrapped in a <code>Some</code>.
        *
        * @return the first character of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>.
        */
      def headOption: Option[Char] = Some(head)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyString</code>.
        *
        * @param c the character value to search for.
        * @return the index of the first character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(c: Char): Int = nonEmptyString.toList.indexOf(c, 0)

      /**
        * Finds index of first occurrence of some value in this <code>NonEmptyString</code> after or at some start index.
        *
        * @param c the character value to search for.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first element of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>elem</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexOf(c: Char, from: Int): Int = nonEmptyString.toList.indexOf(c, from)

      /**
        * Finds first index where this <code>NonEmptyString</code> contains a given <code>IterableOnce[Char]</code> as a slice.
        *
        * @param that the <code>IterableOnce[Char]</code> defining the slice to look for
        * @return the first index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *     <code>IterableOnce</code> <code>that</code>, or <code>-1</code> of no such subsequence exists.
        */
      def indexOfSlice(that: IterableOnce[Char]): Int = nonEmptyString.toIndexedSeq.indexOfSlice(that.mkString)

      /**
        * Finds first index after or at a start index where this <code>NonEmptyString</code> contains a given <code>IterableOnce[Char]</code> as a slice.
        *
        * @param that the <code>IterableOnce[Char]</code> defining the slice to look for
        * @param from the start index
        * @return the first index <code>&gt;=</code> <code>from</code> at which the characters of this <code>NonEmptyString</code> starting at that index match the characters of
        *     <code>IterableOnce[Char]</code> <code>that</code>, or <code>-1</code> of no such subsequence exists.
        */
      def indexOfSlice(that: IterableOnce[Char], from: Int): Int = {
        val normalizedFrom = math.max(0, from)
        val seq: Seq[Char] = nonEmptyString.toSeq
        val searchSeq: Seq[Char] = that.iterator.toSeq
        seq.indexOfSlice(searchSeq, normalizedFrom)
      }

      /**
        * Finds index of the first character satisfying some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the index of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: Char => Boolean): Int = new StringOps(nonEmptyString).indexWhere(p)

      /**
        * Finds index of the first character satisfying some predicate after or at some start index.
        *
        * @param p the predicate used to test characters.
        * @param from the start index
        * @return the index <code>&gt;=</code> <code>from</code> of the first character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists.
        */
      def indexWhere(p: Char => Boolean, from: Int): Int = new StringOps(nonEmptyString).indexWhere(p, from)

      /**
        * Produces the range of all indices of this <code>NonEmptyString</code>. 
        *
        * @return a <code>Range</code> value from <code>0</code> to one less than the length of this <code>NonEmptyString</code>. 
        */
      def indices: Range = new StringOps(nonEmptyString).indices

      /**
        * Returns <code>false</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
        *
        * @return false
        */
      def isEmpty: Boolean = false

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, can be traversed repeatedly.
        *
        * @return true
        */
      def isTraversableAgain: Boolean = true

      /**
        * Selects the last character of this <code>NonEmptyString</code>.
        *
        * @return the last character of this <code>NonEmptyString</code>.
        */
      def last: Char = if (length > 1) nonEmptyString.charAt(length - 1) else nonEmptyString.charAt(0) // Can never be empty, so safe

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code>.
        *
        * @param c the character value to search for.
        * @return the index of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>) to <code>c</code>,
        *     or <code>-1</code>, if none exists.
        */
      def lastIndexOf(c: Char): Int = nonEmptyString.toList.lastIndexOf(c)

      /**
        * Finds the index of the last occurrence of some value in this <code>NonEmptyString</code> before or at a given <code>end</code> index.
        *
        * @param c the character value to search for.
        * @param end the end index. 
        * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that is equal (as determined by <code>==</code>)
        *     to <code>elem</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexOf(c: Char, end: Int): Int = nonEmptyString.toList.lastIndexOf(c, end)

      /**
        * Finds the last index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
        *
        * @param that the <code>Every</code> defining the slice to look for
        * @return the last index at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice(that: IterableOnce[Char]): Int = nonEmptyString.toIndexedSeq.lastIndexOfSlice(that.toVector)

      /**
        * Finds the last index before or at a given end index where this <code>NonEmptyString</code> contains a given <code>Every</code> as a slice. 
        *
        * @param that the <code>Every</code> defining the slice to look for
        * @param end the end index
        * @return the last index <code>&gt;=</code> <code>end</code> at which the elements of this <code>NonEmptyString</code> starting at that index match the characters of
        *    <code>Every</code> <code>that</code>, or <code>-1</code> of no such subsequence exists. 
        */
      def lastIndexOfSlice(that: IterableOnce[Char], end: Int): Int = nonEmptyString.toIndexedSeq.lastIndexOfSlice(that.toVector, end)

       /**
        * Finds index of last character satisfying some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the index of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>, or <code>-1</code>, if none exists.
        */
      def lastIndexWhere(p: Char => Boolean): Int = nonEmptyString.toList.lastIndexWhere(p)

      /**
        * Finds index of last character satisfying some predicate before or at given end index.
        *
        * @param p the predicate used to test characters.
        * @param end the end index
        * @return the index <code>&gt;=</code> <code>end</code> of the last character of this <code>NonEmptyString</code> that satisfies the predicate <code>p</code>,
        *     or <code>-1</code>, if none exists. 
        */
      def lastIndexWhere(p: Char => Boolean, end: Int): Int = nonEmptyString.toList.lastIndexWhere(p, end)

      /**
        * Returns the last element of this <code>NonEmptyString</code>, wrapped in a <code>Some</code>. 
        *
        * @return the last element, wrapped in a <code>Some</code>. 
        */
      def lastOption: Option[Char] = nonEmptyString.toList.lastOption // Will always return a Some

      /**
        * Compares the length of this <code>NonEmptyString</code> to a test value. 
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
      def lengthCompare(len: Int): Int = new StringOps(nonEmptyString).lengthCompare(len)

      /**
        * Finds the largest character.
        *
        * @return the largest element of this <code>NonEmptyString</code>. 
        */
      def max(using cmp: Ordering[Char]): Char = nonEmptyString.toList.max(cmp)

      /**
        * Finds the largest result after applying the given function to every character.
        *
        * @return the largest result of applying the given function to every character of this <code>NonEmptyString</code>.
        */
      def maxBy[U](f: Char => U)(using cmp: Ordering[U]): Char = nonEmptyString.toList.maxBy(f)(cmp)

      /**
        * Finds the smallest character.
        *
        * @return the smallest character of this <code>NonEmptyString</code>.
        */
      def min(using cmp: Ordering[Char]): Char = nonEmptyString.toList.min(cmp)

      /**
        * Finds the smallest result after applying the given function to every character.
        *
        * @return the smallest result of applying the given function to every character of this <code>NonEmptyString</code>.
        */
      def minBy[U](f: Char => U)(using cmp: Ordering[U]): Char = nonEmptyString.toList.minBy(f)(cmp)

      /**
        * Displays all characters of this <code>NonEmptyString</code> in a string.
        *
        * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all characters of this
        *     <code>NonEmptyString</code> follow each other without any separator string. 
        */
      def mkString: NonEmptyString = new StringOps(nonEmptyString).mkString

      /**
        * Displays all elements of this <code>NonEmptyString</code> in a string using a separator string. 
        *
        * @param sep the separator string
        * @return a string representation of this <code>NonEmptyString</code>. In the resulting string, the result of invoking <code>toString</code> on all elements of this
        *     <code>NonEmptyString</code> are separated by the string <code>sep</code>. 
        */
      def mkString(sep: String): NonEmptyString = new StringOps(nonEmptyString).mkString(sep)

      /**
        * Displays all characters of this <code>NonEmptyString</code> in a string using start, end, and separator strings.
        *
        * @param start the starting string.
        * @param sep the separator string.
        * @param end the ending string.
        * @return a string representation of this <code>NonEmptyString</code>. The resulting string begins with the string <code>start</code> and ends with the string
        *     <code>end</code>. Inside, In the resulting string, the result of invoking <code>toString</code> on all characters of this <code>NonEmptyString</code> are
        *     separated by the string <code>sep</code>. 
        */
      def mkString(start: String, sep: String, end: String): NonEmptyString = new StringOps(nonEmptyString).mkString(start, sep, end)

      /**
        * Returns <code>true</code> to indicate this <code>NonEmptyString</code>, like all <code>NonEmptyString</code>s, is non-empty.
        *
        * @return true
        */
      def nonEmpty: Boolean = true

      /**
        * A copy of this <code>NonEmptyString</code> with an element value appended until a given target length is reached.
        *
        * @param len the target length 
        * @param c the padding character
        * @return a new <code>NonEmptyString</code> consisting of all characters of this <code>NonEmptyString</code> followed by the minimal number of occurrences
        *     of <code>elem</code> so that the resulting <code>NonEmptyString</code> has a length of at least <code>len</code>. 
        */
      def padTo(len: Int, c: Char): NonEmptyString =
        new StringOps(nonEmptyString).padTo(len, c)

      /**
        * Produces a new <code>NonEmptyString</code> where a slice of characters in this <code>NonEmptyString</code> is replaced by another <code>NonEmptyString</code>
        *
        * @param from the index of the first replaced character
        * @param that the <code>NonEmptyString</code> whose characters should replace a slice in this <code>NonEmptyString</code>
        * @param replaced the number of characters to drop in the original <code>NonEmptyString</code>
        */
      def patch(from: Int, that: NonEmptyString, replaced: Int): NonEmptyString =
        new StringOps(nonEmptyString).patch(from, that, replaced)

      /**
        * Iterates over distinct permutations. 
        *
        * <p>
        * Here's an example:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyString("abb").permutations.toList == list(NonEmptyString("abb"), NonEmptyString("bab"), NonEmptyString("bba"))
        * </pre>
        *
        * @return an iterator that traverses the distinct permutations of this <code>NonEmptyString</code>.
        */
      def permutations: Iterator[NonEmptyString] = {
        val it = new StringOps(nonEmptyString).permutations
        it map { list => new NonEmptyString(list) }
      }

      /**
        * Returns the length of the longest prefix whose characters all satisfy some predicate.
        *
        * @param p the predicate used to test characters.
        * @return the length of the longest prefix of this <code>NonEmptyString</code> such that every characters
        *     of the segment satisfies the predicate <code>p</code>. 
        */
      def prefixLength(p: Char => Boolean): Int = nonEmptyString.toList.prefixLength(p)

      /**
        * The result of multiplying all the characters of this <code>NonEmptyString</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyString</code> for which a given <code>Numeric[T]</code> exists.
        * </p>
        *
        * @return the product of all elements
        */
      def product(using num: Numeric[Char]): Char = nonEmptyString.toList.product(num)

      /**
        * Reduces the elements of this <code>NonEmptyString</code> using the specified associative binary operator.
        *
        * <p>
        * The order in which operations are performed on characters is unspecified and may be nondeterministic.
        * </p>
        *
        * @param op a binary operator that must be associative.
        * @return the result of applying reduce operator <code>op</code> between all the characters of this <code>NonEmptyString</code>.
        */
      def reduce(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduce(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going left to right.
        *
        * @param op the binary operator.
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going left to right:
        *
        * <pre>
        * op(...op(op(x_1, x_2), x_3), ..., x_n)
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the characters of this <code>NonEmptyString</code>.
        * </p>
        */
      def reduceLeft(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduceLeft(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going left to right, returning the result in a <code>Some</code>.
        *
        * @param op the binary operator.
        * @return a <code>Some</code> containing the result of <code>reduceLeft(op)</code>
        * </p>
        */
      def reduceLeftOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceLeftOption(op)

      /** 
        * If this <code>String</code> is nonempty, reduces it with the given binary operator `op`.
        *
        *  The behavior is the same as <code>reduce</code> except that the value is `None` if the <code>String</code>
        *  is empty.
        *
        *  @param op      A binary operator; must be associative for the result to always be the
        *                 same across runs.
        *  @return        The result of reducing this <code>String</code> with `op` if the <code>String</code> is nonempty,
        *                 inside a `Some`, and `None` otherwise.
        */
      def reduceOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceOption(op)

      /**
        * Applies a binary operator to all characters of this <code>NonEmptyString</code>, going right to left.
        *
        * @param op the binary operator
        * @return the result of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>, going right to left:
        *
        * <pre>
        * op(x_1, op(x_2, ... op(x_{n-1}, x_n)...))
        * </pre>
        *
        * <p>
        * where x<sub>1</sub>, ..., x<sub>n</sub> are the elements of this <code>NonEmptyString</code>. 
        * </p>
        */
      def reduceRight(op: (Char, Char) => Char): Char = nonEmptyString.toList.reduceRight(op)

      /**
        * Applies a binary operator to all elements of this <code>NonEmptyString</code>, going right to left, returning the result in a <code>Some</code>.
        *
        * @param op the binary operator
        * @return a <code>Some</code> containing the result of <code>reduceRight(op)</code>
        */
      def reduceRightOption(op: (Char, Char) => Char): Option[Char] = nonEmptyString.toList.reduceRightOption(op)

      /**
        * Returns new <code>NonEmptyString</code> with characters in reverse order.
        *
        * @return a new <code>NonEmptyString</code> with all characters of this <code>NonEmptyString</code> in reversed order.
        */
      def reverse: NonEmptyString =
        new StringOps(nonEmptyString).reverse

      /**
        * An iterator yielding characters in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyString.reverseIterator</code> is the same as <code>nonEmptyString.reverse.iterator</code>, but might be more efficient. 
        * </p>
        *
        * @return an iterator yielding the characters of this <code>NonEmptyString</code> in reversed order
        */
      def reverseIterator: Iterator[Char] = new StringOps(nonEmptyString).reverseIterator

      /**
        * Builds a new <code>Iterable</code> by applying a function to all characters of this <code>NonEmptyString</code> and collecting the results in reverse order.
        *
        * <p>
        * Note: <code>nonEmptyString.reverseMap(f)</code> is the same as <code>nonEmptyString.reverse.map(f)</code>, but might be more efficient. 
        * </p>
        *
        * @tparam U the element type of the returned <codeIterable</code>.
        * @param f the function to apply to each character.
        * @return a new <code>Iterable</code> resulting from applying the given function <code>f</code> to each character of this <code>NonEmptyString</code>
        *     and collecting the results in reverse order. 
        */
      def reverseMap[U](f: Char => U): Iterable[U] = nonEmptyString.toList.reverseMap(f)

      /**
        * Checks if the given <code>IterableOnce</code> contains the same characters in the same order as this <code>NonEmptyString</code>.
        *
        * @param that the <code>IterableOnce</code> with which to compare
        * @return <code>true</code>, if both this <code>NonEmptyString</code> and the given <code>IterableOnce</code> contain the same characters
        *     in the same order, <code>false</code> otherwise. 
        */
      def sameElements(that: IterableOnce[Char]): Boolean = nonEmptyString.toList.sameElements(that)

      /**
        * Computes a prefix scan of the characters of this <code>NonEmptyString</code>.
        *
        * <p>
        * Note: The neutral character z may be applied more than once.
        * </p>
        *
        * <p>
        * Here are some examples:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyString("123").scan(0)(_ + _) == NonEmptyString(0, 1, 3, 6)
        * NonEmptyString("123").scan("z")(_ + _.toString) == NonEmptyString("z", "z1", "z12", "z123")
        * </pre>
        *
        * @param z a neutral element for the scan operation; may be added to the result an arbitrary number of
        *     times, and must not change the result (<em>e.g.</em>, <code>Nil</code> for list concatenation,
        *     0 for addition, or 1 for multiplication.)
        * @param op a binary operator that must be associative
        * @return a new <code>NonEmptyString</code> containing the prefix scan of the elements in this <code>NonEmptyString</code> 
        */
      def scan(z: Char)(op: (Char, Char) => Char): NonEmptyString = nonEmptyString.toList.scan(z)(op).mkString

      /**
        * Produces a <code>NonEmptyString</code> containing cumulative results of applying the operator going left to right.
        *
        * <p>
        * Here are some examples:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyString("123").scanLeft(0)(_ + _.toString.toInt) == Vector(0, 1, 3, 6)
        * NonEmptyString("123").scanLeft("z")(_ + _) == Vector("z", "z1", "z12", "z123")
        * </pre>
        *
        * @tparam B the result type of the binary operator and type of the resulting <code>NonEmptyString</code>
        * @param z the start value.
        * @param op the binary operator.
        * @return a new <code>NonEmptyString</code> containing the intermediate results of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>,
        *     going left to right, with the start value, <code>z</code>, on the left.
        */
      def scanLeft[B](z: B)(op: (B, Char) => B): Iterable[B] = nonEmptyString.toList.scanLeft(z)(op)

      /**
        * Produces a <code>NonEmptyString</code> containing cumulative results of applying the operator going right to left.
        *
        * <p>
        * Here are some examples:
        * </p>
        *
        * <pre class="stHighlight">
        * NonEmptyString("123").scanRight(0)(_.toString.toInt + _) == NonEmptyString(6, 5, 3, 0)
        * NonEmptyString("123").scanRight("z")(_ + _) == NonEmptyString("123z", "23z", "3z", "z")
        * </pre>
        *
        * @tparam B the result of the binary operator and type of the resulting <code>NonEmptyString</code>
        * @param z the start value
        * @param op the binary operator
        * @return a new <code>NonEmptyString</code> containing the intermediate results of inserting <code>op</code> between consecutive characters of this <code>NonEmptyString</code>,
        *     going right to left, with the start value, <code>z</code>, on the right.
        */
      def scanRight[B](z: B)(op: (Char, B) => B): Iterable[B] = nonEmptyString.toList.scanRight(z)(op)

      /**
        * Computes length of longest segment whose characters all satisfy some predicate.
        *
        * @param p the predicate used to test elements.
        * @param from the index where the search starts.
        * @return the length of the longest segment of this <code>NonEmptyString</code> starting from index <code>from</code> such that every character of the
        *     segment satisfies the predicate <code>p</code>. 
        */
      def segmentLength(p: Char => Boolean, from: Int): Int = nonEmptyString.toList.segmentLength(p, from)

      /**
        * Groups characters in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.)
        *
        * @param size the number of characters per group
        * @return an iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last and the only element will be truncated
        *     if there are fewer characters than <code>size</code>.
        */
      def sliding(size: Int): Iterator[NonEmptyString] = new StringOps(nonEmptyString).sliding(size).map(new NonEmptyString(_))

      /**
        * Groups characters in fixed size blocks by passing a &ldquo;sliding window&rdquo; over them (as opposed to partitioning them, as is done in grouped.),
        * moving the sliding window by a given <code>step</code> each time.
        *
        * @param size the number of characters per group
        * @param step the distance between the first characters of successive groups
        * @return an iterator producing <code>NonEmptyString</code>s of size <code>size</code>, except the last and the only character will be truncated
        *     if there are fewer characters than <code>size</code>.
        */
      def sliding(size: Int, step: Int): Iterator[NonEmptyString] = new StringOps(nonEmptyString).sliding(size, step).map(new NonEmptyString(_))

      /**
        * The size of this <code>NonEmptyString</code>.
        *
        * <p>
        * Note: <code>length</code> and <code>size</code> yield the same result, which will be <code>&gt;</code>= 1. 
        * </p>
        *
        * @return the number of characters in this <code>NonEmptyString</code>.
        */
      def size: Int = new StringOps(nonEmptyString).size

      /**
        * Sorts this <code>NonEmptyString</code> according to the <code>Ordering</code> of the result of applying the given function to every character.
        *
        * @tparam U the target type of the transformation <code>f</code>, and the type where the <code>Ordering</code> <code>ord</code> is defined.
        * @param f the transformation function mapping elements to some other domain <code>U</code>.
        * @param ord the ordering assumed on domain <code>U</code>.
        * @return a <code>NonEmptyString</code> consisting of the elements of this <code>NonEmptyString</code> sorted according to the <code>Ordering</code> where
        *    <code>x &lt; y if ord.lt(f(x), f(y))</code>. 
        */
      def sortBy[U](f: Char => U)(using ord: Ordering[U]): NonEmptyString = new StringOps(nonEmptyString).sortBy(f)

      /**
        * Sorts this <code>NonEmptyString</code> according to a comparison function.
        *
        * <p>
        * The sort is stable. That is, characters that are equal (as determined by <code>lt</code>) appear in the same order in the
        * sorted <code>NonEmptyString</code> as in the original. 
        * </p>
        *
        * @param lt the comparison function that tests whether its first argument precedes its second argument in the desired ordering.
        * @return a <code>NonEmptyString</code> consisting of the elements of this <code>NonEmptyString</code> sorted according to the comparison function <code>lt</code>.
        */
      def sortWith(lt: (Char, Char) => Boolean): NonEmptyString = new StringOps(nonEmptyString).sortWith(lt)

      /**
        * Sorts this <code>NonEmptyString</code> according to an <code>Ordering</code>.
        *
        * <p>
        * The sort is stable. That is, elements that are equal (as determined by <code>lt</code>) appear in the same order in the
        * sorted <code>NonEmptyString</code> as in the original. 
        * </p>
        *
        * @param ord the <code>Ordering</code> to be used to compare elements.
        * @return a <code>NonEmptyString</code> consisting of the characters of this <code>NonEmptyString</code> sorted according to the ordering defined by <code>ord</code>.
        */
      def sorted(using ord: Ordering[Char]): NonEmptyString = new StringOps(nonEmptyString).sorted(ord)

      /**
        * Indicates whether this <code>NonEmptyString</code> starts with the given <code>IterableOnce</code>. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyString</code>
        * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a prefix, <code>false</code> otherwise.
        */
      def startsWith(that: IterableOnce[Char]): Boolean = nonEmptyString.toList.startsWith(that)

      /**
        * Indicates whether this <code>NonEmptyString</code> starts with the given <code>IterableOnce</code> at the given index. 
        *
        * @param that the <code>IterableOnce</code> slice to look for in this <code>NonEmptyString</code>
        * @param offset the index at which this <code>NonEmptyString</code> is searched.
        * @return <code>true</code> if this <code>NonEmptyString</code> has <code>that</code> as a slice at the index <code>offset</code>, <code>false</code> otherwise.
        */
      def startsWith(that: IterableOnce[Char], offset: Int): Boolean = nonEmptyString.toList.startsWith(that.toVector, offset)

      /**
        * The result of summing all the characters of this <code>NonEmptyString</code>.
        *
        * <p>
        * This method can be invoked for any <code>NonEmptyString</code> for which a given <code>Numeric[Char]</code> exists.
        * </p>
        *
        * @return the sum of all elements
        */
      def sum(using num: Numeric[Char]): Long = nonEmptyString.toList.sum(num)

      /**
        * Converts this <code>NonEmptyString</code> into a collection of type <code>Col</code> by copying all elements.
        *
        * @tparam Col the collection type to build.
        * @return a new collection containing all elements of this <code>NonEmptyString</code>. 
        */
      def to[Col[_]](factory: org.scalactic.ColCompatHelper.Factory[Char, Col[Char @ uV]]): Col[Char @ uV] =
        nonEmptyString.toList.to(factory)

      /**
        * Converts this <code>NonEmptyString</code> to an array.
        *
        * @return an array containing all characters of this <code>NonEmptyString</code>. A <code>ClassTag</code> must be available for the element type of this <code>NonEmptyString</code>.
        */
      def toArray(using classTag: ClassTag[Char]): Array[Char] = new StringOps(nonEmptyString).toArray

      /**
        * Converts this <code>NonEmptyString</code> to a <code>Vector</code>.
        *
        * @return a <code>Vector</code> containing all characters of this <code>NonEmptyString</code>.
        */
      def toVector: Vector[Char] = nonEmptyString.toList.toVector

      /**
        * Converts this <code>NonEmptyString</code> to a mutable buffer.
        *
        * @return a buffer containing all characters of this <code>NonEmptyString</code>.
        */
      def toBuffer: Buffer[Char] = nonEmptyString.toList.toBuffer

      /**
        * Converts this <code>NonEmptyString</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all characters of this <code>NonEmptyString</code>.
        */
      def toIndexedSeq: collection.immutable.IndexedSeq[Char] = nonEmptyString.toList.toVector

      /**
        * Converts this <code>NonEmptyString</code> to an iterable collection.
        *
        * @return an <code>Iterable</code> containing all characters of this <code>NonEmptyString</code>.
        */
      def toIterable: scala.collection.Iterable[Char] = nonEmptyString.toList.toIterable

      /**
        * Returns an <code>Iterator</code> over the elements in this <code>NonEmptyString</code>.
        *
        * @return an <code>Iterator</code> containing all characters of this <code>NonEmptyString</code>.
        */
      def toIterator: Iterator[Char] = nonEmptyString.toList.toIterator

      /**
        * Converts this <code>NonEmptyString</code> to a map.
        *
        * <p>
        * This method is unavailable unless the elements are members of <code>Tuple2</code>, each <code>((K, V))</code> becoming a key-value pair
        * in the map. Duplicate keys will be overwritten by later keys.
        * </p>
        *
        * @return a map of type <code>immutable.Map[Int, Char]</code> containing all index/character pairs of type <code>(Int, Char)</code> of this <code>NonEmptyString</code>.
        */
      def toMap: Map[Int, Char] = Map.empty[Int, Char] ++ nonEmptyString.toList.zipWithIndex.map(e => e._2 -> e._1)

      /**
        * Converts this <code>NonEmptyString</code> to an immutable <code>IndexedSeq</code>.
        *
        * @return an immutable <code>IndexedSeq</code> containing all characters of this <code>NonEmptyString</code>.
        */
      def toSeq: collection.immutable.Seq[Char] = nonEmptyString

      /**
        * Converts this <code>NonEmptyString</code> to a set.
        *
        * @return a set containing all characters of this <code>NonEmptyString</code>.
        */
      def toSet: Set[Char] = nonEmptyString.toList.toSet

      /**
        * Converts this <code>NonEmptyString</code> to a stream.
        *
        * @return a stream containing all characters of this <code>NonEmptyString</code>.
        */
      def toStream: Stream[Char] = nonEmptyString.toList.toStream

      /**
        * Produces a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> and also all characters of a given <code>Every</code>.
        *
        * <p>
        * <code>nonEmptyStringX</code> <code>union</code> <code>everyY</code> is equivalent to <code>nonEmptyStringX</code> <code>++</code> <code>everyY</code>.
        * </p>
        *
        * <p>
        * Another way to express this is that <code>nonEmptyStringX</code> <code>union</code> <code>everyY</code> computes the order-presevring multi-set union
        * of <code>nonEmptyStringX</code> and <code>everyY</code>. This <code>union</code> method is hence a counter-part of <code>diff</code> and <code>intersect</code> that
        * also work on multi-sets.
        * </p>
        *
        * @param that the <code>Every</code> to add.
        * @return a new <code>NonEmptyString</code> that contains all characters of this <code>NonEmptyString</code> followed by all characters of <code>that</code> <code>Every</code>.
        */
      def union(that: IterableOnce[Char]): NonEmptyString = nonEmptyString.toList.union(that.toList).mkString

      /**
        * Converts this <code>NonEmptyString</code> of pairs into two <code>NonEmptyString</code>s of the first and second half of each pair. 
        *
        * @tparam L the type of the first half of the character pairs
        * @tparam R the type of the second half of the character pairs
        * @param asPair an using conversion that asserts that the character type of this <code>NonEmptyString</code> is a pair.
        * @return a pair of <code>NonEmptyString</code>s, containing the first and second half, respectively, of each character pair of this <code>NonEmptyString</code>.
        */
      def unzip[L, R](using asPair: Char => (L, R)): (Iterable[L], Iterable[R]) = {
        val unzipped = nonEmptyString.toList.unzip
        (unzipped._1, unzipped._2)
      }

      /**
        * Converts this <code>NonEmptyString</code> of triples into three <code>NonEmptyString</code>s of the first, second, and and third character of each triple.
        *
        * @tparam L the type of the first member of the character triples
        * @tparam R the type of the second member of the character triples
        * @tparam R the type of the third member of the character triples
        * @param asTriple an using conversion that character that the character type of this <code>NonEmptyString</code> is a triple.
        * @return a triple of <code>NonEmptyString</code>s, containing the first, second, and third member, respectively, of each character triple of this <code>NonEmptyString</code>.
        */
      def unzip3[L, M, R](using asTriple: Char => (L, M, R)): (Iterable[L], Iterable[M], Iterable[R]) = {
        val unzipped = nonEmptyString.toList.unzip3
        (unzipped._1, unzipped._2, unzipped._3)
      }

      /**
        * A copy of this <code>NonEmptyString</code> with one single replaced character.
        *
        * @param idx the position of the replacement
        * @param c the replacing character
        * @throws IndexOutOfBoundsException if the passed index is greater than or equal to the length of this <code>NonEmptyString</code>
        * @return a copy of this <code>NonEmptyString</code> with the character at position <code>idx</code> replaced by <code>c</code>.
        */
      def updated(idx: Int, c: Char): NonEmptyString =
        new StringOps(nonEmptyString).updated(idx, c)

      /**
        * Returns a <code>NonEmptyString</code> formed from this <code>NonEmptyString</code> and an iterable collection by combining corresponding
        * elements in pairs. If one of the two collections is shorter than the other, placeholder elements will be used to extend the
        * shorter collection to the length of the longer.
        *
        * @tparam O the element type of the <code>other</code>
        * @param other the <code>IterableOnce</code> providing the second half of each result pair
        * @param thisElem the element to be used to fill up the result if this <code>NonEmptyString</code> is shorter than <code>that</code> <code>Iterable</code>.
        * @param otherElem the element to be used to fill up the result if <code>that</code> <code>IterableOnce</code> is shorter than this <code>NonEmptyString</code>.
        * @return a new <code>NonEmptyString</code> containing pairs consisting of corresponding characters of this <code>NonEmptyString</code> and <code>that</code>. The
        *     length of the returned collection is the maximum of the lengths of this <code>NonEmptyString</code> and <code>that</code>. If this <code>NonEmptyString</code>
        *     is shorter than <code>that</code>, <code>thisElem</code> values are used to pad the result. If <code>that</code> is shorter than this
        *     <code>NonEmptyString</code>, <code>thatElem</code> values are used to pad the result. 
        */
      def zipAll[O](other: collection.IterableOnce[O], thisElem: Char, otherElem: O): Iterable[(Char, O)] =
        nonEmptyString.toList.zipAll(other.toList, thisElem, otherElem)

      /**
        * Zips this <code>NonEmptyString</code>  with its indices.
        *
        * @return A new <code>NonEmptyString</code> containing pairs consisting of all elements of this <code>NonEmptyString</code> paired with their index. Indices start at 0.
        */
      def zipWithIndex: Iterable[(Char, Int)] = nonEmptyString.toList.zipWithIndex

    }
  }

}
