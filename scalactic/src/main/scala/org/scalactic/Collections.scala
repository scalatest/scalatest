/* Copyright 2001-2013 Artima, Inc.
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

import scala.collection.generic.CanBuildFrom
import scala.collection.generic.FilterMonadic
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenIterable
import scala.collection.parallel.mutable.ParArray
import scala.language.higherKinds
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.reflect.ClassTag
import org.scalactic.views._
import org.scalactic.iterators._

class Collections[E](val equality: HashingEquality[E]) { thisCollections =>

  case class Box[+T <: E](value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: Collections[_]#Box[_] if equality eq other.path.equality =>
          equality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = equality.hashCodeFor(value)
    override def toString: String = s"Box(${value.toString})"
    val path: thisCollections.type = thisCollections
  }
  
  class Immutable { thisImmutable =>

    // I think we can just put this flatten on Set itself, and possibly have a flatten
    // method here that works if S is a GenTraversable[T]. That means that they have
    // say a val xss = x.Set(List(1), List(2, 3)). Since the element type is Int, then they
    // can say xss.into(number).flatten and you'd end up with a number.Set(1, 2, 3)
    // for consistency I'd probably say flattenTrav for this one.
    //
    // The above then makes me wonder if there's not another potential method here that is
    // def flatMapTrav(f: S => GenTraversable[T]): thisCollections.Set
    // that way you can say something like:
    // val ss = lower.Set("hi", "ha")
    // ss.into(number).flatMapTrav((s: String) => s.toList.map(_.toInt)))
    // and you'd end up with number.Set(104, 97, 105)
    //
    // scala> 'h'.toInt
    // res1: Int = 104
    //
    // scala> 'a'.toInt
    // res2: Int = 97
    //
    // scala> 'i'.toInt
    // res3: Int = 105
 
    // What's missing is a flatten on Bridge that let's you go to a different Collections than the
    // current one so long as the type parameter is the same. So if you have an x.Set[number.Set], and
    // since number's type parameter is Int, you could say into(anotherIntOne).flatten. Boy that seems like
    // it would be never invoked.
  
    trait Set[+T <: E] extends GenIterableOnce[T] {
  
      /**
       * Creates a new `Set` with an additional element, unless the element is
       * already present.
       *
       * @param elem the element to be added
       * @return a new `Set` that contains all elements of this `Set` and that also
       * contains `elem`.
       */
      def +[U >: T <: E](elem: U): thisCollections.immutable.inhabited.Set[U]
  
      /**
       * Creates a new `Set` with additional elements.
       *
       * This method takes two or more elements to be added. Another overloaded
       * variant of this method handles the case where a single element is added.
       *
       * @param elem1 the first element to add.
       * @param elem2 the second element to add.
       * @param elems the remaining elements to add.
       * @return a new `Set` with the given elements added.
       */
      def +[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.inhabited.Set[U]
  
      /** Creates a new `Set` by adding all elements contained in another collection to this `Set`.
        *
        *  @param elems     the collection containing the added elements.
        *  @return          a new `Set` with the given elements added.
        */
      def ++[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.Set[U]
  
      /**
       * Creates a new `Set` by adding elements contained in another `Set`.
       *
       * @param that     the other `Set` containing the added elements.
       * @return         a new `Set` with the given elements added.
       */
      def ++[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.Set[U]
  
      /**
       * Creates a new `Set` with a given element removed from this `Set`.
       *
       * @param elem the element to be removed
       * @return a new `Set` that contains all elements of this `Set` but that does not
       * contain `elem`.
       */
      def -[U >: T <: E](elem: U): thisCollections.immutable.Set[U]
  
      /* * USE LATER
       * Creates a new `Set` from this `Set` by removing all elements of another
       * collection.
       *
       * @param xs the collection containing the removed elements.
       * @return a new `Set` that contains all elements of the current `Set`
       * except one less occurrence of each of the elements of `elems`.
       */
  
      /**
       * Creates a new `Set` from this `Set` with some elements removed.
       *
       * This method takes two or more elements to be removed. Another overloaded
       * variant of this method handles the case where a single element is
       * removed.
       * @param elem1 the first element to remove.
       * @param elem2 the second element to remove.
       * @param elems the remaining elements to remove.
       * @return a new `Set` that contains all elements of the current `Set`
       * except one less occurrence of each of the given elements.
       */
      def -[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.Set[U]
  
      /**
       * Creates a new `Set` from this `Set` by removing all elements of another
       *  collection.
       *
       *  @param elems     the collection containing the removed elements.
       *  @return a new `Set` that contains all elements of the current `Set`
       *  except one less occurrence of each of the elements of `elems`.
       */
      def --[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.Set[U]
  
      /**
       * Creates a new `Set` from this `Set` by removing all elements of another `Set`
       *
       * @param that       the other `Set` containing the removed elements.
       * @return a new `Set` that contains all elements of the current `Set` minus elements contained in the passed in `Set`.
       */
      def --[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.Set[U]
  
      /**
       * Appends all elements of this `Set` to a string builder.
       *  The written text consists of the string representations (w.r.t. the method
       * `toString`) of all elements of this `Set` without any separator string.
       *
       * Example:
       *
       * {{{
       *      scala> val a = List(1,2,3,4)
       *      a: List[Int] = List(1, 2, 3, 4)
       *
       *      scala> val b = new StringBuilder()
       *      b: StringBuilder =
       *
       *      scala> val h = a.addString(b)
       *      h: StringBuilder = 1234
       * }}}
       *
       *  @param  b    the string builder to which elements are appended.
       *  @return      the string builder `b` to which elements were appended.
       */
      def addString(b: StringBuilder): StringBuilder
  
      /**
       * Appends all elements of this `Set` to a string builder using a separator string.
       *  The written text consists of the string representations (w.r.t. the method `toString`)
       *  of all elements of this `Set`, separated by the string `sep`.
       *
       * Example:
       *
       * {{{
       *      scala> val a = List(1,2,3,4)
       *      a: List[Int] = List(1, 2, 3, 4)
       *
       *      scala> val b = new StringBuilder()
       *      b: StringBuilder =
       *
       *      scala> a.addString(b, ", ")
       *      res0: StringBuilder = 1, 2, 3, 4
       * }}}
       *
       *  @param  b    the string builder to which elements are appended.
       *  @param sep   the separator string.
       *  @return      the string builder `b` to which elements were appended.
       */
      def addString(b: StringBuilder, sep: String): StringBuilder
  
      /** Appends all elements of this `Set` to a string builder using start, end, and separator strings.
       *  The written text begins with the string `start` and ends with the string `end`.
       *  Inside, the string representations (w.r.t. the method `toString`)
       *  of all elements of this `Set` are separated by the string `sep`.
       *
       * Example:
       *
       * {{{
       *      scala> val a = List(1,2,3,4)
       *      a: List[Int] = List(1, 2, 3, 4)
       *
       *      scala> val b = new StringBuilder()
       *      b: StringBuilder =
       *
       *      scala> a.addString(b , "List(" , ", " , ")")
       *      res5: StringBuilder = List(1, 2, 3, 4)
       * }}}
       *
       *  @param  b    the string builder to which elements are appended.
       *  @param start the starting string.
       *  @param sep   the separator string.
       *  @param end   the ending string.
       *  @return      the string builder `b` to which elements were appended.
       */
      def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder
  
      /**
       * Aggregates the results of applying an operator to subsequent elements.
       *
       *  This is a more general form of `fold` and `reduce`. It has similar
       *  semantics, but does not require the result to be a supertype of the
       *  element type. It traverses the elements in different partitions
       *  sequentially, using `seqop` to update the result, and then applies
       *  `combop` to results from different partitions. The implementation of
       *  this operation may operate on an arbitrary number of collection
       *  partitions, so `combop` may be invoked an arbitrary number of times.
       *
       *  For example, one might want to process some elements and then produce
       *  a `Set`. In this case, `seqop` would process an element and append it
       *  to the list, while `combop` would concatenate two lists from different
       *  partitions together. The initial value `z` would be an empty set.
       *  {{{
       *    pc.aggregate(Set[Int]())(_ += process(_), _ ++ _)
       *  }}}
       *
       *  Another example is calculating geometric mean from a collection of doubles
       *  (one would typically require big doubles for this).
       *
       *  @tparam B        the type of accumulated results
       *  @param z         the initial value for the accumulated result of the partition - this
       *                   will typically be the neutral element for the `seqop` operator (e.g.
       *                   `Nil` for list concatenation or `0` for summation) and may be evaluated
       *                   more than once
       *  @param seqop     an operator used to accumulate results within a partition
       *  @param combop    an associative operator used to combine results from different partitions
       */
      def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B
  
      def canEqual(that: Any): Boolean
  
      def contains[U >: T <: E](elem: U): Boolean
  
      /**
       * Copies values of this `Set` to an array.
       * Fills the given array `xs` with values of this `Set`.
       * Copying will stop once either the end of the current `Set` is reached,
       * or the end of the array is reached.
       *
       * @param xs the array to fill.
       *
       */
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]]): Unit
  
      /**
       * Copies values of this `Set` to an array.
       * Fills the given array `xs` with values of this `Set`, beginning at index `start`.
       * Copying will stop once either the end of the current `Set` is reached,
       * or the end of the array is reached.
       *
       * @param xs the array to fill.
       * @param start the starting index.
       *
       */
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int): Unit
  
      /**
       * Copies values of this `Set` to an array.
       * Fills the given array `xs` with values of this `Set`, beginning at index `start`.
       * Copying will stop once the count of element copied reach <code>len</code>.
       *
       * @param xs the array to fill.
       * @param start the starting index.
       * @param len the length of elements to copy
       *
       */
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int, len: Int): Unit
  
      /**
       * Copies all elements of this `Set` to a buffer.
       *
       * @param dest The buffer to which elements are copied.
       */
      def copyToBuffer[U >: T <: E](dest: mutable.Buffer[thisCollections.Box[U]]): Unit
  
      /**
       * Counts the number of elements in the `Set` which satisfy a predicate.
       *
       * @param p the predicate used to test elements.
       * @return the number of elements satisfying the predicate `p`.
       */
      def count(p: T => Boolean): Int
  
      /**
       * Computes the difference of this `Set` and another `Set`.
       *
       * @param that the `Set` of elements to exclude.
       * @return an `Set` containing those elements of this
       * `Set` that are not also contained in the given `Set` `that`.
       */
      def diff[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.Set[U]
  
      /**
       * Selects all elements except first ''n'' ones.
       *
       * @param n the number of elements to drop from this `Set`.
       * @return an `Set` consisting of all elements of this `Set` except the first `n` ones, or else the
       * empty `Set`, if this `Set` has less than `n` elements.
       */
      def drop(n: Int): thisCollections.immutable.Set[T]
  
      /**
       * Selects all elements except last ''n'' ones.
       *
       * @param n The number of elements to take
       * @return an `Set` consisting of all elements of this `Set` except the last `n` ones, or else the
       * empty `Set`, if this `Set` has less than `n` elements.
       */
      def dropRight(n: Int): thisCollections.immutable.Set[T]
  
      /**
       * Drops longest prefix of elements that satisfy a predicate.
       *
       * @param pred The predicate used to test elements.
       * @return the longest suffix of this `Set` whose first element
       * does not satisfy the predicate `p`.
       */
      def dropWhile(pred: T => Boolean): thisCollections.immutable.Set[T]
  
      /**
       * Check if this `Set` contains element which satisfy a predicate.
       *
       * @param pred predicate predicate used to test elements
       * @return <code>true</code> if there's at least one element satisfy the given predicate <code>pred</code>
       */
      def exists(pred: T => Boolean): Boolean
  
      /**
       * Selects all elements of this `Set` which satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `Set` consisting of all elements of this `Set` that satisfy the given
       * predicate <code>pred</code>. Their order may not be preserved.
       */
      def filter(pred: T => Boolean): thisCollections.immutable.Set[T]
  
      /**
       * Selects all elements of this `Collections` which do not satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `Collections` consisting of all elements of this `Collections` that do not satisfy the given
       * predicate <code>pred</code>. Their order may not be preserved.
       */
      def filterNot(pred: T => Boolean): thisCollections.immutable.Set[T]
  
      /**
       * Finds the first element of the `Set` satisfying a predicate, if any.
       *
       *
       * @param pred the predicate used to test elements.
       * @return an option value containing the first element in the `Set`
       * that satisfies <code>pred</code>, or <code>None</code> if none exists.
       */
      def find(pred: T => Boolean): Option[T]

      def flatMap[U](f: T => GenIterableOnce[U]): SetView[U]
  
      /**
       * Converts this `Set` of `Set` into
       * an `Set` formed by the elements of these `Set`.
       *
       *
       * @return a new `Set` resulting from concatenating all element `Set`s.
       *
       * The resulting collection's type will be guided by the
       * static type of `Set`. For example:
       *
       * {{{
       *
       * val ys = `Set`(
       * `Set`(1, 2, 3),
       * `Set`(3, 2, 1)
       * ).flatten
       * // ys == `Set`(1, 2, 3)
       * }}}
       */
      //def flatten[S, T >: Collections[S]#Set]: Collections[S]#Set
  
      /**
       * Folds the elements of this `Set` using the specified associative
       * binary operator.
       *
       * @tparam T1 a type parameter for the binary operator, a supertype of `A`.
       * @param z a neutral element for the fold operation; may be added to the result
       * an arbitrary number of times, and must not change the result (e.g., `Nil` for list concatenation,
       * 0 for addition, or 1 for multiplication.)
       * @param op a binary operator that must be associative
       * @return the result of applying fold operator `op` between all the elements and `z`
       */
      def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1
  
      /**
       * Applies a binary operator to a start value and all elements of this `Set`,
       * going left to right.
       *
       *  Examples:
       *
       *  Note that the folding function used to compute b is equivalent to that used to compute c.
       *  {{{
       *      scala> val a = List(1, 2, 3, 4)
       *      a: List[Int] = List(1,2,3,4)
       *
       *      scala> val b = a.foldLeft(5)(_+_)
       *      b: Int = 15
       *
       *      scala> val c = a.foldLeft(5)((x,y) => x + y)
       *      c: Int = 15
       *  }}}
       *
       * @param z the start value.
       * @param op the binary operator.
       * @tparam B the result type of the binary operator.
       * @return the result of inserting `op` between consecutive elements of this `Set`,
       * going left to right with the start value `z` on the left:
       * {{{
       * op(...op(z, x_1), x_2, ..., x_n)
       * }}}
       * where `x,,1,,, ..., x,,n,,` are the elements of this `Set`.
       */
      def foldLeft[B](z: B)(op: (B, T) => B): B
  
      /**
       * Applies a binary operator to all elements of this `Set` and a start value,
       * going right to left.
       *
       *  Examples:
       *
       *  Note that the folding function used to compute b is equivalent to that used to compute c.
       *  {{{
       *      scala> val a = List(1, 2, 3, 4)
       *      a: List[Int] = List(1,2,3,4)
       *
       *      scala> val b = a.foldRight(5)(_+_)
       *      b: Int = 15
       *
       *      scala> val c = a.foldRight(5)((x,y) => x + y)
       *      c: Int = 15
       *
       *  }}}
       *
       * @param z the start value.
       * @param op the binary operator.
       * @tparam B the result type of the binary operator.
       * @return the result of inserting `op` between consecutive elements of this `Set`,
       * going right to left with the start value `z` on the right:
       * {{{
       * op(x_1, op(x_2, ... op(x_n, z)...))
       * }}}
       * where `x,,1,,, ..., x,,n,,` are the elements of this `Set`.
       */
      def foldRight[B](z: B)(op: (T, B) => B): B
  
      /**
       * Check if all elements in this `Set` satisfy the predicate.
       *
       * @param pred the predicate to check for
       * @return <code>true</code> if all elements satisfy the predicate, <code>false</code> otherwise.
       */
      def forall(pred: T => Boolean): Boolean
  
      /**
       * Applies a function `f` to all elements of this `Set`.
       *
       * @param f the function that is applied for its side-effect to every element.
       * The result of function `f` is discarded.
       *
       * @tparam U the type parameter describing the result of function `f`.
       * This result will always be ignored. Typically `U` is `Unit`,
       * but this is not necessary.
       *
       */
      def foreach[U](f: T => U): Unit
  
      /**
       * Partitions this `Set` into a map of `Set`s according to some discriminator function.
       *
       * Note: this method is not re-implemented by views. This means
       * when applied to a view it will always force the view and
       * return a new `Set`.
       *
       * @param f the discriminator function.
       * @tparam K the type of keys returned by the discriminator function.
       * @return A map from keys to `Set`s such that the following invariant holds:
       * {{{
       * (xs groupBy f)(k) = xs filter (x => f(x) == k)
       * }}}
       * That is, every key `k` is bound to an `Set` of those elements `x`
       * for which `f(x)` equals `k`.
       *
       */
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.Set[T]]
  
      /**
       * Partitions elements in fixed size `Set`s.
       * @see [[scala.collection.Iterator]], method `grouped`
       *
       * @param size the number of elements per group
       * @return An iterator producing `Set`s of size `size`, except the
       * last will be less than size `size` if the elements don't divide evenly.
       */
      def grouped(size: Int): Iterator[thisCollections.immutable.Set[T]]
  
      def hasDefiniteSize: Boolean
  
      /** Optionally selects the first element.
        *
        * @return the first element of this `Set` if it is nonempty,
        * `None` if it is empty.
        */
      def headOption: Option[T]
  
      /**
       * Selects all elements except the last.
       *
       * @return an `Set` consisting of all elements of this `Set`
       * except the last one.
       * @throws `UnsupportedOperationException` if the `Set` is empty.
       */
      def init: thisCollections.immutable.Set[T]
  
      /**
       * Iterates over the inits of this `Set`. The first value will be this
       * `Set` and the final one will be an empty `Set`, with the intervening
       * values the results of successive applications of `init`.
       *
       * @return an iterator over all the inits of this `Set`
       * @example Set(1,2,3).inits = Iterator(Set(1,2,3), Set(1,2), Set(1), Set())
       */
      def inits: Iterator[thisCollections.immutable.Set[T]]
  
      /**
       * Computes the intersection between this `Set` and another `Set`.
       *
       * @param that the `Set` to intersect with.
       * @return a new `Set` consisting of all elements that are both in this
       * `Set` and in the given `Set` `that`.
       */
      def intersect[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.Set[U]

      /**
       * Tests if this `Set` is empty.
       *
       * @return `true` if there is no element in the set, `false` otherwise.
       */
      def isEmpty: Boolean
  
      /**
       * Tests whether this `Set` can be repeatedly traversed. Always
       * true for `Set` unless overridden.
       *
       * @return `true` unless overriden.
       */
      def isTraversableAgain: Boolean = true
  
      /**
       * Get an instance of `Iterator` for elements of this `Set`.
       *
       * @return an instance of `Iterator` for elements of this `Set`
       */
      def iterator: org.scalactic.iterators.Iterator[T]
  
      /**
       * Selects the last element.
       *
       * @return The last element of this `Set`.
       * @throws NoSuchElementException If the `Set` is empty.
       */
      def last: T
  
      /**
       * Optionally selects the last element.
       *
       * @return the last element of this `Set` if it is nonempty,
       * `None` if it is empty.
       */
      def lastOption: Option[T]
  
      def map[U](f: T => U): SetView[U]

      /**
       * Finds the largest element.
        *
        * @param ord An ordering to be used for comparing elements.
        * @tparam T1 The type over which the ordering is defined.
        * @return the largest element of this `Set` with respect to the ordering `ord`.
        *
        * @return the largest element of this `Set`.
        */
      def max[T1 >: T](implicit ord: Ordering[T1]): T
  
      /**
       * Finds the first element which yields the largest value measured by function f.
       *
       * @param cmp An ordering to be used for comparing elements.
       * @tparam B The result type of the function f.
       * @param f The measuring function.
       * @return the first element of this `Set` with the largest value measured by function f
       * with respect to the ordering `cmp`.
       *
       * @return the first element of this `Set` with the largest value measured by function f.
       */
      def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T
  
      def membership[U >: T <: E]: Membership[U]

      /**
       * Finds the smallest element.
       *
       * @param ord An ordering to be used for comparing elements.
       * @tparam T1 The type over which the ordering is defined.
       * @return the smallest element of this `Set` with respect to the ordering `ord`.
       *
       * @return the smallest element of this `Set`
       */
      def min[T1 >: T](implicit ord: Ordering[T1]): T
  
      /**
       * Finds the first element which yields the smallest value measured by function f.
       *
       * @param cmp An ordering to be used for comparing elements.
       * @tparam B The result type of the function f.
       * @param f The measuring function.
       * @return the first element of this `Set` with the smallest value measured by function f
       * with respect to the ordering `cmp`.
       *
       * @return the first element of this `Set` with the smallest value measured by function f.
       */
      def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T
  
      /**
       * Displays all elements of this `Set` in a string using start, end, and
       * separator strings.
       *
       * @param start the starting string.
       * @param sep the separator string.
       * @param end the ending string.
       * @return a string representation of this `Set`. The resulting string
       * begins with the string `start` and ends with the string
       * `end`. Inside, the string representations (w.r.t. the method
       * `toString`) of all elements of this `Set` are separated by
       * the string `sep`.
       *
       * @example `Set(1, 2, 3).mkString("(", "; ", ")") = "(1; 2; 3)"`
       */
      def mkString(start: String, sep: String, end: String): String
      /**
       * Displays all elements of this `Set` in a string using a separator string.
       *
       * @param sep the separator string.
       * @return a string representation of this `Set`. In the resulting string
       * the string representations (w.r.t. the method `toString`)
       * of all elements of this `Set` are separated by the string `sep`.
       *
       * @example `Set(1, 2, 3).mkString("|") = "1|2|3"`
       */
      def mkString(sep: String): String
      /**
       * Displays all elements of this `Set` in a string.
       *
       * @return a string representation of this `Set`. In the resulting string
       * the string representations (w.r.t. the method `toString`)
       * of all elements of this `Set` follow each other without any
       * separator string.
       */
      def mkString: String
  
      /** Tests whether the `Set` is not empty.
        *
        * @return `true` if the `Set` contains at least one element, `false` otherwise.
        */
      def nonEmpty: Boolean
  
      /**
       * Partitions this `Set` in two `Set`s according to a predicate.
       *
       * @param pred the predicate on which to partition.
       * @return a pair of `Set`s: the first `Set` consists of all elements that
       * satisfy the predicate `p` and the second `Set` consists of all elements
       * that don't. The relative order of the elements in the resulting `Set`s
       * may not be preserved.
       */
      def partition(pred: T => Boolean): (thisCollections.immutable.Set[T], thisCollections.immutable.Set[T])
  
      /**
       * Multiplies up the elements of this collection.
       *
       * @param num an implicit parameter defining a set of numeric operations
       * which includes the `*` operator to be used in forming the product.
       * @tparam T1 the result type of the `*` operator.
       * @return the product of all elements of this `Set` with respect to the `*` operator in `num`.
       *
       * @return the product of all elements in this `Set` of numbers of type `Int`.
       * Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
       * can be used as element type of the `Set` and as result type of `product`.
       * Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
       */
      def product[T1 >: T](implicit num: Numeric[T1]): T1
  
      /**
       * Reduces the elements of this `Set` using the specified associative binary operator.
       *
       * @tparam T1 A type parameter for the binary operator, a supertype of `T`.
       * @param op A binary operator that must be associative.
       * @return The result of applying reduce operator `op` between all the elements if the `Set` is nonempty.
       * @throws UnsupportedOperationException
       * if this `Set` is empty.
       */
      def reduce[T1 >: T](op: (T1, T1) => T1): T1
  
      /**
       * Applies a binary operator to all elements of this `Set`,
       * going left to right.
       *
       * @param op the binary operator.
       * @tparam T1 the result type of the binary operator.
       * @return the result of inserting `op` between consecutive elements of this `Set`,
       * going left to right:
       * {{{
       * op( op( ... op(x_1, x_2) ..., x_{n-1}), x_n)
       * }}}
       * where `x,,1,,, ..., x,,n,,` are the elements of this `Set`.
       * @throws `UnsupportedOperationException` if this `Set` is empty. */
      def reduceLeft[T1 >: T](op: (T1, T) => T1): T1
  
      /**
       * Optionally applies a binary operator to all elements of this `Set`, going left to right.
       *
       * @param op the binary operator.
       * @tparam T1 the result type of the binary operator.
       * @return an option value containing the result of `reduceLeft(op)` is this `Set` is nonempty,
       * `None` otherwise.
       */
      def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1]
  
      /**
       * Reduces the elements of this `Set`, if any, using the specified
       * associative binary operator.
       *
       * @tparam T1 A type parameter for the binary operator, a supertype of `T`.
       * @param op A binary operator that must be associative.
       * @return An option value containing result of applying reduce operator `op` between all
       * the elements if the collection is nonempty, and `None` otherwise.
       */
      def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1]
  
      /**
       * Applies a binary operator to all elements of this `Set`, going right to left.
       *
       * @param op the binary operator.
       * @tparam T1 the result type of the binary operator.
       * @return the result of inserting `op` between consecutive elements of this `Set`,
       * going right to left:
       * {{{
       * op(x_1, op(x_2, ..., op(x_{n-1}, x_n)...))
       * }}}
       * where `x,,1,,, ..., x,,n,,` are the elements of this `Set`.
       * @throws `UnsupportedOperationException` if this `Set` is empty.
       */
      def reduceRight[T1 >: T](op: (T, T1) => T1): T1
  
      /**
       * Optionally applies a binary operator to all elements of this `Set`, going
       * right to left.
       *
       * @param op the binary operator.
       * @tparam T1 the result type of the binary operator.
       * @return an option value containing the result of `reduceRight(op)` is this `Set` is nonempty,
       * `None` otherwise.
       */
      def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1]
  
      /**
       * Checks if the other iterable collection contains the same elements in the same order as this `Set`.
       *
       * @param that the collection to compare with.
       * @tparam T1 the type of the elements of collection `that`.
       * @return `true`, if both collections contain the same elements in the same order, `false` otherwise.
       */
      def sameElements[T1 >: T](that: GenIterable[T1]): Boolean
  
      /**
       * The size of this `Set`.
       *
       * @return the number of elements in this `Set`.
       */
      def size: Int
  
      /**
       * Selects an interval of elements. The returned collection is made up
       * of all elements `x` which satisfy the invariant:
       * {{{
       * from <= indexOf(x) < until
       * }}}
       *
       * @param unc_from the lowest index to include from this `Set`.
       * @param unc_until the lowest index to EXCLUDE from this `Set`.
       * @return an `Set` containing the elements greater than or equal to
       * index `from` extending up to (but not including) index `until`
       * of this `Set`.
       */
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.Set[T]
  
      /**
       * Groups elements in fixed size blocks by passing a "sliding window"
       * over them (as opposed to partitioning them, as is done in grouped.)
       * @see [[scala.collection.Iterator]], method `sliding`
       *
       * @param size the number of elements per group
       * @return An iterator producing `SortedSet`s of size `size`, except the
       * last and the only element will be truncated if there are
       * fewer elements than size.
       */
      def sliding(size: Int): Iterator[thisCollections.immutable.Set[T]]
  
      /**
       * Groups elements in fixed size blocks by passing a "sliding window"
       * over them (as opposed to partitioning them, as is done in grouped.)
       * @see [[scala.collection.Iterator]], method `sliding`
       *
       * @param size the number of elements per group
       * @param step the distance between the first elements of successive
       * groups (defaults to 1)
       * @return An iterator producing `Set`s of size `size`, except the
       * last and the only element will be truncated if there are
       * fewer elements than size.
       */
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.Set[T]]
  
      /**
       * Splits this `Set` into a prefix/suffix pair according to a predicate.
       *
       * Note: `c span p` is equivalent to (but possibly more efficient than)
       * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
       * predicate `p` does not cause any side-effects.
       *
       *
       * @param pred the test predicate
       * @return a pair consisting of the longest prefix of this `Set` whose
       * elements all satisfy `p`, and the rest of this `Set`.
       */
      def span(pred: T => Boolean): (thisCollections.immutable.Set[T], thisCollections.immutable.Set[T])
  
      /**
       * Splits this `Set` into two at a given position.
       * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
       * `(c take n, c drop n)`.
       *
       *
       * @param n the position at which to split.
       * @return a pair of `Set`s consisting of the first `n`
       * elements of this `Set`, and the other elements.
       */
      def splitAt(n: Int): (thisCollections.immutable.Set[T], thisCollections.immutable.Set[T])
  
      /**
       * Defines the prefix of this object's `toString` representation.
       *
       * @return a string representation which starts the result of `toString`
       * applied to this `Set`. By default the string prefix is the
       * simple name of the collection class `Set`.
       */
      def stringPrefix: String
  
      /**
       * Tests whether this set is a subset of another set.
       *
       * @param that the set to test.
       * @return `true` if this set is a subset of `that`, i.e. if
       * every element of this set is also an element of `that`.
       */
      def subsetOf[U >: T <: E](that: thisCollections.immutable.Set[U]): Boolean
  
      /**
       * An iterator over all subsets of this set of the given size.
       * If the requested size is impossible, an empty iterator is returned.
       *
       * @param len the size of the subsets.
       * @return the iterator.
       */
      def subsets(len: Int): Iterator[thisCollections.immutable.Set[T]]
  
      /**
       * An iterator over all subsets of this set.
       *
       * @return the iterator.
       */
      def subsets: Iterator[thisCollections.immutable.Set[T]]
  
      /** Sums up the elements of this collection.
        *
        * @param num an implicit parameter defining a set of numeric operations
        * which includes the `+` operator to be used in forming the sum.
        * @tparam T1 the result type of the `+` operator.
        * @return the sum of all elements of this `Set` with respect to the `+` operator in `num`.
        *
        * @return the sum of all elements in this `Set` of numbers of type `Int`.
        * Instead of `Int`, any other type `T` with an implicit `Numeric[T]` implementation
        * can be used as element type of the `Set` and as result type of `sum`.
        * Examples of such types are: `Long`, `Float`, `Double`, `BigInt`.
        *
        */
      def sum[T1 >: T](implicit num: Numeric[T1]): T1
  
      /**
       * Selects all elements except the first.
       *
       * @return an `Set` consisting of all elements of this `Set`
       * except the first one.
       * @throws `UnsupportedOperationException` if the `Set` is empty.
       */
      def tail: thisCollections.immutable.Set[T]
  
      /**
       * Iterates over the tails of this `Set`. The first value will be this
       * `Set` and the final one will be an empty `Set`, with the intervening
       * values the results of successive applications of `tail`.
       *
       * @return an iterator over all the tails of this `Set`
       * @example `Set(1,2,3).tails = Iterator(Set(1,2,3), Set(2,3), Set(3), Set())`
       */
      def tails: Iterator[thisCollections.immutable.Set[T]]
  
      /**
       * Selects first ''n'' elements.
       *
       * @param n the number of elements to take from this `Set`.
       * @return an `Set` consisting only of the first `n` elements of this `Set`,
       * or else the whole `Set`, if it has less than `n` elements.
       */
      def take(n: Int): thisCollections.immutable.Set[T]
  
      /**
       * Selects last ''n'' elements.
       *
       *
       * @param n the number of elements to take
       * @return an `Set` consisting only of the last `n` elements of this `Set`, or else the
       * whole `Set`, if it has less than `n` elements.
       */
      def takeRight(n: Int): thisCollections.immutable.Set[T]
  
      /**
       * Converts this `Set` to an array.
       *
       * @return an array containing all elements of this `Set[T]`.
       */
      def toArray[U >: T <: E](implicit ct: ClassTag[U]): Array[U]
  
      /**
       * Converts this `Set` to an array of `Box[T]`es containing the elements.
       *
       * @return an array containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxArray[U >: T <: E]: Array[thisCollections.Box[U]]
  
      /**
       * Uses the contents of this `Set` to create a new mutable buffer.
       *
       * @return a buffer containing all elements of this `Set`.
       */
      def toStandardBuffer[U >: T <: E]: scala.collection.mutable.Buffer[U]
  
      /**
       * Uses the contents of this `Set` to create a new mutable buffer containing `Box`es of elements.
       *
       * @return a buffer containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardBuffer[U >: T <: E]: scala.collection.mutable.Buffer[thisCollections.Box[U]]
  
      /**
       * Converts this `Set` to an indexed sequence.
       *
       * @return an indexed sequence containing all elements of this `Set`.
       */
      def toStandardIndexedSeq: scala.collection.immutable.IndexedSeq[T]
  
      /**
       * Converts this `Set` to an indexed sequence containing `Box`es of elements.
       *
       * @return an indexed sequence containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardIndexedSeq: scala.collection.immutable.IndexedSeq[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to an iterable collection. Note that
       * the choice of target `Iterable` is lazy in this default implementation
       * as this `TraversableOnce` may be lazy and unevaluated (i.e. it may
       * be an iterator which is only traversable once).
       *
       * @return an `Iterable` containing all elements of this `Set`.
       */
      def toStandardIterable: GenIterable[T]
  
      /**
       * Converts this `Set` to an iterable collection of `Box`es containing the elements. Note that
       * the choice of target `Iterable` is lazy in this default implementation as this `TraversableOnce` may
       * be lazy and unevaluated (i.e. it may be an iterator which is only traversable once).
       *
       * @return an `Iterable` containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardIterable: GenIterable[thisCollections.Box[T]]
  
      /**
       * Returns an Iterator over the elements in this `Set`.  Will return
       * the same Iterator if this instance is already an Iterator.
       *
       * @return an Iterator containing all elements of this  `Set`.
       */
      def toStandardIterator: scala.collection.Iterator[T]
  
      /**
       * Returns an Iterator over the `Box`es in this `Set`.  Will return
       * the same Iterator if this instance is already an Iterator.
       *
       * @return an Iterator containing all elements of this  `Set`, boxed in `Box`.
       */
      def toBoxStandardIterator: scala.collection.Iterator[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to a list of `Box`.
       *
       * @return a list containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardList: List[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to a list.
       *
       * @return a list containing all elements of this `Set`.
       */
      def toStandardList: List[T]
  
      /**
       * Converts this `Set` to a map. This method is unavailable unless
       * the elements are members of Tuple2, each ((K, V)) becoming a key-value
       * pair in the map. Duplicate keys will be overwritten by later keys:
       * if this is an unordered `Set`, which key is in the resulting map
       * is undefined.
       * @return a map containing all elements of this `Set`.
       *
       * @return a map of type `immutable.Map[K, V]`
       * containing all key/value pairs of type `(K, V)` of this `Set`.
       */
      def toStandardMap[K, V](implicit ev: T <:< (K, V)): Map[K, V]
  
      /**
       * Converts this `Set` to a `ParArray`.
       *
       * @return a `ParArray` containing all elements of this `Set`.
       */
      def toStandardParArray[U >: T <: E]: ParArray[U]
  
      /**
       * Converts this `Set` to a `ParArray` containing `Box`es of elements.
       *
       * @return a `ParArray` containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardParArray[U >: T <: E]: ParArray[thisCollections.Box[U]]
  
      /**
       * Converts this `Set` to a sequence. As with `toIterable`, it's lazy
       * in this default implementation, as this `TraversableOnce` may be
       * lazy and unevaluated.
       *
       * @return a sequence containing all elements of this `Set`.
       */
      def toStandardSeq: GenSeq[T]
  
      /**
       * Converts this `Set` to a sequence containing `Box`es of elements.
       * As with `toIterable`, it's lazy in this default implementation, as this
       * `TraversableOnce` may be lazy and unevaluated.
       *
       * @return a sequence containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardSeq: GenSeq[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to a set.
       *
       * @return a set containing all elements of this `Set`.
       */
      def toStandardSet[U >: T <: E]: scala.collection.immutable.Set[U]
  
      /**
       * Converts this `Set` to a set of `Box`.
       *
       * @return a set containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardSet[U >: T <: E]: scala.collection.immutable.Set[thisCollections.Box[U]]
  
      /**
       * Converts this `Set` to a stream.
       *
       * @return a stream containing all elements of this `Set`.
       */
      def toStandardStream: Stream[T]
  
      /**
       * Converts this `Set` to a stream of `Box`es containing the elements.
       *
       * @return a stream containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardStream: Stream[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to a `Traversable`.
       *
       * @return a Traversable containing all elements of this `Set`.
       */
      def toStandardTraversable: GenTraversable[T]
  
      /**
       * Converts this `Set` to a `Traversable` of `Box`es containing the elements.
       *
       * @return a Traversable containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardTraversable: GenTraversable[thisCollections.Box[T]]
  
      /**
       * Converts this `Set` to a Vector.
       *
       * @return a vector containing all elements of this `Set`.
       */
      def toStandardVector: Vector[T]
  
      /**
       * Converts this `Set` to a Vector of `Box`es containing the elements.
       *
       * @return a vector containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardVector: Vector[thisCollections.Box[T]]
  
      /**
       * Transposes this `Set` of traversable collections into
       * an `Set` of `Set`s.
       *
       * The resulting collection's type will be guided by the
       * static type of `Set`. For example:
       *
       * {{{
       * val xs = Set(
       * Set(1, 2, 3),
       * Set(4, 5, 6)).transpose
       * // xs == List(
       * // List(1, 4),
       * // List(2, 5),
       * // List(3, 6))
       *
       * val ys = Vector(
       * List(1, 2, 3),
       * List(4, 5, 6)).transpose
       * // ys == Vector(
       * // Vector(1, 4),
       * // Vector(2, 5),
       * // Vector(3, 6))
       * }}}
       *
       * @tparam B the type of the elements of each traversable collection.
       * @param asTraversable an implicit conversion which asserts that the
       * element type of this `Set` is a `Traversable`.
       * @return a two-dimensional `Set` of ${coll}s which has as ''n''th row
       * the ''n''th column of this `Set`.
       * @throws `IllegalArgumentException` if all collections in this `Set`
       * are not of the same size.
       */
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.Set[T]
  
      /**
       * Computes the union between of set and another set.
       *
       * @param that the set to form the union with.
       * @return a new set consisting of all elements that are in this
       * set or in the given set `that`.
       */
      def union[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.Set[U]
  
      /**
       * Creates a non-strict filter of this `Set`.
       *
       * Note: the difference between `c filter p` and `c withFilter p` is that
       * the former creates a new `Set`, whereas the latter only
       * restricts the domain of subsequent `map`, `flatMap`, `foreach`,
       * and `withFilter` operations.
       *
       * @param p the predicate used to test elements.
       * @return an object of class `FilterMonadic`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `Set`
       * which satisfy the predicate `p`.
       */
      def withFilter(p: T => Boolean): WithFilter = new WithFilter(p)
  
      /**
       * A class supporting filtered operations. Instances of this class are
       * returned by method `withFilter`.
       */
      class WithFilter(p: T=> Boolean) {
  
        /**
         * Applies a function `f` to all elements of the outer `Set` containing
         * this `WithFilter` instance that satisfy predicate `p`.
         *
         * @param f the function that is applied for its side-effect to every element.
         * The result of function `f` is discarded.
         *
         * @tparam U the type parameter describing the result of function `f`.
         * This result will always be ignored. Typically `U` is `Unit`,
         * but this is not necessary.
         *
         */
        def foreach[U](f: T => U): Unit =
          filter(p).foreach(f)
        // TODO: Why is there a U here? Shouldn't it be Unit?
  
        /**
         * Further refines the filter for this `Set`.
         *
         * @param q the predicate used to test elements.
         * @return an object of class `WithFilter`, which supports
         * `map`, `flatMap`, `foreach`, and `withFilter` operations.
         * All these operations apply to those elements of this `Set` which
         * satisfy the predicate `q` in addition to the predicate `p`.
         */
        def withFilter(q: T => Boolean): WithFilter =
          new WithFilter(x => p(x) && q(x))
      }
  
      val path: thisCollections.type
  
      // def copyInto(thatCollections: Collections[T]): thatCollections.Set
  
      def view: SetView[T]
    }
  /*
    trait EquaMap[V]/* extends Function[T, V] with Equals*/ {
  
      /**
       * Add a key/value pair to this `EquaMap`, returning a new `EquaMap`.
       * @param kv the key/value pair.
       * @return A new `EquaMap` with the new binding added to this `EquaMap`.
       */
      def +[V1 >: V](kv: (T, V1)): EquaMap[V1]
  
      /**
       * Creates a new `EquaMap` with additional entries.
       *
       * This method takes two or more entries to be added. Another overloaded
       * variant of this method handles the case where a single entry is added.
       *
       * @param entry1 the first entry to add.
       * @param entry2 the second entry to add.
       * @param entries the remaining entries to add.
       * @return a new `EquaMap` with the given entries added.
       */
      def +[V1 >: V](entry1: (T, V1), entry2: (T, V1), entries: (T, V1)*): thisCollections.immutable.EquaMap[V1]
  
      /**
       * Creates a new `EquaMap` by adding all elements contained in another collection to this `Set`.
       *
       *  @param entries     the collection containing the added elements.
       *  @return          a new `Set` with the given elements added.
       */
      def ++[V1 >: V](entries: GenTraversableOnce[(T, V1)]): thisCollections.immutable.EquaMap[V1]
  
      /**
       * Creates a new `EquaMap` by adding entries contained in another `EquaMap`.
       *
       * @param that     the other `EquaMap` containing the added entries.
       * @return         a new `EquaMap` with the given entries added.
       */
      def ++[V1 >: V](that: EquaMap[V1]): thisCollections.immutable.EquaMap[V1]
  
      /**
       * Creates a new `EquaMap` with entry with given key removed from this `EquaMap`.
       *
       * @param elem the key for entry to be removed
       * @return a new `EquaMap` that contains all elements of this `EquaMap` but that does not
       * contain entry with `key`.
       */
      def -(key: T): thisCollections.immutable.EquaMap[V]
  
      /**
       * Creates a new `EquaMap` from this `EquaMap` with some elements removed.
       *
       * This method takes two or more key of entries to be removed. Another overloaded
       * variant of this method handles the case where a single key of entry is
       * removed.
       * @param key1 the first key of entry to remove.
       * @param key2 the second key of entry to remove.
       * @param keys the remaining keys of entries to remove.
       * @return a new `EquaMap` that contains all elements of the current `EquaMap`
       * except entries with the given keys.
       */
      def -(key1: T, key2: T, keys: T*): thisCollections.immutable.EquaMap[V]
  
      /**
       * Creates a new `EquaMap` from this `EquaMap` by removing all entries of another
       *  collection.
       *
       *  @param keys     the collection containing the keys of entries to remove.
       *  @return a new `EquaMap` that contains all entries of the current `EquaMap`
       *  except those with key contained in the given collection.
       */
      def --(keys: GenTraversableOnce[T]): thisCollections.immutable.EquaMap[V]
  
      /**
       * Creates a new `EquaMap` from this `EquaMap` by removing all entries with keys specified by the given `Set`
       *
       * @param equaSet       the `Set` containing the keys of entries to be removed.
       * @return a new `EquaMap` that contains all entries of the current `EquaMap` minus entries with keys contained in the passed in `Set`.
       */
      def --(equaSet: thisCollections.immutable.Set): thisCollections.immutable.EquaMap[V]
  
      /**
       * Applies a binary operator to a start value and all entries of this `EquaMap`,
       *  going left to right.
       *
       *  Note: `/:` is alternate syntax for `foldLeft`; `z /: xs` is the same as
       *  `xs foldLeft z`.
       *
       *  Examples:
       *
       *  Note that the folding function used to compute b is equivalent to that used to compute c.
       *  {{{
       *      scala> val a = List(1,2,3,4)
       *      a: List[Int] = List(1, 2, 3, 4)
       *
       *      scala> val b = (5 /: a)(_+_)
       *      b: Int = 15
       *
       *      scala> val c = (5 /: a)((x,y) => x + y)
       *      c: Int = 15
       *  }}}
       *
       *  $willNotTerminateInf
       *  $orderDependentFold
       *
       *  @param   z    the start value.
       *  @param   op   the binary operator.
       *  @tparam  R    the result type of the binary operator.
       *  @return  the result of inserting `op` between consecutive elements of this `EquaMap`,
       *           going left to right with the start value `z` on the left:
       *           {{{
       *             op(...op(op(z, x_1), x_2), ..., x_n)
       *           }}}
       *           where `x,,1,,, ..., x,,n,,` are the elements of this `EquaMap`.
       */
      def /:[R](z: R)(op: (R, (T, V)) => R): R
  
      /**
       * Tests if this `EquaMap` is empty.
       *
       * @return `true` if there is no element in the set, `false` otherwise.
       */
      def isEmpty: Boolean
  
      /**
       * Get an instance of `Iterator` for keys of this `EquaMap`.
       *
       * @return an instance of `Iterator` for keys of this `EquaMap`
       */
      def keysIterator: Iterator[T]
  
      /**
       * Get an instance of `Iterator` for values of this `EquaMap`.
       *
       * @return an instance of `Iterator` for values of this `EquaMap`
       */
      def valuesIterator: Iterator[V]
  
      /**
       * The size of this `EquaMap`.
       *
       * @return the number of elements in this `EquaMap`.
       */
      def size: Int
  
      /**
       * Converts this `EquaMap` to a `Map`.
       *
       * @return a `Map` containing all entries of this `EquaMap`.
       */
      def toStandardMap: Map[T, V]
  
      /**
       * Converts this `Set` to a set of `Box`.
       *
       * @return a set containing all elements of this `Set`, boxed in `Box`.
       */
      def toBoxStandardMap: Map[thisCollections.Box[T], V]
  
      val path: thisCollections.type
    }
  */

    sealed class FastSet[+T <: E] private[scalactic] (private val underlying: scala.collection.immutable.Set[Box[T@uV]]) extends Set[T] { thisFastSet =>
      def +[U >: T <: E](elem: U): thisCollections.immutable.inhabited.FastSet[U] = new immutable.inhabited.FastSet[U](underlying.map(ebt => (ebt: Box[U])) + Box[U](elem))
      def +[U >: T <: E](elem1: U, elem2: U, elem3: U*): thisCollections.immutable.inhabited.FastSet[U] =
        new immutable.inhabited.FastSet[U](underlying.map(ebt => (ebt: Box[U])) + (Box[U](elem1), Box[U](elem2), elem3.map(Box[U](_)): _*))
      def ++[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying ++ elems.toList.map(Box[U](_)))
      def ++[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.FastSet[U] = new immutable.FastSet[U](underlying ++ that.toBoxStandardSet)
      def -[U >: T <: E](elem: U): thisCollections.immutable.FastSet[U] = new immutable.FastSet[U](underlying.map(ebt => (ebt: Box[U])) - Box[U](elem))
      def -[U >: T <: E](elem1: U, elem2: U, elem3: U*): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => (ebt: Box[U])) - (Box[U](elem1), Box[U](elem2), elem3.map(Box[U](_)): _*))
      def --[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => (ebt: Box[U])) -- elems.toList.map(Box[U](_)))
      def --[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => (ebt: Box[U])) -- that.toBoxStandardSet)
      def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
      def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
      def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
      def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: Box[T]) => seqop(b, e.value), combop)
      def canEqual(that: Any): Boolean =
        that match {
          case thatSet: (Collections[_]#Immutable#Set[_]) => thatSet.path.equality eq thisCollections.equality
          case _ => false
        }
      def collect[U >: T <: E](pf: PartialFunction[T, U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying collect { case hb: thisCollections.Box[T] if pf.isDefinedAt(hb.value) => Box[U](pf(hb.value)) })
      def contains[U >: T <: E](elem: U): Boolean = underlying.toList.contains(Box[U](elem))
  
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]]): Unit = underlying.copyToArray(xs)
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int): Unit = underlying.copyToArray(xs, start)
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
      def copyToBuffer[U >: T <: E](dest: mutable.Buffer[thisCollections.Box[U]]): Unit = underlying.copyToBuffer(dest)
      def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
      def diff[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => ebt: Box[U]) diff that.toBoxStandardSet)
      def drop(n: Int): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.drop(n))
      def dropRight(n: Int): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.dropRight(n))
      def dropWhile(pred: T => Boolean): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.dropWhile((p: Box[T]) => pred(p.value)))
      override def equals(other: Any): Boolean = { 
        other match {
          case thatSet: Collections[E]#Immutable#Set[T] => 
            (thisCollections.equality eq thatSet.path.equality) && underlying == thatSet.toBoxStandardSet
          case _ => false
        }
      }
      def exists(pred: T => Boolean): Boolean = underlying.exists((box: Box[T]) => pred(box.value))
      def filter(pred: T => Boolean): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.filter((box: Box[T]) => pred(box.value)))
      def filterNot(pred: T => Boolean): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.filterNot((box: Box[T]) => pred(box.value)))
      def find(pred: T => Boolean): Option[T] = underlying.find((box: Box[T]) => pred(box.value)).map(_.value)
      def flatMap[U](f: T => GenIterableOnce[U]): SetView[U] = view.flatMap(f)
      def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
      def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
      def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
      def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
      def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.FastSet[T]] = underlying.groupBy((box: Box[T]) => f(box.value)).map(t => (t._1, new immutable.FastSet[T](t._2)))
      def grouped(size: Int): Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.grouped(size).map(new immutable.FastSet[T](_)))
      def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
      override def hashCode: Int = underlying.hashCode
      def headOption: Option[T] =
        underlying.headOption match {
          case Some(head) => Some(head.value)
          case None => None
        }
      def init: thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.init)
      def inits: Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.inits.map(new immutable.FastSet[T](_)))
      def intersect[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => ebt: Box[U]) intersect that.toBoxStandardSet)
      def isEmpty: Boolean = underlying.isEmpty
      def iterator: org.scalactic.iterators.Iterator[T] = 
        new Iterator[T] {
          private val it = underlying.iterator.map(_.value)
          def hasNext: Boolean = it.hasNext
          def next(): T = it.next()
          def iterator: Iterator[T] = this
          def toStandardList: scala.collection.immutable.List[T] = it.toList
        }
      def last: T = underlying.last.value
      def lastOption: Option[T] =
        underlying.lastOption match {
          case Some(last) => Some(last.value)
          case None => None
        }
      def map[U](f: T => U): FastSetView[U] = view.map(f)
      def max[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).max(ord)
      def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).maxBy(f)
      def membership[U >: T <: E]: Membership[U] = new Membership[U]((a: U) => thisFastSet.toStandardList.exists(ele => equality.areEqual(ele, a)))
      def min[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).min(ord)
      def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).minBy(f)
      def mkString(start: String, sep: String, end: String): String = underlying.toList.map(_.value).mkString(start, sep, end)
      def mkString(sep: String): String = underlying.toList.map(_.value).mkString(sep)
      def mkString: String = underlying.toList.map(_.value).mkString
      def nonEmpty: Boolean = underlying.nonEmpty
      def partition(pred: T => Boolean): (thisCollections.immutable.FastSet[T], thisCollections.immutable.FastSet[T]) = {
        val tuple2 = underlying.partition((box: Box[T]) => pred(box.value))
        (new immutable.FastSet[T](tuple2._1), new immutable.FastSet[T](tuple2._2))
      }
      def product[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.toList.map(_.value).product(num)
      def reduce[T1 >: T](op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).reduce(op)
      def reduceLeft[T1 >: T](op: (T1, T) => T1): T1 = underlying.toList.map(_.value).reduceLeft(op)
      def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1] = underlying.toList.map(_.value).reduceLeftOption(op)
      def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceOption(op)
      def reduceRight[T1 >: T](op: (T, T1) => T1): T1 = underlying.toList.map(_.value).reduceRight(op)
      def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceRightOption(op)
      def sameElements[T1 >: T](that: GenIterable[T1]): Boolean = underlying.toList.map(_.value).sameElements(that)
/*
      def scanLeft(z: T)(op: (T, T) => T): thisCollections.immutable.FastSet[T] = {
        val set = underlying.scanLeft(Box[T](z))((b1: Box[T], b2: Box[T]) => Box[T](op(b1.value, b2.value)))
        new immutable.FastSet[T](set)
      }
      def scanRight(z: T)(op: (T, T) => T): thisCollections.immutable.FastSet[T] = {
        val set = underlying.scanRight(Box[T](z))((b1: Box[T], b2: Box[T]) => Box[T](op(b1.value, b2.value)))
        new immutable.FastSet[T](set)
      }
*/
      def size: Int = underlying.size
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.slice(unc_from, unc_until))
      def sliding(size: Int): Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.sliding(size).map(new immutable.FastSet[T](_)))
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.sliding(size, step).map(new immutable.FastSet[T](_)))
      def span(pred: T => Boolean): (thisCollections.immutable.FastSet[T], thisCollections.immutable.FastSet[T]) = {
        val (trueSet, falseSet) = underlying.span((box: Box[T]) => pred(box.value))
        (new immutable.FastSet[T](trueSet), new immutable.FastSet[T](falseSet))
      }
      def splitAt(n: Int): (thisCollections.immutable.FastSet[T], thisCollections.immutable.FastSet[T]) = {
        val (trueSet, falseSet) = underlying.splitAt(n)
        (new immutable.FastSet[T](trueSet), new immutable.FastSet[T](falseSet))
      }
      def stringPrefix: String = "Set"
      def subsetOf[U >: T <: E](that: thisCollections.immutable.Set[U]): Boolean = underlying.map(ebt => ebt: Box[U]).subsetOf(that.toBoxStandardSet)
      def subsets(len: Int): Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.subsets(len).map(new immutable.FastSet[T](_)))
      def subsets: Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.subsets.map(new immutable.FastSet[T](_)))
      def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
      def tail: thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.tail)
      def tails: Iterator[thisCollections.immutable.FastSet[T]] = Iterator(underlying.tails.map(new immutable.FastSet[T](_)))
      def take(n: Int): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.take(n))
      def takeRight(n: Int): thisCollections.immutable.FastSet[T] = new immutable.FastSet[T](underlying.takeRight(n))
      def toArray[U >: T <: E](implicit ct: ClassTag[U]): Array[U] = {
        // A workaround becauase underlying.map(_.value).toArray does not work due to this weird error message:
        // No ClassTag available for T
        // val arr: Array[Any] = new Array[Any](underlying.size)
        // underlying.map(_.value).copyToArray(arr)
        // arr.asInstanceOf[Array[T]]
        underlying.map(_.value).toArray
      }
      def toBoxArray[U >: T <: E]: Array[thisCollections.Box[U]] = underlying.toArray
      def toStandardBuffer[U >: T <: E]: scala.collection.mutable.Buffer[U] = underlying.map(_.value).toBuffer
      def toBoxStandardBuffer[U >: T <: E]: scala.collection.mutable.Buffer[thisCollections.Box[U]] = underlying.toBuffer
      def toStandardIndexedSeq: scala.collection.immutable.IndexedSeq[T] = underlying.map(_.value).toIndexedSeq
      def toBoxStandardIndexedSeq: scala.collection.immutable.IndexedSeq[thisCollections.Box[T]] = underlying.toIndexedSeq
      def toStandardIterable: GenIterable[T] = underlying.toIterable.map(_.value)
      def toBoxStandardIterable: GenIterable[thisCollections.Box[T]] = underlying.toIterable
      def toStandardIterator: scala.collection.Iterator[T] = underlying.toIterator.map(_.value)
      def toBoxStandardIterator: scala.collection.Iterator[thisCollections.Box[T]] = underlying.toIterator
      def toBoxStandardList: List[thisCollections.Box[T]] = underlying.toList
      def toStandardList: List[T] = underlying.toList.map(_.value)
      def toStandardMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.map(_.value).toMap
      def toStandardParArray[U >: T <: E]: ParArray[U] = underlying.toParArray.map(_.value)
      def toBoxStandardParArray[U >: T <: E]: ParArray[thisCollections.Box[U]] = underlying.toList.map(ebt => ebt: Box[U]).toParArray
      def toStandardSeq: GenSeq[T] = underlying.toSeq.map(_.value)
      def toBoxStandardSeq: GenSeq[thisCollections.Box[T]] = underlying.toSeq
      def toStandardSet[U >: T <: E]: scala.collection.immutable.Set[U] = underlying.map(_.value)
      def toBoxStandardSet[U >: T <: E]: scala.collection.immutable.Set[thisCollections.Box[U]] = underlying.map(ebt => (ebt: Box[U]))
      def toStandardStream: Stream[T] = underlying.toStream.map(_.value)
      def toBoxStandardStream: Stream[thisCollections.Box[T]] = underlying.toStream
      def toStandardTraversable: GenTraversable[T] = underlying.map(_.value)
      def toBoxStandardTraversable: GenTraversable[thisCollections.Box[T]] = underlying.toTraversable
      def toStandardVector: Vector[T] = underlying.toVector.map(_.value)
      def toBoxStandardVector: Vector[thisCollections.Box[T]] = underlying.toVector
      // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.FastSet[T] = {
        val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
        new immutable.FastSet[T](listList.map(Box[T](_)).toSet)
      }
      def union[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.FastSet[U] =
        new immutable.FastSet[U](underlying.map(ebt => ebt: Box[U]) union that.toBoxStandardSet)
/*
      def unzip[T1, T2](t1Collections: Collections[T1], t2Collections: Collections[T2])(implicit asPair: T => (T1, T2)): (t1Collections.immutable.FastSet[T1], t2Collections.immutable.FastSet[T2]) = {
        val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
        (t1Collections.immutable.FastSet[T1](t1: _*), t2Collections.immutable.FastSet[T2](t2: _*))
      }
      def unzip3[T1, T2, T3](t1Collections: Collections[T1], t2Collections: Collections[T2], t3Collections: Collections[T3])(implicit asTriple: T => (T1, T2, T3)): (t1Collections.immutable.FastSet[T1], t2Collections.immutable.FastSet[T2], t3Collections.immutable.FastSet[T3]) = {
        val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
        (t1Collections.immutable.FastSet[T1](t1: _*), t2Collections.immutable.FastSet[T2](t2: _*), t3Collections.immutable.FastSet[T3](t3: _*))
      }
      def zip[U](that: GenIterable[U]): Set[(T, U)] = underlying.toList.map(_.value).zip(that).toSet
      def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U): Set[(T1, U)] = underlying.toList.map(_.value).zipAll(that, thisElem, thatElem).toSet
      def zipWithIndex: Set[(T, Int)] = underlying.toList.map(_.value).zipWithIndex.toSet
 */
      val path: thisCollections.type = thisCollections
      def view: FastSetView[T] = FastSetView(thisFastSet.toStandardList: _*)
    }

    object FastSet {
      def empty[T <: E]: thisCollections.immutable.FastSet[T] = new thisCollections.immutable.FastSet[T](scala.collection.immutable.Set.empty)
      def apply[T <: E](): thisCollections.immutable.FastSet[T] = 
        new thisCollections.immutable.FastSet[T](scala.collection.immutable.Set.empty)
      def apply[T <: E](firstElem: T, otherElems: T*): thisCollections.immutable.inhabited.FastSet[T] = {
        val elements = firstElem +: otherElems
        new thisCollections.immutable.inhabited.FastSet[T](scala.collection.immutable.Set(elements.map(Box[T](_)): _*))
      }
    }
    object Set {
      def empty[T <: E]: thisCollections.immutable.Set[T] = thisCollections.immutable.FastSet.empty[T]
      def apply[T <: E](): thisCollections.immutable.Set[T] = thisCollections.immutable.FastSet.empty[T]
      def apply[T <: E](firstElem: T, otherElems: T*): thisCollections.immutable.inhabited.Set[T] = {
        thisCollections.immutable.inhabited.FastSet[T](firstElem, otherElems: _*)
      }
    }
  /*
    class FastEquaMap[V] private[scalactic] (private val underlying: Map[Box, V]) extends EquaMap[V] { thisFastEquaMap =>
      def + [V1 >: V](kv: (T, V1)): FastEquaMap[V1] = new FastEquaMap(underlying + (Box(kv._1) -> kv._2))
      def +[V1 >: V](entry1: (T, V1), entry2: (T, V1), entries: (T, V1)*): FastEquaMap[V1] =
        new FastEquaMap(underlying + (Box(entry1._1) -> entry1._2, Box(entry2._1) -> entry2._2, entries.map(e => Box(e._1) -> e._2): _*))
      def ++[V1 >: V](entries: GenTraversableOnce[(T, V1)]): FastEquaMap[V1] = new FastEquaMap(underlying ++ entries.toList.map(e => (Box(e._1) -> e._2)))
      def ++[V1 >: V](that: EquaMap[V1]): FastEquaMap[V1] = new FastEquaMap(underlying ++ that.toBoxMap)
      def -(key: T): FastEquaMap[V] = new FastEquaMap(underlying - Box(key))
      def -(key1: T, key2: T, keys: T*): FastEquaMap[V] =
        new FastEquaMap(underlying - (Box(key1), Box(key2), keys.map(Box(_)): _*))
      def --(keys: GenTraversableOnce[T]): FastEquaMap[V] =
        new FastEquaMap(underlying -- keys.toList.map(Box(_)))
      def --(equaSet: thisCollections.immutable.Set): FastEquaMap[V] =
        new FastEquaMap(underlying -- equaSet.toBoxStandardSet)
      def /:[R](z: R)(op: (R, (T, V)) => R): R =
        underlying.toSeq.map(e => (e._1.value, e._2))./:(z)((r: R, e: (T, V)) => op(r, e))
      def keysIterator: Iterator[T] = underlying.keysIterator.map(_.value)
      def valuesIterator: Iterator[V] = underlying.valuesIterator
      def isEmpty: Boolean = underlying.isEmpty
      def size: Int = underlying.size
      def toStandardMap: Map[T, V] = underlying.map(e => (e._1.value, e._2))
      def toBoxStandardMap: Map[thisCollections.Box, V] = underlying
      val path: thisCollections.type = thisCollections
      def stringPrefix: String = "EquaMap"
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(e => e._1.value + " -> " + e._2).mkString(", ")})"
      override def equals(other: Any): Boolean = {
        other match {
          case thatEquaMap: Collections[_]#EquaMap[_] =>
            (thisCollections.equality eq thatEquaMap.path.equality) && underlying == thatEquaMap.toBoxMap
          case _ => false
        }
      }
    }
    object FastEquaMap {
      def empty[V]: FastEquaMap[V] = new FastEquaMap[V](Map.empty)
      def apply[V](entries: (T, V)*): FastEquaMap[V] = new FastEquaMap[V](Map.empty ++ entries.map(e => Box(e._1) -> e._2))
    }
    object EquaMap {
      def empty[V]: EquaMap[V] = new FastEquaMap[V](Map.empty)
      def apply[V](entries: (T, V)*): EquaMap[V] = new FastEquaMap[V](Map.empty ++ entries.map(e => Box(e._1) -> e._2))
    }
  */
  
    class Inhabited {
      trait Set[+T <: E] extends thisImmutable.Set[T] {
        /** Selects the first element of this `Set`.
          *
          * @return the first element of this `Set`.
          * @throws `NoSuchElementException` if the `Set` is empty.
          */
        def head: T
      }
  
      class FastSet[+T <: E] private[scalactic] (private val underlying: scala.collection.immutable.Set[Box[T@uV]]) extends thisImmutable.FastSet[T](underlying) with Set[T] {
        def head: T = underlying.head.value
      }
      object FastSet {
        def apply[T <: E](firstElem: T, otherElems: T*): thisCollections.immutable.inhabited.FastSet[T] = {
          val elements = firstElem +: otherElems
          new thisCollections.immutable.inhabited.FastSet[T](scala.collection.immutable.Set(elements.map(Box[T](_)): _*))
        }
      }
      object Set {
        def apply[T <: E](firstElem: T, otherElems: T*): thisCollections.immutable.inhabited.Set[T] = {
          thisCollections.immutable.inhabited.FastSet[T](firstElem, otherElems: _*)
        }
      }
    }
    val inhabited: Inhabited = new Inhabited
  }
  val immutable: Immutable = new Immutable
  type Set[T <: E] = immutable.Set[T]
  lazy val Set = immutable.Set
  type FastSet[T <: E] = immutable.FastSet[T]
  lazy val FastSet = immutable.FastSet
}

object Collections {
  def apply[T](implicit equality: HashingEquality[T]): Collections[T] = new Collections(equality)
  val default: Collections[Any] = Collections[Any](HashingEquality.default[Any])
}
