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

import scala.Iterator
import scala.collection.generic.CanBuildFrom
import scala.collection.immutable._
import scala.collection.mutable
import scala.collection.GenTraversableOnce
import scala.collection.GenTraversable
import scala.collection.GenSeq
import scala.collection.GenMap
import scala.collection.GenIterable
import scala.collection.TraversableView
import scala.collection.parallel.mutable.ParArray
import scala.annotation.unchecked.{ uncheckedVariance => uV }
import scala.language.higherKinds

class SortedCollections[E](override val equality: OrderingEquality[E]) extends Collections[E](equality) { thisCollections =>

  def ordering[T <: E]: Ordering[thisCollections.EquaBox[T]] =
    new Ordering[thisCollections.EquaBox[T]] {
      def compare(a: thisCollections.EquaBox[T], b: thisCollections.EquaBox[T]): Int =
        equality.compare(a.value, b.value)
    }

  class SortedImmutable extends Immutable {

    trait SortedEquaSet[T <: E] extends EquaSet[T] {
  
      /**
       * Creates a new `SortedEquaSet` with an additional element, unless the element is
       * already present.
       *
       * @param elem the element to be added
       * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` and that also
       * contains `elem`.
       */
      def + (elem: T): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Creates a new `SortedEquaSet` with additional elements.
       *
       * This method takes two or more elements to be added. Another overloaded
       * variant of this method handles the case where a single element is added.
       *
       * @param elem1 the first element to add.
       * @param elem2 the second element to add.
       * @param elems the remaining elements to add.
       * @return a new `SortedEquaSet` with the given elements added.
       */
      def + (elem1: T, elem2: T, elems: T*): thisCollections.immutable.SortedEquaSet[T]
  
      /** Creates a new `SortedEquaSet` by adding all elements contained in another collection to this `SortedEquaSet`.
        *
        *  @param elems     the collection containing the added elements.
        *  @return          a new `SortedEquaSet` with the given elements added.
        */
      def ++ (elems: GenTraversableOnce[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Creates a new `SortedEquaSet` by adding elements contained in another `EquaSet`.
       *
       * @param that     the other `EquaSet` containing the added elements.
       * @return         a new `SortedEquaSet` with the given elements added.
       */
      def ++ (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Creates a new `SortedEquaSet` with a given element removed from this `SortedEquaSet`.
       *
       * @param elem the element to be removed
       * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` but that does not
       * contain `elem`.
       */
      def - (elem: T): thisCollections.immutable.SortedEquaSet[T]
  
      /* * USE LATER
       * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another
       * collection.
       *
       * @param xs the collection containing the removed elements.
       * @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
       * except one less occurrence of each of the elements of `elems`.
       */
  
      /**
       * Creates a new `SortedEquaSet` from this `SortedEquaSet` with some elements removed.
       *
       * This method takes two or more elements to be removed. Another overloaded
       * variant of this method handles the case where a single element is
       * removed.
       * @param elem1 the first element to remove.
       * @param elem2 the second element to remove.
       * @param elems the remaining elements to remove.
       * @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
       * except one less occurrence of each of the given elements.
       */
      def - (elem1: T, elem2: T, elems: T*): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another
       *  collection.
       *
       *  @param elems     the collection containing the removed elements.
       *  @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
       *  except one less occurrence of each of the elements of `elems`.
       */
      def --(elems: GenTraversableOnce[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another `EquaSet`
       *
       * @param that       the other `EquaSet` containing the removed elements.
       * @return a new `SortedEquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
       */
      def --(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Computes the union between this `SortedEquaSet` and another `EquaSet`.
       *
       * '''Note:''' Same as `union`.
       * @param that the `EquaSet` to form the union with.
       * @return a new `SortedEquaSet` consisting of all elements that are in this
       * `SortedEquaSet` or in the given `EquaSet` `that`.
       */
      def | (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
       *
       * '''Note:''' Same as `intersect`.
       * @param that the `EquaSet` to intersect with.
       * @return a new `SortedEquaSet` consisting of all elements that are both in this
       * `SortedEquaSet` and in the given `EquaSet` `that`.
       */
      def & (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * The difference of this `SortedEquaSet` and another `EquaSet`.
       *
       * '''Note:''' Same as `diff`.
       * @param that the `EquaSet` of elements to exclude.
       * @return a `SortedEquaSet` containing those elements of this
       * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
       */
      def &~ (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Builds a new collection by applying a partial function to all elements of this `SortedEquaSet`
       * on which the function is defined.
       *
       * @param pf the partial function which filters and maps the `SortedEquaSet`.
       * @return a new collection of type `That` resulting from applying the partial function
       * `pf` to each element on which it is defined and collecting the results.
       * The order of the elements is preserved.
       *
       * @return a new `SortedEquaSet` resulting from applying the given partial function
       * `pf` to each element on which it is defined and collecting the results.
       * The order of the elements is preserved.
       */
      def collect(pf: PartialFunction[T, T]): thisCollections.immutable.SortedEquaSet[T]
  
      def contains[U](elem: U)(implicit ev: U <:< T): Boolean
  
      /**
       * Computes the difference of this `SortedEquaSet` and another `SortedEquaSet`.
       *
       * @param that the `EquaSet` of elements to exclude.
       * @return a `SortedEquaSet` containing those elements of this
       * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
       */
      def diff(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Selects all elements except first ''n'' ones.
       *
       * @param n the number of elements to drop from this `SortedEquaSet`.
       * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the first `n` ones, or else the
       * empty `SortedEquaSet`, if this `EquaSet` has less than `n` elements.
       */
      def drop(n: Int): thisCollections.immutable.SortedEquaSet[T]
  
      /** Selects all elements except last ''n'' ones.
        *
        * @param n The number of elements to take
        * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the last `n` ones, or else the
        * empty `SortedEquaSet`, if this `SortedEquaSet` has less than `n` elements.
        */
      def dropRight(n: Int): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Drops longest prefix of elements that satisfy a predicate.
       *
       * @param pred The predicate used to test elements.
       * @return the longest suffix of this `SortedEquaSet` whose first element
       * does not satisfy the predicate `p`.
       */
      def dropWhile(pred: T => Boolean): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Selects all elements of this `SortedEquaSet` which satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `SortedEquaSet` consisting of all elements of this `SortedEquaSet` that satisfy the given
       * predicate <code>pred</code>.
       */
      def filter(pred: T => Boolean): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Selects all elements of this `SortedCollections` which do not satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `SortedCollections` consisting of all elements of this `SortedCollections` that do not satisfy the given
       * predicate <code>pred</code>.
       */
      def filterNot(pred: T => Boolean): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Partitions this `SortedEquaSet` into a map of `SortedEquaSet`s according to some discriminator function.
       *
       * Note: this method is not re-implemented by views. This means
       * when applied to a view it will always force the view and
       * return a new `SortedEquaSet`.
       *
       * @param f the discriminator function.
       * @tparam K the type of keys returned by the discriminator function.
       * @return A map from keys to `SortedEquaSet`s such that the following invariant holds:
       * {{{
       * (xs groupBy f)(k) = xs filter (x => f(x) == k)
       * }}}
       * That is, every key `k` is bound to a `SortedEquaSet` of those elements `x`
       * for which `f(x)` equals `k`.
       *
       */
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Partitions elements in fixed size `SortedEquaSet`s.
       * @see [[scala.collection.Iterator]], method `grouped`
       *
       * @param size the number of elements per group
       * @return An iterator producing `SortedEquaSet`s of size `size`, except the
       * last will be less than size `size` if the elements don't divide evenly.
       */
      def grouped(size: Int): Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Selects all elements except the last.
       *
       * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
       * except the last one.
       * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
       */
      def init: thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Iterates over the inits of this `SortedEquaSet`. The first value will be this
       * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
       * values the results of successive applications of `init`.
       *
       * @return an iterator over all the inits of this `SortedEquaSet`
       * @example SortedEquaSet(1,2,3).inits = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(1,2), SortedEquaSet(1), SortedEquaSet())
       */
      def inits: Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
       *
       * @param that the `EquaSet` to intersect with.
       * @return a new `SortedEquaSet` consisting of all elements that are both in this
       * `SortedEquaSet` and in the given `EquaSet` `that`.
       */
      def intersect(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Tests if this `SortedEquaSet` is empty.
       *
       * @return `true` if there is no element in the set, `false` otherwise.
       */
      def isEmpty: Boolean
  
      /**
       * Get an instance of `Iterator` for elements of this `SortedEquaSet`.
       *
       * @return an instance of `Iterator` for elements of this `SortedEquaSet`
       */
      def iterator: Iterator[T]
  
      /**
       * Partitions this `SortedEquaSet` in two `SortedEquaSet`s according to a predicate.
       *
       * @param pred the predicate on which to partition.
       * @return a pair of `SortedEquaSet`s: the first `SortedEquaSet` consists of all elements that
       * satisfy the predicate `p` and the second `SortedEquaSet` consists of all elements
       * that don't. The relative order of the elements in the resulting `SortedEquaSet`s
       * may not be preserved.
       */
      def partition(pred: T => Boolean): (thisCollections.immutable.SortedEquaSet[T], thisCollections.immutable.SortedEquaSet[T])
  
      /**
       * The `SortedSet[EquaBox]` underlying this `SortedEquaSet` object.
       */
      def repr: SortedSet[EquaBox[T]]
  
      /**
       * Produces a collection containing cumulative results of applying the
       * operator going left to right.
       *
       * @param z the initial value
       * @param op the binary operator applied to the intermediate result and the element
       * @return `SortedEquaSet` with intermediate results
       */
      def scanLeft(z: T)(op: (T, T) => T): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Produces a collection containing cumulative results of applying the operator going right to left.
       * The head of the collection is the last cumulative result.
       *
       * Example:
       * {{{
       * `SortedEquaSet`(1, 2, 3, 4).scanRight(0)(_ + _) == `SortedEquaSet`(10, 9, 7, 4, 0)
       * }}}
       *
       * @param z the initial value
       * @param op the binary operator applied to the intermediate result and the element
       * @return `SortedEquaSet` with intermediate results
       */
      def scanRight(z: T)(op: (T, T) => T): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * The size of this `SortedEquaSet`.
       *
       * @return the number of elements in this `SortedEquaSet`.
       */
      def size: Int
  
      /**
       * Selects an interval of elements. The returned collection is made up
       * of all elements `x` which satisfy the invariant:
       * {{{
       * from <= indexOf(x) < until
       * }}}
       *
       * @param unc_from the lowest index to include from this `EquaSet`.
       * @param unc_until the lowest index to EXCLUDE from this `EquaSet`.
       * @return an `EquaSet` containing the elements greater than or equal to
       * index `from` extending up to (but not including) index `until`
       * of this `EquaSet`.
       */
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Groups elements in fixed size blocks by passing a "sliding window"
       * over them (as opposed to partitioning them, as is done in grouped.)
       * @see [[scala.collection.Iterator]], method `sliding`
       *
       * @param size the number of elements per group
       * @return An iterator producing `SortedEquaSet`s of size `size`, except the
       * last and the only element will be truncated if there are
       * fewer elements than size.
       */
      def sliding(size: Int): Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Groups elements in fixed size blocks by passing a "sliding window"
       * over them (as opposed to partitioning them, as is done in grouped.)
       * @see [[scala.collection.Iterator]], method `sliding`
       *
       * @param size the number of elements per group
       * @param step the distance between the first elements of successive
       * groups (defaults to 1)
       * @return An iterator producing `SortedEquaSet`s of size `size`, except the
       * last and the only element will be truncated if there are
       * fewer elements than size.
       */
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Splits this `SortedEquaSet` into a prefix/suffix pair according to a predicate.
       *
       * Note: `c span p` is equivalent to (but possibly more efficient than)
       * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
       * predicate `p` does not cause any side-effects.
       *
       *
       * @param pred the test predicate
       * @return a pair consisting of the longest prefix of this `SortedEquaSet` whose
       * elements all satisfy `p`, and the rest of this `SortedEquaSet`.
       */
      def span(pred: T => Boolean): (thisCollections.immutable.SortedEquaSet[T], thisCollections.immutable.SortedEquaSet[T])
  
      /**
       * Splits this `SortedEquaSet` into two at a given position.
       * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
       * `(c take n, c drop n)`.
       *
       *
       * @param n the position at which to split.
       * @return a pair of `SortedEquaSet`s consisting of the first `n`
       * elements of this `SortedEquaSet`, and the other elements.
       */
      def splitAt(n: Int): (thisCollections.immutable.SortedEquaSet[T], thisCollections.immutable.SortedEquaSet[T])
  
      /**
       * An iterator over all subsets of this set of the given size.
       * If the requested size is impossible, an empty iterator is returned.
       *
       * @param len the size of the subsets.
       * @return the iterator.
       */
      def subsets(len: Int): Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * An iterator over all subsets of this set.
       *
       * @return the iterator.
       */
      def subsets: Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Selects all elements except the first.
       *
       * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
       * except the first one.
       * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
       */
      def tail: thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Iterates over the tails of this `SortedEquaSet`. The first value will be this
       * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
       * values the results of successive applications of `tail`.
       *
       * @return an iterator over all the tails of this `SortedEquaSet`
       * @example `SortedEquaSet(1,2,3).tails = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(2,3), SortedEquaSet(3), SortedEquaSet())`
       */
      def tails: Iterator[thisCollections.immutable.SortedEquaSet[T]]
  
      /**
       * Selects first ''n'' elements.
       *
       * @param n the number of elements to take from this `SortedEquaSet`.
       * @return a `SortedEquaSet` consisting only of the first `n` elements of this `SortedEquaSet`,
       * or else the whole `SortedEquaSet`, if it has less than `n` elements.
       */
      def take(n: Int): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Selects last ''n'' elements.
       *
       *
       * @param n the number of elements to take
       * @return a `SortedEquaSet` consisting only of the last `n` elements of this `SortedEquaSet`, or else the
       * whole `SortedEquaSet`, if it has less than `n` elements.
       */
      def takeRight(n: Int): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Converts this `SortedEquaSet` to a set.
       *
       * @return a set containing all elements of this `SortedEquaSet`.
       */
      def toSet: SortedSet[T]
  
      /**
       * Converts this `SortedEquaSet` to a set of `EquaBox`.
       *
       * @return a set containing all elements of this `SortedEquaSet`, boxed in `EquaBox`.
       */
      def toEquaBoxSet: SortedSet[thisCollections.EquaBox[T]]
  
      /**
       * Transposes this `SortedEquaSet` of traversable collections into
       * a `SortedEquaSet` of `GenTraversableOnce`s.
       *
       * The resulting collection's type will be guided by the
       * static type of `EquaSet`. For example:
       *
       * {{{
       * val xs = SortedEquaSet(
       * List(1, 2, 3),
       * List(4, 5, 6)).transpose
       * // xs == SortedEquaSet(
       * // List(1, 4),
       * // List(2, 5),
       * // List(3, 6))
       *
       * val ys = SortedEquaSet(
       * List(1, 2, 3),
       * List(4, 5, 6)).transpose
       * // ys == SortedEquaSet(
       * // Vector(1, 4),
       * // Vector(2, 5),
       * // Vector(3, 6))
       * }}}
       *
       * @tparam B the type of the elements of each traversable collection.
       * @param asTraversable an implicit conversion which asserts that the
       * element type of this `SortedEquaSet` is a `Traversable`.
       * @return a two-dimensional `SortedEquaSet` of ${coll}s which has as ''n''th row
       * the ''n''th column of this `SortedEquaSet`.
       * @throws `IllegalArgumentException` if all collections in this `SortedEquaSet`
       * are not of the same size.
       */
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.SortedEquaSet[T]
  
      /**
       * Computes the union between of set and another set.
       *
       * @param that the set to form the union with.
       * @return a new set consisting of all elements that are in this
       * set or in the given set `that`.
       */
      def union(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.SortedEquaSet[T]
  
      override val path: thisCollections.type
  
  /*
      def copyInto(thatCollections: Collections[T]): thatCollections.EquaSet
  
      def copyInto(thatCollections: SortedCollections[T]): thatCollections.SortedEquaSet
  */
      def view: SortedEquaSetView[T]
    }
  
    class TreeEquaSet[T <: E] private[scalactic] (private val underlying: TreeSet[EquaBox[T]]) extends SortedEquaSet[T] { thisTreeEquaSet =>
  
      def + (elem: T): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying + EquaBox[T](elem))
      def + (elem1: T, elem2: T, elems: T*): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying + (EquaBox[T](elem1), EquaBox[T](elem2), elems.map(EquaBox[T](_)): _*))
      def ++ (elems: GenTraversableOnce[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying ++ elems.toSeq.map(EquaBox[T](_)))
      def ++ (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying ++ that.toEquaBoxSet)
      def - (elem: T): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying - EquaBox[T](elem))
      def - (elem1: T, elem2: T, elems: T*): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying - (EquaBox[T](elem1), EquaBox[T](elem2), elems.map(EquaBox[T](_)): _*))
      def --(elems: GenTraversableOnce[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying -- elems.toSeq.map(EquaBox[T](_)))
      def --(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying -- that.toEquaBoxSet)
      def | (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] = this union that
      def & (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] = this intersect that
      def &~ (that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] = this diff that
      def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
      def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
      def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
      def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox[T]) => seqop(b, e.value), combop)
      def apply(elem: T): Boolean = underlying.apply(EquaBox[T](elem))
      // What this one is saying is that two different Collections instances can contain equal Collections so
      // long as the Equality discriminator is the same instance.
      def canEqual(that: Any): Boolean =
        that match {
          case thatEquaSet: Collections[_]#Immutable#EquaSet[_] => thatEquaSet.path.equality eq thisCollections.equality
          case _ => false
        }
        // that.isInstanceOf[thisCollections.immutable.EquaSet] && equality == that.asInstanceOf[thisCollections.immutable.EquaSet].path.equality
      def collect(pf: PartialFunction[T, T]): thisCollections.immutable.TreeEquaSet[T] = {
        implicit val ord: Ordering[thisCollections.EquaBox[T]] = ordering
        new immutable.TreeEquaSet[T](underlying collect { case hb: thisCollections.EquaBox[T] if pf.isDefinedAt(hb.value) => EquaBox[T](pf(hb.value)) })
      }
      def contains[U](elem: U)(implicit ev: U <:< T): Boolean = underlying.contains(EquaBox[T](elem))
      def copyToArray(xs: Array[thisCollections.EquaBox[T]]): Unit = underlying.copyToArray(xs)
      def copyToArray(xs: Array[thisCollections.EquaBox[T]], start: Int): Unit = underlying.copyToArray(xs, start)
      def copyToArray(xs: Array[thisCollections.EquaBox[T]], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
      def copyToBuffer(dest: mutable.Buffer[thisCollections.EquaBox[T]]): Unit = underlying.copyToBuffer(dest)
      def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
      def diff(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying diff that.toEquaBoxSet)
      def drop(n: Int): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.drop(n))
      def dropRight(n: Int): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.dropRight(n))
      def dropWhile(pred: T => Boolean): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.dropWhile((p: EquaBox[T]) => pred(p.value)))
      // Two Collections whose containing Collections have identical equalities can be equal
      override def equals(other: Any): Boolean =
        other match {
          case thatEquaSet: Collections[_]#Immutable#EquaSet[_] => 
            (thisCollections.equality eq thatEquaSet.path.equality) && underlying == thatEquaSet.toEquaBoxSet
          case _ => false
        }
  /*
        other match {
          case equaSet: thisCollections.immutable.EquaSet => 
            underlying == equaSet.toSet
          case _ => false
        }
  */
      def exists(pred: T => Boolean): Boolean = underlying.exists((box: EquaBox[T]) => pred(box.value))
      def filter(pred: T => Boolean): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.filter((box: EquaBox[T]) => pred(box.value)))
      def filterNot(pred: T => Boolean): thisCollections.immutable.SortedEquaSet[T] = new immutable.TreeEquaSet[T](underlying.filterNot((box: EquaBox[T]) => pred(box.value)))
      def find(pred: T => Boolean): Option[T] = underlying.find((box: EquaBox[T]) => pred(box.value)).map(_.value)
      def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
      def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
      def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
      def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
      def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.TreeEquaSet[T]] = underlying.groupBy((box: EquaBox[T]) => f(box.value)).map(t => (t._1, new immutable.TreeEquaSet[T](t._2)))
      def grouped(size: Int): Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.grouped(size).map(new immutable.TreeEquaSet[T](_))
      def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
      override def hashCode: Int = underlying.hashCode
      def head: T = underlying.head.value
      def headOption: Option[T] =
        underlying.headOption match {
          case Some(head) => Some(head.value)
          case None => None
        }
      def init: thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.init)
      def inits: Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.inits.map(new immutable.TreeEquaSet[T](_))
      def intersect(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying intersect that.toEquaBoxSet)
      def isEmpty: Boolean = underlying.isEmpty
      def iterator: Iterator[T] = underlying.iterator.map(_.value)
      def last: T = underlying.last.value
      def lastOption: Option[T] =
        underlying.lastOption match {
          case Some(last) => Some(last.value)
          case None => None
        }
      def max[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).max(ord)
      def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).maxBy(f)
      def min[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).min(ord)
      def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).minBy(f)
      def mkString(start: String, sep: String, end: String): String = underlying.toList.map(_.value).mkString(start, sep, end)
      def mkString(sep: String): String = underlying.toList.map(_.value).mkString(sep)
      def mkString: String = underlying.toList.map(_.value).mkString
      def nonEmpty: Boolean = underlying.nonEmpty
      def partition(pred: T => Boolean): (thisCollections.immutable.TreeEquaSet[T], thisCollections.immutable.TreeEquaSet[T]) = {
        val tuple2 = underlying.partition((box: EquaBox[T]) => pred(box.value))
        (new immutable.TreeEquaSet[T](tuple2._1), new immutable.TreeEquaSet[T](tuple2._2))
      }
      def product[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.toList.map(_.value).product(num)
      def reduce[T1 >: T](op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).reduce(op)
      def reduceLeft[T1 >: T](op: (T1, T) => T1): T1 = underlying.toList.map(_.value).reduceLeft(op)
      def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1] = underlying.toList.map(_.value).reduceLeftOption(op)
      def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceOption(op)
      def reduceRight[T1 >: T](op: (T, T1) => T1): T1 = underlying.toList.map(_.value).reduceRight(op)
      def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceRightOption(op)
      def repr: SortedSet[EquaBox[T]] = underlying
      def sameElements[T1 >: T](that: GenIterable[T1]): Boolean = underlying.toList.map(_.value).sameElements(that)
      def scanLeft(z: T)(op: (T, T) => T): thisCollections.immutable.TreeEquaSet[T] = {
        val set = underlying.scanLeft(EquaBox[T](z))((b1: EquaBox[T], b2: EquaBox[T]) => EquaBox[T](op(b1.value, b2.value)))
        new immutable.TreeEquaSet[T](TreeSet(set.toList: _*)(ordering))
      }
      def scanRight(z: T)(op: (T, T) => T): thisCollections.immutable.TreeEquaSet[T] = {
        val set = underlying.scanRight(EquaBox[T](z))((b1: EquaBox[T], b2: EquaBox[T]) => EquaBox[T](op(b1.value, b2.value)))
        new immutable.TreeEquaSet[T](TreeSet(set.toList: _*)(ordering))
      }
      def size: Int = underlying.size
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.slice(unc_from, unc_until))
      def sliding(size: Int): Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.sliding(size).map(new immutable.TreeEquaSet[T](_))
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.sliding(size, step).map(new immutable.TreeEquaSet[T](_))
      def span(pred: T => Boolean): (thisCollections.immutable.TreeEquaSet[T], thisCollections.immutable.TreeEquaSet[T]) = {
        val (trueSet, falseSet) = underlying.span((box: EquaBox[T]) => pred(box.value))
        (new immutable.TreeEquaSet[T](trueSet), new immutable.TreeEquaSet[T](falseSet))
      }
      def splitAt(n: Int): (thisCollections.immutable.TreeEquaSet[T], thisCollections.immutable.TreeEquaSet[T]) = {
        val (trueSet, falseSet) = underlying.splitAt(n)
        (new immutable.TreeEquaSet[T](trueSet), new immutable.TreeEquaSet[T](falseSet))
      }
      def stringPrefix: String = "TreeEquaSet"
      def subsetOf(that: thisCollections.immutable.EquaSet[T]): Boolean = underlying.subsetOf(that.toEquaBoxSet)
      def subsets(len: Int): Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.subsets(len).map(new immutable.TreeEquaSet[T](_))
      def subsets: Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.subsets.map(new immutable.TreeEquaSet[T](_))
      def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
      def tail: thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.tail)
      def tails: Iterator[thisCollections.immutable.TreeEquaSet[T]] = underlying.tails.map(new immutable.TreeEquaSet[T](_))
      def take(n: Int): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.take(n))
      def takeRight(n: Int): thisCollections.immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](underlying.takeRight(n))
      def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, thisCollections.EquaBox[T], Col[thisCollections.EquaBox[T] @uV]]): Col[thisCollections.EquaBox[T] @uV] = underlying.to[Col]
      def toArray: Array[T] = {
        // A workaround becauase underlying.map(_.value).toArray does not work due to this weird error message:
        // No ClassTag available for T
        val arr: Array[Any] = new Array[Any](underlying.size)
        underlying.map(_.value).copyToArray(arr)
        arr.asInstanceOf[Array[T]]
      }
      def toEquaBoxArray: Array[thisCollections.EquaBox[T]] = underlying.toArray
      def toBuffer: scala.collection.mutable.Buffer[T] = underlying.map(_.value).toBuffer
      def toEquaBoxBuffer: scala.collection.mutable.Buffer[thisCollections.EquaBox[T]] = underlying.toBuffer
      def toIndexedSeq: scala.collection.immutable.IndexedSeq[T] = underlying.map(_.value).toIndexedSeq
      def toEquaBoxIndexedSeq: scala.collection.immutable.IndexedSeq[thisCollections.EquaBox[T]] = underlying.toIndexedSeq
      def toIterable: GenIterable[T] = underlying.toIterable.map(_.value)
      def toEquaBoxIterable: GenIterable[thisCollections.EquaBox[T]] = underlying.toIterable
      def toIterator: Iterator[T] = underlying.toIterator.map(_.value)
      def toEquaBoxIterator: Iterator[thisCollections.EquaBox[T]] = underlying.toIterator
      def toEquaBoxList: List[thisCollections.EquaBox[T]] = underlying.toList
      def toList: List[T] = underlying.toList.map(_.value)
      def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.map(_.value).toMap
      def toParArray: ParArray[T] = underlying.toParArray.map(_.value)
      def toEquaBoxParArray: ParArray[thisCollections.EquaBox[T]] = underlying.toParArray
      def toSeq: GenSeq[T] = underlying.toSeq.map(_.value)
      def toEquaBoxSeq: GenSeq[thisCollections.EquaBox[T]] = underlying.toSeq
      def toSet: TreeSet[T] = {
        val valueOrdering: Ordering[T] =
          new Ordering[T] {
            def compare(a: T, b: T): Int =
              equality.compare(a, b)
          }
        TreeSet(underlying.map(_.value).toList: _*)(valueOrdering)
      }
      def toEquaBoxSet: TreeSet[thisCollections.EquaBox[T]] = underlying
      def toStream: Stream[T] = underlying.toStream.map(_.value)
      def toEquaBoxStream: Stream[thisCollections.EquaBox[T]] = underlying.toStream
      def toTraversable: GenTraversable[T] = underlying.map(_.value)
      def toEquaBoxTraversable: GenTraversable[thisCollections.EquaBox[T]] = underlying.toTraversable
      def toVector: Vector[T] = underlying.toVector.map(_.value)
      def toEquaBoxVector: Vector[thisCollections.EquaBox[T]] = underlying.toVector
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.TreeEquaSet[T] = {
        val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
        new immutable.TreeEquaSet[T](TreeSet(listList.map(EquaBox[T](_)): _ *)(ordering))
      }
      def union(that: thisCollections.immutable.EquaSet[T]): thisCollections.immutable.TreeEquaSet[T] =
        new immutable.TreeEquaSet[T](underlying union that.toEquaBoxSet)
      def unzip[T1, T2](t1Collections: Collections[T1], t2Collections: Collections[T2])(implicit asPair: T => (T1, T2)): (t1Collections.immutable.EquaSet[T1], t2Collections.immutable.EquaSet[T2]) = {
        val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
        (t1Collections.immutable.EquaSet[T1](t1: _*), t2Collections.immutable.EquaSet[T2](t2: _*))
      }
      def unzip3[T1, T2, T3](t1Collections: Collections[T1], t2Collections: Collections[T2], t3Collections: Collections[T3])(implicit asTriple: T => (T1, T2, T3)): (t1Collections.immutable.EquaSet[T1], t2Collections.immutable.EquaSet[T2], t3Collections.immutable.EquaSet[T3]) = {
        val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
        (t1Collections.immutable.EquaSet[T1](t1: _*), t2Collections.immutable.EquaSet[T2](t2: _*), t3Collections.immutable.EquaSet[T3](t3: _*))
      }
      def zip[U](that: GenIterable[U]) = underlying.toList.map(_.value).zip(that).toSet
      def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U) = underlying.toList.map(_.value).zipAll(that, thisElem, thatElem).toSet
      def zipWithIndex = underlying.toList.map(_.value).zipWithIndex.toSet
      val path: thisCollections.type = thisCollections
  /*
      def copyInto(thatCollections: Collections[T]): thatCollections.EquaSet = thisTreeEquaSet.into(thatCollections).map(t => t)
      def copyInto(thatCollections: SortedCollections[T]): thatCollections.TreeEquaSet =
        if (thatCollections eq thisCollections)
          thisTreeEquaSet.asInstanceOf[thatCollections.TreeEquaSet]
        else
          thisTreeEquaSet.into(thatCollections).map(t => t)
  */
  
      def view: TreeEquaSetView[T] = TreeEquaSetView(thisTreeEquaSet.toList: _*)
    }
    object SortedEquaSet {
      def empty[T <: E]: immutable.SortedEquaSet[T] = immutable.TreeEquaSet.empty[T]
      def apply[T <: E](elems: T*): immutable.SortedEquaSet[T] = immutable.TreeEquaSet[T](elems: _*)
    }
    object TreeEquaSet {
      def empty[T <: E]: immutable.TreeEquaSet[T] = new immutable.TreeEquaSet[T](TreeSet.empty(ordering))
      def apply[T <: E](elems: T*): immutable.TreeEquaSet[T] = 
        new immutable.TreeEquaSet[T](TreeSet(elems.map(EquaBox[T](_)): _*)(ordering))
    }
  
  /*
    trait SortedEquaMap[V] extends EquaMap[V] {
  
      /**
       * Add a key/value pair to this `SortedEquaMap`, returning a new `SortedEquaMap`.
       * @param kv the key/value pair.
       * @return A new `SortedEquaMap` with the new binding added to this `SortedEquaMap`.
       */
      def +[V1 >: V](kv: (T, V1)): SortedEquaMap[V1]
  
      /**
       * Creates a new `SortedEquaMap` with additional entries.
       *
       * This method takes two or more entries to be added. Another overloaded
       * variant of this method handles the case where a single entry is added.
       *
       * @param entry1 the first entry to add.
       * @param entry2 the second entry to add.
       * @param entries the remaining entries to add.
       * @return a new `SortedEquaMap` with the given entries added.
       */
      def +[V1 >: V](entry1: (T, V1), entry2: (T, V1), entries: (T, V1)*): thisCollections.immutable.SortedEquaMap[V1]
  
      /** Creates a new `SortedEquaMap` by adding all entries contained in another collection to this `SortedEquaMap`.
        *
        *  @param entries     the collection containing the added entries.
        *  @return          a new `SortedEquaMap` with the given entries added.
        */
      def ++[V1 >: V](entries: GenTraversableOnce[(T, V1)]): thisCollections.immutable.SortedEquaMap[V1]
  
      /**
       * Creates a new `SortedEquaMap` by adding entries contained in another `EquaMap`.
       *
       * @param that     the other `EquaMap` containing the added entries.
       * @return         a new `SortedEquaMap` with the given entries added.
       */
      def ++[V1 >: V](that: EquaMap[V1]): thisCollections.immutable.SortedEquaMap[V1]
  
      /**
       * Creates a new `SortedEquaMap` with entry having the given key removed from this `SortedEquaMap`.
       *
       * @param key the key of entry to be removed
       * @return a new `SortedEquaMap` that contains all elements of this `SortedEquaMap` but that does not
       * contain entry with the given `key`.
       */
      def -(key: T): thisCollections.immutable.SortedEquaMap[V]
  
      /**
       * Creates a new `SortedEquaMap` from this `SortedEquaMap` with entries of the given keys removed.
       *
       * This method takes two or more keys of entries to be removed. Another overloaded
       * variant of this method handles the case where a single key of entry is
       * removed.
       * @param key1 the first key of entry to remove.
       * @param key2 the second key of entry to remove.
       * @param keys the remaining keys of entries to remove.
       * @return a new `SortedEquaMap` that contains all entries of the current `SortedEquaMap`
       * except those with given keys.
       */
      def -(key1: T, key2: T, keys: T*): thisCollections.immutable.SortedEquaMap[V]
  
      /**
       * Creates a new `SortedEquaMap` from this `SortedEquaMap` by removing all entries with keys contained in given
       *  collection.
       *
       *  @param keys     the collection containing the keys for entries to remove.
       *  @return a new `SortedEquaMap` that contains all entries of the current `SortedEquaMap`
       *  except entries with keys specified by the given collection.
       */
      def --(keys: GenTraversableOnce[T]): thisCollections.immutable.SortedEquaMap[V]
  
      /**
       * Creates a new `SortedEquaMap` from this `SortedEquaMap` by removing all entries with keys contains in the given `EquaSet`
       *
       * @param equaSet       the `EquaSet` containing keys of entries to be removed.
       * @return a new `SortedEquaMap` that contains all entries of the current `EquaMap` minus entries with keys contained in the passed in `EquaSet`.
       */
      def --(equaSet: thisCollections.immutable.EquaSet): thisCollections.immutable.SortedEquaMap[V]
  
  
      /**
       * Tests if this `SortedEquaMap` is empty.
       *
       * @return `true` if there is no element in the `SortedEquaMap`, `false` otherwise.
       */
      def isEmpty: Boolean
  
      /**
       * Get an instance of `Iterator` for keys of this `SortedEquaMap`.
       *
       * @return an instance of `Iterator` for keys of this `SortedEquaMap`
       */
      def keysIterator: Iterator[T]
  
      /**
       * Get an instance of `Iterator` for values of this `SortedEquaMap`.
       *
       * @return an instance of `Iterator` for values of this `SortedEquaMap`
       */
      def valuesIterator: Iterator[V]
  
      /**
       * The size of this `SortedEquaMap`.
       *
       * @return the number of elements in this `SortedEquaMap`.
       */
      def size: Int
  
      override val path: thisCollections.type
  
      /**
       * Converts this `SortedEquaMap` to a `Map`.
       *
       * @return a `Map` containing all entries of this `SortedEquaMap`.
       */
      def toMap: SortedMap[T, V]
  
      /**
       * Converts this `SortedEquaMap` to a `Map` with `EquaBox` as its key type.
       *
       * @return a `Map` containing all entries of this `SortedEquaMap`, with its key boxed in `EquaBox`.
       */
      def toEquaBoxMap: SortedMap[thisCollections.EquaBox, V]
    }
  
    class TreeEquaMap[V] private[scalactic] (private val underlying: TreeMap[EquaBox, V]) extends SortedEquaMap[V] { thisTreeEquaSet =>
      def +[V1 >: V](kv: (T, V1)): TreeEquaMap[V1] = new immutable.TreeEquaMap(underlying + (EquaBox(kv._1) -> kv._2))
      def +[V1 >: V](entry1: (T, V1), entry2: (T, V1), entries: (T, V1)*): TreeEquaMap[V1] =
        new immutable.TreeEquaMap(underlying + (EquaBox(entry1._1) -> entry1._2, EquaBox(entry2._1) -> entry2._2, entries.map(e => EquaBox(e._1) -> e._2): _*))
      def ++[V1 >: V](entries: GenTraversableOnce[(T, V1)]): TreeEquaMap[V1] =
        new immutable.TreeEquaMap(underlying ++ entries.toSeq.map(e => (EquaBox(e._1) -> e._2)))
      def ++[V1 >: V](that: EquaMap[V1]): TreeEquaMap[V1] = new immutable.TreeEquaMap(underlying ++ that.toEquaBoxMap)
      def -(key: T): TreeEquaMap[V] = new immutable.TreeEquaMap(underlying - EquaBox(key))
      def -(key1: T, key2: T, keys: T*): TreeEquaMap[V] = new immutable.TreeEquaMap(underlying - (EquaBox(key1), EquaBox(key2), keys.map(EquaBox(_)): _*))
      def --(keys: GenTraversableOnce[T]): TreeEquaMap[V] =
        new immutable.TreeEquaMap(underlying -- keys.toSeq.map(EquaBox(_)))
      def --(equaSet: thisCollections.immutable.EquaSet): TreeEquaMap[V] =
        new immutable.TreeEquaMap(underlying -- equaSet.toEquaBoxSet)
      def /:[R](z: R)(op: (R, (T, V)) => R): R =
        underlying.toSeq.map(e => (e._1.value, e._2))./:(z)((r: R, e: (T, V)) => op(r, e))
      def isEmpty: Boolean = underlying.isEmpty
      def keysIterator: Iterator[T] = underlying.keysIterator.map(_.value)
      def valuesIterator: Iterator[V] = underlying.valuesIterator
      def size: Int = underlying.size
      val path: thisCollections.type = thisCollections
      def toMap: TreeMap[T, V] = {
        val keyOrdering: Ordering[T] =
          new Ordering[T] {
            def compare(a: T, b: T): Int =
              equality.compare(a, b)
          }
        TreeMap(underlying.map(e => (e._1.value, e._2)).toList: _*)(keyOrdering)
      }
      def toEquaBoxMap: TreeMap[thisCollections.EquaBox, V] = underlying
      def stringPrefix: String = "TreeEquaMap"
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(e => e._1.value + " -> " + e._2).mkString(", ")})"
      override def equals(other: Any): Boolean = {
        other match {
          case thatEquaMap: Collections[_]#EquaMap[_] =>
            (thisCollections.equality eq thatEquaMap.path.equality) && underlying == thatEquaMap.toEquaBoxMap
          case _ => false
        }
      }
    }
  
    object SortedEquaMap {
      def empty[V]: SortedEquaMap[V] = TreeEquaMap.empty[V]
      def apply[V](entries: (T, V)*): SortedEquaMap[V] = new immutable.TreeEquaMap[V](TreeMap(entries.map(e => EquaBox(e._1) -> e._2): _*)(ordering))
    }
    object TreeEquaMap {
      def empty[V]: TreeEquaMap[V] = new immutable.TreeEquaMap(TreeMap.empty[EquaBox, V](ordering))
      def apply[V](entries: (T, V)*): TreeEquaMap[V] = new immutable.TreeEquaMap[V](TreeMap(entries.map(e => EquaBox(e._1) -> e._2): _*)(ordering))
    }
  */
  }
  override val immutable: SortedImmutable = new SortedImmutable
  type SortedEquaSet[T <: E] = immutable.SortedEquaSet[T]
  lazy val SortedEquaSet = immutable.SortedEquaSet
  type TreeEquaSet[T <: E] = immutable.TreeEquaSet[T]
  lazy val TreeEquaSet = immutable.TreeEquaSet
}

object SortedCollections {

  def apply[T](equality: OrderingEquality[T]): SortedCollections[T] = new SortedCollections(equality)

  def native[T](implicit ordering: Ordering[T]): SortedCollections[T] = 
    SortedCollections[T] {
      new OrderingEquality[T] {
        def compare(a: T, b: T): Int = ordering.compare(a, b) // Need to deal with arrays somehow
        def areEqual(a: T, b: Any): Boolean = Equality.default.areEqual(a, b)
        def hashCodeFor(a: T): Int =
          a match {
            case arr: Array[_] => arr.deep.##
            case _ => a.##
          }
      }
    }
}

