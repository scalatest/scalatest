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
import scala.reflect.ClassTag

class SortedCollections[E](override val equality: OrderingEquality[E]) extends Collections[E](equality) { thisCollections =>

  def ordering[T <: E]: Ordering[thisCollections.Box[T]] =
    new Ordering[thisCollections.Box[T]] {
      def compare(a: thisCollections.Box[T], b: thisCollections.Box[T]): Int =
        equality.compare(a.value, b.value)
    }

  class SortedImmutable extends Immutable {

    trait SortedSet[+T <: E] extends Set[T] {
  
      /**
       * Creates a new `SortedSet` with an additional element, unless the element is
       * already present.
       *
       * @param elem the element to be added
       * @return a new `SortedSet` that contains all elements of this `SortedSet` and that also
       * contains `elem`.
       */
      def +[U >: T <: E](elem: U): thisCollections.immutable.SortedSet[U]
  
      /**
       * Creates a new `SortedSet` with additional elements.
       *
       * This method takes two or more elements to be added. Another overloaded
       * variant of this method handles the case where a single element is added.
       *
       * @param elem1 the first element to add.
       * @param elem2 the second element to add.
       * @param elems the remaining elements to add.
       * @return a new `SortedSet` with the given elements added.
       */
      def +[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.SortedSet[U]
  
      /** Creates a new `SortedSet` by adding all elements contained in another collection to this `SortedSet`.
        *
        *  @param elems     the collection containing the added elements.
        *  @return          a new `SortedSet` with the given elements added.
        */
      def ++[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Creates a new `SortedSet` by adding elements contained in another `Set`.
       *
       * @param that     the other `Set` containing the added elements.
       * @return         a new `SortedSet` with the given elements added.
       */
      def ++[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Creates a new `SortedSet` with a given element removed from this `SortedSet`.
       *
       * @param elem the element to be removed
       * @return a new `SortedSet` that contains all elements of this `SortedSet` but that does not
       * contain `elem`.
       */
      def -[U >: T <: E](elem: U): thisCollections.immutable.SortedSet[U]
  
      /* * USE LATER
       * Creates a new `SortedSet` from this `SortedSet` by removing all elements of another
       * collection.
       *
       * @param xs the collection containing the removed elements.
       * @return a new `SortedSet` that contains all elements of the current `SortedSet`
       * except one less occurrence of each of the elements of `elems`.
       */
  
      /**
       * Creates a new `SortedSet` from this `SortedSet` with some elements removed.
       *
       * This method takes two or more elements to be removed. Another overloaded
       * variant of this method handles the case where a single element is
       * removed.
       * @param elem1 the first element to remove.
       * @param elem2 the second element to remove.
       * @param elems the remaining elements to remove.
       * @return a new `SortedSet` that contains all elements of the current `SortedSet`
       * except one less occurrence of each of the given elements.
       */
      def -[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.SortedSet[U]
  
      /**
       * Creates a new `SortedSet` from this `SortedSet` by removing all elements of another
       *  collection.
       *
       *  @param elems     the collection containing the removed elements.
       *  @return a new `SortedSet` that contains all elements of the current `SortedSet`
       *  except one less occurrence of each of the elements of `elems`.
       */
      def --[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Creates a new `SortedSet` from this `SortedSet` by removing all elements of another `Set`
       *
       * @param that       the other `Set` containing the removed elements.
       * @return a new `SortedSet` that contains all elements of the current `Set` minus elements contained in the passed in `Set`.
       */
      def --[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Builds a new collection by applying a partial function to all elements of this `SortedSet`
       * on which the function is defined.
       *
       * @param pf the partial function which filters and maps the `SortedSet`.
       * @return a new collection of type `That` resulting from applying the partial function
       * `pf` to each element on which it is defined and collecting the results.
       * The order of the elements is preserved.
       *
       * @return a new `SortedSet` resulting from applying the given partial function
       * `pf` to each element on which it is defined and collecting the results.
       * The order of the elements is preserved.
       */
      def collect[U >: T <: E](pf: PartialFunction[T, U]): thisCollections.immutable.SortedSet[U]

      def contains[U >: T <: E](elem: U): Boolean

      /**
       * Computes the difference of this `SortedSet` and another `SortedSet`.
       *
       * @param that the `Set` of elements to exclude.
       * @return a `SortedSet` containing those elements of this
       * `SortedSet` that are not also contained in the given `Set` `that`.
       */
      def diff[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Selects all elements except first ''n'' ones.
       *
       * @param n the number of elements to drop from this `SortedSet`.
       * @return a `SortedSet` consisting of all elements of this `SortedSet` except the first `n` ones, or else the
       * empty `SortedSet`, if this `Set` has less than `n` elements.
       */
      def drop(n: Int): thisCollections.immutable.SortedSet[T]
  
      /** Selects all elements except last ''n'' ones.
        *
        * @param n The number of elements to take
        * @return a `SortedSet` consisting of all elements of this `SortedSet` except the last `n` ones, or else the
        * empty `SortedSet`, if this `SortedSet` has less than `n` elements.
        */
      def dropRight(n: Int): thisCollections.immutable.SortedSet[T]
  
      /**
       * Drops longest prefix of elements that satisfy a predicate.
       *
       * @param pred The predicate used to test elements.
       * @return the longest suffix of this `SortedSet` whose first element
       * does not satisfy the predicate `p`.
       */
      def dropWhile(pred: T => Boolean): thisCollections.immutable.SortedSet[T]
  
      /**
       * Selects all elements of this `SortedSet` which satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `SortedSet` consisting of all elements of this `SortedSet` that satisfy the given
       * predicate <code>pred</code>.
       */
      def filter(pred: T => Boolean): thisCollections.immutable.SortedSet[T]
  
      /**
       * Selects all elements of this `SortedCollections` which do not satisfy a predicate.
       *
       * @param pred the predicate used to test elements.
       * @return a new `SortedCollections` consisting of all elements of this `SortedCollections` that do not satisfy the given
       * predicate <code>pred</code>.
       */
      def filterNot(pred: T => Boolean): thisCollections.immutable.SortedSet[T]
  
      /**
       * Partitions this `SortedSet` into a map of `SortedSet`s according to some discriminator function.
       *
       * Note: this method is not re-implemented by views. This means
       * when applied to a view it will always force the view and
       * return a new `SortedSet`.
       *
       * @param f the discriminator function.
       * @tparam K the type of keys returned by the discriminator function.
       * @return A map from keys to `SortedSet`s such that the following invariant holds:
       * {{{
       * (xs groupBy f)(k) = xs filter (x => f(x) == k)
       * }}}
       * That is, every key `k` is bound to a `SortedSet` of those elements `x`
       * for which `f(x)` equals `k`.
       *
       */
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.SortedSet[T]]
  
      /**
       * Partitions elements in fixed size `SortedSet`s.
       * @see [[scala.collection.Iterator]], method `grouped`
       *
       * @param size the number of elements per group
       * @return An iterator producing `SortedSet`s of size `size`, except the
       * last will be less than size `size` if the elements don't divide evenly.
       */
      def grouped(size: Int): Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Selects all elements except the last.
       *
       * @return a `SortedSet` consisting of all elements of this `SortedSet`
       * except the last one.
       * @throws `UnsupportedOperationException` if the `SortedSet` is empty.
       */
      def init: thisCollections.immutable.SortedSet[T]
  
      /**
       * Iterates over the inits of this `SortedSet`. The first value will be this
       * `SortedSet` and the final one will be an empty `SortedSet`, with the intervening
       * values the results of successive applications of `init`.
       *
       * @return an iterator over all the inits of this `SortedSet`
       * @example SortedSet(1,2,3).inits = Iterator(SortedSet(1,2,3), SortedSet(1,2), SortedSet(1), SortedSet())
       */
      def inits: Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Computes the intersection between this `SortedSet` and another `Set`.
       *
       * @param that the `Set` to intersect with.
       * @return a new `SortedSet` consisting of all elements that are both in this
       * `SortedSet` and in the given `Set` `that`.
       */
      def intersect[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.SortedSet[U]
  
      /**
       * Tests if this `SortedSet` is empty.
       *
       * @return `true` if there is no element in the set, `false` otherwise.
       */
      def isEmpty: Boolean
  
      /**
       * Get an instance of `Iterator` for elements of this `SortedSet`.
       *
       * @return an instance of `Iterator` for elements of this `SortedSet`
       */
      def iterator: Iterator[T]
  
      /**
       * Partitions this `SortedSet` in two `SortedSet`s according to a predicate.
       *
       * @param pred the predicate on which to partition.
       * @return a pair of `SortedSet`s: the first `SortedSet` consists of all elements that
       * satisfy the predicate `p` and the second `SortedSet` consists of all elements
       * that don't. The relative order of the elements in the resulting `SortedSet`s
       * may not be preserved.
       */
      def partition(pred: T => Boolean): (thisCollections.immutable.SortedSet[T], thisCollections.immutable.SortedSet[T])
  
      /* *
       * Produces a collection containing cumulative results of applying the
       * operator going left to right.
       *
       * @param z the initial value
       * @param op the binary operator applied to the intermediate result and the element
       * @return `SortedSet` with intermediate results
       */
      // def scanLeft(z: T)(op: (T, T) => T): thisCollections.immutable.SortedSet[T]
  
      /* *
       * Produces a collection containing cumulative results of applying the operator going right to left.
       * The head of the collection is the last cumulative result.
       *
       * Example:
       * {{{
       * `SortedSet`(1, 2, 3, 4).scanRight(0)(_ + _) == `SortedSet`(10, 9, 7, 4, 0)
       * }}}
       *
       * @param z the initial value
       * @param op the binary operator applied to the intermediate result and the element
       * @return `SortedSet` with intermediate results
       */
      // def scanRight(z: T)(op: (T, T) => T): thisCollections.immutable.SortedSet[T]
  
      /**
       * The size of this `SortedSet`.
       *
       * @return the number of elements in this `SortedSet`.
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
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.SortedSet[T]
  
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
      def sliding(size: Int): Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Groups elements in fixed size blocks by passing a "sliding window"
       * over them (as opposed to partitioning them, as is done in grouped.)
       * @see [[scala.collection.Iterator]], method `sliding`
       *
       * @param size the number of elements per group
       * @param step the distance between the first elements of successive
       * groups (defaults to 1)
       * @return An iterator producing `SortedSet`s of size `size`, except the
       * last and the only element will be truncated if there are
       * fewer elements than size.
       */
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Splits this `SortedSet` into a prefix/suffix pair according to a predicate.
       *
       * Note: `c span p` is equivalent to (but possibly more efficient than)
       * `(c takeWhile p, c dropWhile p)`, provided the evaluation of the
       * predicate `p` does not cause any side-effects.
       *
       *
       * @param pred the test predicate
       * @return a pair consisting of the longest prefix of this `SortedSet` whose
       * elements all satisfy `p`, and the rest of this `SortedSet`.
       */
      def span(pred: T => Boolean): (thisCollections.immutable.SortedSet[T], thisCollections.immutable.SortedSet[T])
  
      /**
       * Splits this `SortedSet` into two at a given position.
       * Note: `c splitAt n` is equivalent to (but possibly more efficient than)
       * `(c take n, c drop n)`.
       *
       *
       * @param n the position at which to split.
       * @return a pair of `SortedSet`s consisting of the first `n`
       * elements of this `SortedSet`, and the other elements.
       */
      def splitAt(n: Int): (thisCollections.immutable.SortedSet[T], thisCollections.immutable.SortedSet[T])
  
      /**
       * An iterator over all subsets of this set of the given size.
       * If the requested size is impossible, an empty iterator is returned.
       *
       * @param len the size of the subsets.
       * @return the iterator.
       */
      def subsets(len: Int): Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * An iterator over all subsets of this set.
       *
       * @return the iterator.
       */
      def subsets: Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Selects all elements except the first.
       *
       * @return a `SortedSet` consisting of all elements of this `SortedSet`
       * except the first one.
       * @throws `UnsupportedOperationException` if the `SortedSet` is empty.
       */
      def tail: thisCollections.immutable.SortedSet[T]
  
      /**
       * Iterates over the tails of this `SortedSet`. The first value will be this
       * `SortedSet` and the final one will be an empty `SortedSet`, with the intervening
       * values the results of successive applications of `tail`.
       *
       * @return an iterator over all the tails of this `SortedSet`
       * @example `SortedSet(1,2,3).tails = Iterator(SortedSet(1,2,3), SortedSet(2,3), SortedSet(3), SortedSet())`
       */
      def tails: Iterator[thisCollections.immutable.SortedSet[T]]
  
      /**
       * Selects first ''n'' elements.
       *
       * @param n the number of elements to take from this `SortedSet`.
       * @return a `SortedSet` consisting only of the first `n` elements of this `SortedSet`,
       * or else the whole `SortedSet`, if it has less than `n` elements.
       */
      def take(n: Int): thisCollections.immutable.SortedSet[T]
  
      /**
       * Selects last ''n'' elements.
       *
       *
       * @param n the number of elements to take
       * @return a `SortedSet` consisting only of the last `n` elements of this `SortedSet`, or else the
       * whole `SortedSet`, if it has less than `n` elements.
       */
      def takeRight(n: Int): thisCollections.immutable.SortedSet[T]
  
      /**
       * Converts this `SortedSet` to a set.
       *
       * @return a set containing all elements of this `SortedSet`.
       */
      def toSet[U >: T <: E]: scala.collection.immutable.SortedSet[U]
  
      /**
       * Converts this `SortedSet` to a set of `Box`.
       *
       * @return a set containing all elements of this `SortedSet`, boxed in `Box`.
       */
      def toBoxSet[U >: T <: E]: scala.collection.immutable.SortedSet[thisCollections.Box[U]]
  
      /**
       * Transposes this `SortedSet` of traversable collections into
       * a `SortedSet` of `GenTraversableOnce`s.
       *
       * The resulting collection's type will be guided by the
       * static type of `Set`. For example:
       *
       * {{{
       * val xs = SortedSet(
       * List(1, 2, 3),
       * List(4, 5, 6)).transpose
       * // xs == SortedSet(
       * // List(1, 4),
       * // List(2, 5),
       * // List(3, 6))
       *
       * val ys = SortedSet(
       * List(1, 2, 3),
       * List(4, 5, 6)).transpose
       * // ys == SortedSet(
       * // Vector(1, 4),
       * // Vector(2, 5),
       * // Vector(3, 6))
       * }}}
       *
       * @tparam B the type of the elements of each traversable collection.
       * @param asTraversable an implicit conversion which asserts that the
       * element type of this `SortedSet` is a `Traversable`.
       * @return a two-dimensional `SortedSet` of ${coll}s which has as ''n''th row
       * the ''n''th column of this `SortedSet`.
       * @throws `IllegalArgumentException` if all collections in this `SortedSet`
       * are not of the same size.
       */
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.SortedSet[T]
  
      /**
       * Computes the union between of set and another set.
       *
       * @param that the set to form the union with.
       * @return a new set consisting of all elements that are in this
       * set or in the given set `that`.
       */
      def union[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.SortedSet[U]
  
      override val path: thisCollections.type
  
  /*
      def copyInto(thatCollections: Collections[T]): thatCollections.Set
  
      def copyInto(thatCollections: SortedCollections[T]): thatCollections.SortedSet
  */
      def view: SortedSetView[T]
    }

    class TreeSet[+T <: E] private[scalactic] (private val underlying: scala.collection.immutable.TreeSet[Box[T@uV]]) extends SortedSet[T] { thisTreeSet =>

      def +[U >: T <: E](elem: U): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] = underlying.map(ebt => (ebt: Box[U])) + Box[U](elem)
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def +[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) + (Box[U](elem1), Box[U](elem2), elems.map(Box[U](_)): _*)
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def ++[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) ++ elems.toSeq.map(Box[U](_))
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def ++[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) ++ that.toBoxSet
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def -[U >: T <: E](elem: U): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] = underlying.map(ebt => (ebt: Box[U])) - Box[U](elem)
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def -[U >: T <: E](elem1: U, elem2: U, elems: U*): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) - (Box[U](elem1), Box[U](elem2), elems.map(Box[U](_)): _*)
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def --[U >: T <: E](elems: GenTraversableOnce[U]): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) -- elems.toSeq.map(Box[U](_))
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def --[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] =
          underlying.map(ebt => (ebt: Box[U])) -- that.toBoxSet
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
      def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
      def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
      def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: Box[T]) => seqop(b, e.value), combop)
      // What this one is saying is that two different Collections instances can contain equal Collections so
      // long as the Equality discriminator is the same instance.
      def canEqual(that: Any): Boolean =
        that match {
          case thatSet: Collections[_]#Immutable#Set[_] => thatSet.path.equality eq thisCollections.equality
          case _ => false
        }
        // that.isInstanceOf[thisCollections.immutable.Set] && equality == that.asInstanceOf[thisCollections.immutable.Set].path.equality
      def collect[U >: T <: E](pf: PartialFunction[T, U]): thisCollections.immutable.TreeSet[U] = {
        val setOfBoxOfU: scala.collection.immutable.Set[SortedCollections.this.Box[U]] = underlying collect { case hb: thisCollections.Box[T] if pf.isDefinedAt(hb.value) => Box[U](pf(hb.value)) }
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]](setOfBoxOfU.toList: _*)(ordering[U]))
      }
      def contains[U >: T <: E](elem: U): Boolean = underlying.toList.contains(Box[U](elem))
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]]): Unit = underlying.copyToArray(xs)
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int): Unit = underlying.copyToArray(xs, start)
      def copyToArray[U >: T <: E](xs: Array[thisCollections.Box[U]], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
      def copyToBuffer[U >: T <: E](dest: mutable.Buffer[thisCollections.Box[U]]): Unit = underlying.copyToBuffer(dest)
      def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
      def diff[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.TreeSet[U] =
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]]((underlying.map(ebt => ebt: Box[U]) diff that.toBoxSet).toList: _*)(ordering[U]))
      def drop(n: Int): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.drop(n))
      def dropRight(n: Int): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.dropRight(n))
      def dropWhile(pred: T => Boolean): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.dropWhile((p: Box[T]) => pred(p.value)))
      // Two Collections whose containing Collections have identical equalities can be equal
      override def equals(other: Any): Boolean =
        other match {
          case thatSet: Collections[E]#Immutable#Set[T] => 
            (thisCollections.equality eq thatSet.path.equality) && underlying == thatSet.toBoxSet
          case _ => false
        }
  /*
        other match {
          case equaSet: thisCollections.immutable.Set => 
            underlying == equaSet.toSet
          case _ => false
        }
  */
      def exists(pred: T => Boolean): Boolean = underlying.exists((box: Box[T]) => pred(box.value))
      def filter(pred: T => Boolean): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.filter((box: Box[T]) => pred(box.value)))
      def filterNot(pred: T => Boolean): thisCollections.immutable.SortedSet[T] = new immutable.TreeSet[T](underlying.filterNot((box: Box[T]) => pred(box.value)))
      def find(pred: T => Boolean): Option[T] = underlying.find((box: Box[T]) => pred(box.value)).map(_.value)
      def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
      def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
      def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
      def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
      def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
      def groupBy[K](f: T => K): GenMap[K, thisCollections.immutable.TreeSet[T]] = underlying.groupBy((box: Box[T]) => f(box.value)).map(t => (t._1, new immutable.TreeSet[T](t._2)))
      def grouped(size: Int): Iterator[thisCollections.immutable.TreeSet[T]] = underlying.grouped(size).map(new immutable.TreeSet[T](_))
      def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
      override def hashCode: Int = underlying.hashCode
      def head: T = underlying.head.value
      def headOption: Option[T] =
        underlying.headOption match {
          case Some(head) => Some(head.value)
          case None => None
        }
      def init: thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.init)
      def inits: Iterator[thisCollections.immutable.TreeSet[T]] = underlying.inits.map(new immutable.TreeSet[T](_))
      def intersect[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.TreeSet[U] =
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]]((underlying.map(ebt => ebt: Box[U]) intersect that.toBoxSet).toList: _*)(ordering[U]))
      def isEmpty: Boolean = underlying.isEmpty
      def iterator: Iterator[T] = underlying.iterator.map(_.value)
      def last: T = underlying.last.value
      def lastOption: Option[T] =
        underlying.lastOption match {
          case Some(last) => Some(last.value)
          case None => None
        }
      def max[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).max(ord)
      def membership[U >: T <: E]: Membership[U] = new Membership[U]((a: U) => thisTreeSet.toList.exists(ele => equality.areEqual(ele, a)))
      def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).maxBy(f)
      def min[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).min(ord)
      def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).minBy(f)
      def mkString(start: String, sep: String, end: String): String = underlying.toList.map(_.value).mkString(start, sep, end)
      def mkString(sep: String): String = underlying.toList.map(_.value).mkString(sep)
      def mkString: String = underlying.toList.map(_.value).mkString
      def nonEmpty: Boolean = underlying.nonEmpty
      def partition(pred: T => Boolean): (thisCollections.immutable.TreeSet[T], thisCollections.immutable.TreeSet[T]) = {
        val tuple2 = underlying.partition((box: Box[T]) => pred(box.value))
        (new immutable.TreeSet[T](tuple2._1), new immutable.TreeSet[T](tuple2._2))
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
      def scanLeft(z: T)(op: (T, T) => T): thisCollections.immutable.TreeSet[T] = {
        val set = underlying.scanLeft(Box[T](z))((b1: Box[T], b2: Box[T]) => Box[T](op(b1.value, b2.value)))
        new immutable.TreeSet[T](TreeSet(set.toList: _*)(ordering))
      }
      def scanRight(z: T)(op: (T, T) => T): thisCollections.immutable.TreeSet[T] = {
        val set = underlying.scanRight(Box[T](z))((b1: Box[T], b2: Box[T]) => Box[T](op(b1.value, b2.value)))
        new immutable.TreeSet[T](TreeSet(set.toList: _*)(ordering))
      }
*/
      def size: Int = underlying.size
      def slice(unc_from: Int, unc_until: Int): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.slice(unc_from, unc_until))
      def sliding(size: Int): Iterator[thisCollections.immutable.TreeSet[T]] = underlying.sliding(size).map(new immutable.TreeSet[T](_))
      def sliding(size: Int, step: Int): Iterator[thisCollections.immutable.TreeSet[T]] = underlying.sliding(size, step).map(new immutable.TreeSet[T](_))
      def span(pred: T => Boolean): (thisCollections.immutable.TreeSet[T], thisCollections.immutable.TreeSet[T]) = {
        val (trueSet, falseSet) = underlying.span((box: Box[T]) => pred(box.value))
        (new immutable.TreeSet[T](trueSet), new immutable.TreeSet[T](falseSet))
      }
      def splitAt(n: Int): (thisCollections.immutable.TreeSet[T], thisCollections.immutable.TreeSet[T]) = {
        val (trueSet, falseSet) = underlying.splitAt(n)
        (new immutable.TreeSet[T](trueSet), new immutable.TreeSet[T](falseSet))
      }
      def stringPrefix: String = "TreeSet"
      def subsetOf[U >: T <: E](that: thisCollections.immutable.Set[U]): Boolean = underlying.map(ebt => ebt: Box[U]).subsetOf(that.toBoxSet)
      def subsets(len: Int): Iterator[thisCollections.immutable.TreeSet[T]] = underlying.subsets(len).map(new immutable.TreeSet[T](_))
      def subsets: Iterator[thisCollections.immutable.TreeSet[T]] = underlying.subsets.map(new immutable.TreeSet[T](_))
      def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
      def tail: thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.tail)
      def tails: Iterator[thisCollections.immutable.TreeSet[T]] = underlying.tails.map(new immutable.TreeSet[T](_))
      def take(n: Int): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.take(n))
      def takeRight(n: Int): thisCollections.immutable.TreeSet[T] = new immutable.TreeSet[T](underlying.takeRight(n))
      def toArray[U >: T <: E](implicit ct: ClassTag[U]): Array[U] = {
        // A workaround becauase underlying.map(_.value).toArray does not work due to this weird error message:
        // No ClassTag available for T
        // val arr: Array[Any] = new Array[Any](underlying.size)
        // underlying.map(_.value).copyToArray(arr)
        // arr.asInstanceOf[Array[T]]
        underlying.map(_.value).toArray
      }
      def toBoxArray[U >: T <: E]: Array[thisCollections.Box[U]] = underlying.toArray
      def toBuffer[U >: T <: E]: scala.collection.mutable.Buffer[U] = underlying.map(_.value).toBuffer
      def toBoxBuffer[U >: T <: E]: scala.collection.mutable.Buffer[thisCollections.Box[U]] = underlying.toBuffer
      def toIndexedSeq: scala.collection.immutable.IndexedSeq[T] = underlying.map(_.value).toIndexedSeq
      def toBoxIndexedSeq: scala.collection.immutable.IndexedSeq[thisCollections.Box[T]] = underlying.toIndexedSeq
      def toIterable: GenIterable[T] = underlying.toIterable.map(_.value)
      def toBoxIterable: GenIterable[thisCollections.Box[T]] = underlying.toIterable
      def toIterator: Iterator[T] = underlying.toIterator.map(_.value)
      def toBoxIterator: Iterator[thisCollections.Box[T]] = underlying.toIterator
      def toBoxList: List[thisCollections.Box[T]] = underlying.toList
      def toList: List[T] = underlying.toList.map(_.value)
      def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.map(_.value).toMap
      def toParArray[U >: T <: E]: ParArray[U] = underlying.toParArray.map(_.value)
      def toBoxParArray[U >: T <: E]: ParArray[thisCollections.Box[U]] = underlying.toList.map(ebt => ebt: Box[U]).toParArray
      def toSeq: GenSeq[T] = underlying.toSeq.map(_.value)
      def toBoxSeq: GenSeq[thisCollections.Box[T]] = underlying.toSeq
      def toSet[U >: T <: E]: scala.collection.immutable.TreeSet[U] = {
        val valueOrdering: Ordering[U] =
          new Ordering[U] {
            def compare(a: U, b: U): Int =
              equality.compare(a, b)
          }
        scala.collection.immutable.TreeSet[U](underlying.map(_.value).toList: _*)(valueOrdering)
      }
      def toBoxSet[U >: T <: E]: scala.collection.immutable.TreeSet[thisCollections.Box[U]] = scala.collection.immutable.TreeSet[Box[U]](underlying.map(ebt => (ebt: Box[U])).toList: _*)(ordering[U])
      def toStream: Stream[T] = underlying.toStream.map(_.value)
      def toBoxStream: Stream[thisCollections.Box[T]] = underlying.toStream
      def toTraversable: GenTraversable[T] = underlying.map(_.value)
      def toBoxTraversable: GenTraversable[thisCollections.Box[T]] = underlying.toTraversable
      def toVector: Vector[T] = underlying.toVector.map(_.value)
      def toBoxVector: Vector[thisCollections.Box[T]] = underlying.toVector
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
      def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisCollections.immutable.TreeSet[T] = {
        val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
        new immutable.TreeSet[T](scala.collection.immutable.TreeSet(listList.map(Box[T](_)): _ *)(ordering))
      }
      def union[U >: T <: E](that: thisCollections.immutable.Set[U]): thisCollections.immutable.TreeSet[U] =
        new immutable.TreeSet[U](scala.collection.immutable.TreeSet[Box[U]]((underlying.map(ebt => ebt: Box[U]) union that.toBoxSet).toList: _*)(ordering[U]))
/*
      def unzip[T1, T2](t1Collections: Collections[T1], t2Collections: Collections[T2])(implicit asPair: T => (T1, T2)): (t1Collections.immutable.Set[T1], t2Collections.immutable.Set[T2]) = {
        val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
        (t1Collections.immutable.Set[T1](t1: _*), t2Collections.immutable.Set[T2](t2: _*))
      }
      def unzip3[T1, T2, T3](t1Collections: Collections[T1], t2Collections: Collections[T2], t3Collections: Collections[T3])(implicit asTriple: T => (T1, T2, T3)): (t1Collections.immutable.Set[T1], t2Collections.immutable.Set[T2], t3Collections.immutable.Set[T3]) = {
        val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
        (t1Collections.immutable.Set[T1](t1: _*), t2Collections.immutable.Set[T2](t2: _*), t3Collections.immutable.Set[T3](t3: _*))
      }
      def zip[U](that: GenIterable[U]) = underlying.toList.map(_.value).zip(that).toSet
      def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U) = underlying.toList.map(_.value).zipAll(that, thisElem, thatElem).toSet
      def zipWithIndex = underlying.toList.map(_.value).zipWithIndex.toSet
*/
      val path: thisCollections.type = thisCollections
  /*
      def copyInto(thatCollections: Collections[T]): thatCollections.Set = thisTreeSet.into(thatCollections).map(t => t)
      def copyInto(thatCollections: SortedCollections[T]): thatCollections.TreeSet =
        if (thatCollections eq thisCollections)
          thisTreeSet.asInstanceOf[thatCollections.TreeSet]
        else
          thisTreeSet.into(thatCollections).map(t => t)
  */

      def view: TreeSetView[T] = TreeSetView(thisTreeSet.toList: _*)
    }
    object SortedSet {
      def empty[T <: E]: immutable.SortedSet[T] = immutable.TreeSet.empty[T]
      def apply[T <: E](elems: T*): immutable.SortedSet[T] = immutable.TreeSet[T](elems: _*)
    }
    object TreeSet {
      def empty[T <: E]: immutable.TreeSet[T] = new immutable.TreeSet[T](scala.collection.immutable.TreeSet.empty(ordering))
      def apply[T <: E](elems: T*): immutable.TreeSet[T] = 
        new immutable.TreeSet[T](scala.collection.immutable.TreeSet(elems.map(Box[T](_)): _*)(ordering))
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
       * Creates a new `SortedEquaMap` from this `SortedEquaMap` by removing all entries with keys contains in the given `Set`
       *
       * @param equaSet       the `Set` containing keys of entries to be removed.
       * @return a new `SortedEquaMap` that contains all entries of the current `EquaMap` minus entries with keys contained in the passed in `Set`.
       */
      def --(equaSet: thisCollections.immutable.Set): thisCollections.immutable.SortedEquaMap[V]
  
  
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
       * Converts this `SortedEquaMap` to a `Map` with `Box` as its key type.
       *
       * @return a `Map` containing all entries of this `SortedEquaMap`, with its key boxed in `Box`.
       */
      def toBoxMap: SortedMap[thisCollections.Box, V]
    }
  
    class TreeEquaMap[V] private[scalactic] (private val underlying: TreeMap[Box, V]) extends SortedEquaMap[V] { thisTreeSet =>
      def +[V1 >: V](kv: (T, V1)): TreeEquaMap[V1] = new immutable.TreeEquaMap(underlying + (Box(kv._1) -> kv._2))
      def +[V1 >: V](entry1: (T, V1), entry2: (T, V1), entries: (T, V1)*): TreeEquaMap[V1] =
        new immutable.TreeEquaMap(underlying + (Box(entry1._1) -> entry1._2, Box(entry2._1) -> entry2._2, entries.map(e => Box(e._1) -> e._2): _*))
      def ++[V1 >: V](entries: GenTraversableOnce[(T, V1)]): TreeEquaMap[V1] =
        new immutable.TreeEquaMap(underlying ++ entries.toSeq.map(e => (Box(e._1) -> e._2)))
      def ++[V1 >: V](that: EquaMap[V1]): TreeEquaMap[V1] = new immutable.TreeEquaMap(underlying ++ that.toBoxMap)
      def -(key: T): TreeEquaMap[V] = new immutable.TreeEquaMap(underlying - Box(key))
      def -(key1: T, key2: T, keys: T*): TreeEquaMap[V] = new immutable.TreeEquaMap(underlying - (Box(key1), Box(key2), keys.map(Box(_)): _*))
      def --(keys: GenTraversableOnce[T]): TreeEquaMap[V] =
        new immutable.TreeEquaMap(underlying -- keys.toSeq.map(Box(_)))
      def --(equaSet: thisCollections.immutable.Set): TreeEquaMap[V] =
        new immutable.TreeEquaMap(underlying -- equaSet.toBoxSet)
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
      def toBoxMap: TreeMap[thisCollections.Box, V] = underlying
      def stringPrefix: String = "TreeEquaMap"
      override def toString: String = s"$stringPrefix(${underlying.toVector.map(e => e._1.value + " -> " + e._2).mkString(", ")})"
      override def equals(other: Any): Boolean = {
        other match {
          case thatEquaMap: Collections[_]#EquaMap[_] =>
            (thisCollections.equality eq thatEquaMap.path.equality) && underlying == thatEquaMap.toBoxMap
          case _ => false
        }
      }
    }
  
    object SortedEquaMap {
      def empty[V]: SortedEquaMap[V] = TreeEquaMap.empty[V]
      def apply[V](entries: (T, V)*): SortedEquaMap[V] = new immutable.TreeEquaMap[V](TreeMap(entries.map(e => Box(e._1) -> e._2): _*)(ordering))
    }
    object TreeEquaMap {
      def empty[V]: TreeEquaMap[V] = new immutable.TreeEquaMap(TreeMap.empty[Box, V](ordering))
      def apply[V](entries: (T, V)*): TreeEquaMap[V] = new immutable.TreeEquaMap[V](TreeMap(entries.map(e => Box(e._1) -> e._2): _*)(ordering))
    }
  */
  }
  override val immutable: SortedImmutable = new SortedImmutable
  type SortedSet[T <: E] = immutable.SortedSet[T]
  lazy val SortedSet = immutable.SortedSet
  type TreeSet[T <: E] = immutable.TreeSet[T]
  lazy val TreeSet = immutable.TreeSet
}

object SortedCollections {

  def apply[T](implicit equality: OrderingEquality[T]): SortedCollections[T] = new SortedCollections(equality)

/*
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
*/
}

