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
import scala.collection._
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet
import scala.annotation.unchecked.{ uncheckedVariance => uV }

class SortedEquaSets[T](override val equality: OrderingEquality[T]) extends EquaSets[T](equality) { thisEquaSets =>

  val ordering: Ordering[thisEquaSets.EquaBox] =
    new Ordering[thisEquaSets.EquaBox] {
      def compare(a: thisEquaSets.EquaBox, b: thisEquaSets.EquaBox): Int =
        equality.compare(a.value, b.value)
    }

  class SortedEquaBridge[S](from: List[S]) extends EquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaSets.SortedEquaSet =
      thisEquaSets.SortedEquaSet.empty ++ (from collect pf)
  }

  trait SortedEquaSet extends EquaSet {

    /**
     * Creates a new `SortedEquaSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquaSets.SortedEquaSet

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
    def + (elem1: T, elem2: T, elems: T*): thisEquaSets.SortedEquaSet

    /** Creates a new `SortedEquaSet` by adding all elements contained in another collection to this `SortedEquaSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `SortedEquaSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` by adding elements contained in another `EquaSet`.
     *
     * @param that     the other `EquaSet` containing the added elements.
     * @return         a new `SortedEquaSet` with the given elements added.
     */
    def ++ (that: EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` with a given element removed from this `SortedEquaSet`.
     *
     * @param elem the element to be removed
     * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquaSets.SortedEquaSet

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
    def - (elem1: T, elem2: T, elems: T*): thisEquaSets.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` from this $coll by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet

    /**
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another `EquaSet`
     *
     * @param that       the other `EquaSet` containing the removed elements.
     * @return a new `SortedEquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
     */
    def --(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Applies a binary operator to a start value and all elements of this `SortedEquaSet`,
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
     *  @tparam  B    the result type of the binary operator.
     *  @return  the result of inserting `op` between consecutive elements of this $coll,
     *           going left to right with the start value `z` on the left:
     *           {{{
     *             op(...op(op(z, x_1), x_2), ..., x_n)
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
     */
    def /:[B](z: B)(op: (B, T) => B): B

    /**
     * Applies a binary operator to all elements of this `SortedEquaSet` and a start value,
     *  going right to left.
     *
     *  Note: `:\` is alternate syntax for `foldRight`; `xs :\ z` is the same as
     *  `xs foldRight z`.
     *  $willNotTerminateInf
     *  $orderDependentFold
     *
     *  Examples:
     *
     *  Note that the folding function used to compute b is equivalent to that used to compute c.
     *  {{{
     *      scala> val a = List(1,2,3,4)
     *      a: List[Int] = List(1, 2, 3, 4)
     *
     *      scala> val b = (a :\ 5)(_+_)
     *      b: Int = 15
     *
     *      scala> val c = (a :\ 5)((x,y) => x + y)
     *      c: Int = 15
     *
     *  }}}
     *
     *  @param   z    the start value
     *  @param   op   the binary operator
     *  @tparam  B    the result type of the binary operator.
     *  @return  the result of inserting `op` between consecutive elements of this $coll,
     *           going right to left with the start value `z` on the right:
     *           {{{
     *             op(x_1, op(x_2, ... op(x_n, z)...))
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this $coll.
     */
    def :\[B](z: B)(op: (T, B) => B): B

    /**
     * Computes the union between this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquaSet` to form the union with.
     * @return a new `SortedEquaSet` consisting of all elements that are in this
     * `SortedEquaSet` or in the given `EquaSet` `that`.
     */
    def | (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def & (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * The difference of this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

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
    def collect(pf: PartialFunction[T, T]): thisEquaSets.SortedEquaSet

    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet

    /**
     * Computes the difference of this `SortedEquaSet` and another `SortedEquaSet`.
     *
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def diff(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    /**
     * Selects all elements except first ''n'' ones.
     *
     * @param n the number of elements to drop from this `SortedEquaSet`.
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the first `n` ones, or else the
     * empty `SortedEquaSet`, if this `EquaSet` has less than `n` elements.
     */
    def drop(n: Int): thisEquaSets.SortedEquaSet

    /** Selects all elements except last ''n'' ones.
      *
      * @param n The number of elements to take
      * @return a `SortedEquiSet` consisting of all elements of this `SortedEquiSet` except the last `n` ones, or else the
      * empty `SortedEquiSet`, if this `SortedEquiSet` has less than `n` elements.
      */
    def dropRight(n: Int): thisEquaSets.SortedEquaSet

    /**
     * Drops longest prefix of elements that satisfy a predicate.
     *
     * @param pred The predicate used to test elements.
     * @return the longest suffix of this `SortedEquiSet` whose first element
     * does not satisfy the predicate `p`.
     */
    def dropWhile(pred: T => Boolean): thisEquaSets.EquaSet

    /** Selects all elements of this `SortedEquaSet` which satisfy a predicate.
      *
      * @param pred the predicate used to test elements.
      * @return a new `SortedEquaSet` consisting of all elements of this `SortedEquaSet` that satisfy the given
      * predicate <code>pred</code>.
      */
    def filter(pred: T => Boolean): thisEquaSets.SortedEquaSet

    /** Selects all elements of this `SortedEquaSets` which do not satisfy a predicate.
      *
      * @param pred the predicate used to test elements.
      * @return a new `SortedEquaSets` consisting of all elements of this `SortedEquaSets` that do not satisfy the given
      * predicate <code>pred</code>.
      */
    def filterNot(pred: T => Boolean): thisEquaSets.SortedEquaSet

    /**
     * Partitions this $coll into a map of `SortedEquaSet`s according to some discriminator function.
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
    def groupBy[K](f: T => K): GenMap[K, thisEquaSets.SortedEquaSet]

    /**
     * Partitions elements in fixed size `SortedEquaSet`s.
     * @see [[scala.collection.Iterator]], method `grouped`
     *
     * @param size the number of elements per group
     * @return An iterator producing `SortedEquaSet`s of size `size`, except the
     * last will be less than size `size` if the elements don't divide evenly.
     */
    def grouped(size: Int): Iterator[thisEquaSets.SortedEquaSet]

    /**
     * Selects all elements except the last.
     *
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
     * except the last one.
     * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
     */
    def init: thisEquaSets.SortedEquaSet

    /**
     * Iterates over the inits of this `SortedEquaSet`. The first value will be this
     * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
     * values the results of successive applications of `init`.
     *
     * @return an iterator over all the inits of this `SortedEquaSet`
     * @example SortedEquaSet(1,2,3).inits = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(1,2), SortedEquaSet(1), SortedEquaSet())
     */
    def inits: Iterator[thisEquaSets.SortedEquaSet]

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet
    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.EquaBridge[T]
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.SortedEquaBridge[T]
    def isEmpty: Boolean
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
    def partition(pred: T => Boolean): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet)

    /**
     * The collection of type traversable collection underlying this TraversableLike object. By default this is implemented as the TraversableLike object itself, but this can be overridden.
     */
    def repr: SortedSet[EquaBox]

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
     * @return a `EquaSet` containing the elements greater than or equal to
     * index `from` extending up to (but not including) index `until`
     * of this `EquaSet`.
     */
    def slice(unc_from: Int, unc_until: Int): thisEquaSets.SortedEquaSet

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
    def sliding(size: Int): Iterator[thisEquaSets.SortedEquaSet]

    /**
     * Groups elements in fixed size blocks by passing a "sliding window"
     * over them (as opposed to partitioning them, as is done in grouped.)
     * @see [[scala.collection.Iterator]], method `sliding`
     *
     * @param size the number of elements per group
     * @param step the distance between the first elements of successive
     * groups (defaults to 1)
     * @return An iterator producing `SortedEquiSet`s of size `size`, except the
     * last and the only element will be truncated if there are
     * fewer elements than size.
     */
    def sliding(size: Int, step: Int): Iterator[thisEquaSets.SortedEquaSet]

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
    def span(pred: T => Boolean): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet)

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
    def splitAt(n: Int): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet)

    /**
     * An iterator over all subsets of this set of the given size.
     * If the requested size is impossible, an empty iterator is returned.
     *
     * @param len the size of the subsets.
     * @return the iterator.
     */
    def subsets(len: Int): Iterator[thisEquaSets.SortedEquaSet]

    /**
     * An iterator over all subsets of this set.
     *
     * @return the iterator.
     */
    def subsets: Iterator[thisEquaSets.SortedEquaSet]

    /**
     * Selects all elements except the first.
     *
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
     * except the first one.
     * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
     */
    def tail: thisEquaSets.SortedEquaSet

    /**
     * Iterates over the tails of this `SortedEquaSet`. The first value will be this
     * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
     * values the results of successive applications of `tail`.
     *
     * @return an iterator over all the tails of this `SortedEquaSet`
     * @example `SortedEquaSet(1,2,3).tails = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(2,3), SortedEquaSet(3), SortedEquaSet())`
     */
    def tails: Iterator[thisEquaSets.EquaSet]

    /**
     * Selects first ''n'' elements.
     *
     * @param n the number of elements to take from this `EquaSet`.
     * @return a `EquaSet` consisting only of the first `n` elements of this `EquaSet`,
     * or else the whole $coll, if it has less than `n` elements.
     */
    def take(n: Int): thisEquaSets.EquaSet

    /**
     * Selects last ''n'' elements.
     *
     *
     * @param n the number of elements to take
     * @return a `SortedEquaSet` consisting only of the last `n` elements of this `SortedEquaSet`, or else the
     * whole `SortedEquaSet`, if it has less than `n` elements.
     */
    def takeRight(n: Int): thisEquaSets.EquaSet

    /**
     * Converts this `SortedEquaSet` to a set.
     *
     * @return a set containing all elements of this `SortedEquaSet`.
     */
    def toSet: SortedSet[thisEquaSets.EquaBox]

    /**
     * Transposes this `SortedEquaSet` of traversable collections into
     * a `EquaSet` of `SortedEquaSet`s.
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
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaSets.SortedEquaSet

    /**
     * Computes the union between of set and another set.
     *
     * @param that the set to form the union with.
     * @return a new set consisting of all elements that are in this
     * set or in the given set `that`.
     */
    def union(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet

    private[scalactic] override def owner: SortedEquaSets[T] = thisEquaSets
  }

  class TreeEquaBridge[S](from: List[S]) extends SortedEquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaSets.TreeEquaSet =
      thisEquaSets.TreeEquaSet.empty ++ (from collect pf)
  }

  class TreeEquaSet private[scalactic] (private val underlying: TreeSet[EquaBox]) extends SortedEquaSet {

    def + (elem: T): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying + EquaBox(elem))
    def + (elem1: T, elem2: T, elems: T*): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying + (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying ++ elems.toSeq.map(EquaBox(_)))
    def ++ (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying ++ that.toSet)
    def - (elem: T): thisEquaSets.TreeEquaSet = new TreeEquaSet(underlying - EquaBox(elem))
    def - (elem1: T, elem2: T, elems: T*): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying - (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet =
      new TreeEquaSet(underlying -- elems.toSeq.map(EquaBox(_)))
    def --(that: thisEquaSets.EquaSet): thisEquaSets.SortedEquaSet =
      new TreeEquaSet(underlying -- that.toSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquaBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquaBox, b: B) => op(e.value, b))
    def | (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this union that
    def & (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this intersect that
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquaBox(elem))
    def canEqual(that: Any): Boolean = that.isInstanceOf[thisEquaSets.EquaSet] && equality == that.asInstanceOf[thisEquaSets.EquaSet].owner.equality
    def collect(pf: PartialFunction[T, T]): thisEquaSets.SortedEquaSet = {
      implicit val ord: Ordering[thisEquaSets.EquaBox] = ordering
      new TreeEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => EquaBox(pf(hb.value)) })
    }
    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet =
      new thatEquaSets.FastEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) })
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet =
      new thatEquaSets.TreeEquaSet(TreeSet.empty(thatEquaSets.ordering) ++ (underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) }))
    def copyToArray(xs: Array[thisEquaSets.EquaBox]): Unit = underlying.copyToArray(xs)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int): Unit = underlying.copyToArray(xs, start)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
    def copyToBuffer(dest: mutable.Buffer[thisEquaSets.EquaBox]): Unit = underlying.copyToBuffer(dest)
    def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
    def diff(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying diff that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def drop(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.drop(n))
    def dropRight(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.dropRight(n))
    def dropWhile(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.dropWhile((p: EquaBox) => pred(p.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquaSets.EquaSet => 
          underlying == equiSet.toSet
        case _ => false
      }
    def exists(pred: T => Boolean): Boolean = underlying.exists((box: EquaBox) => pred(box.value))
    def filter(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.filter((box: EquaBox) => pred(box.value)))
    def filterNot(pred: T => Boolean): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.filterNot((box: EquaBox) => pred(box.value)))
    def find(pred: T => Boolean): Option[EquaBox] = underlying.find((box: EquaBox) => pred(box.value))
    def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
    def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
    def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
    def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
    def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
    def groupBy[K](f: T => K): GenMap[K, thisEquaSets.SortedEquaSet] = underlying.groupBy((box: EquaBox) => f(box.value)).map(t => (t._1, new TreeEquaSet(t._2)))
    def grouped(size: Int): Iterator[thisEquaSets.SortedEquaSet] = underlying.grouped(size).map(new TreeEquaSet(_))
    def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
    override def hashCode: Int = underlying.hashCode
    def head: T = underlying.head.value
    def headOption: Option[T] =
      underlying.headOption match {
        case Some(head) => Some(head.value)
        case None => None
      }
    def init: thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.init)
    def inits: Iterator[thisEquaSets.SortedEquaSet] = underlying.inits.map(new TreeEquaSet(_))
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying intersect that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.EquaBridge[T] = new thatEquaSets.FastEquaBridge[T](underlying.toList.map(_.value))
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.TreeEquaBridge[T] = new thatEquaSets.TreeEquaBridge[T](underlying.toList.map(_.value))
    def isEmpty: Boolean = underlying.isEmpty
    def isTraversableAgain: Boolean = underlying.isTraversableAgain
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
    def partition(pred: T => Boolean): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet) = {
      val tuple2 = underlying.partition((box: EquaBox) => pred(box.value))
      (new TreeEquaSet(tuple2._1), new TreeEquaSet(tuple2._2))
    }
    def product[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.toList.map(_.value).product(num)
    def reduce[T1 >: T](op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).reduce(op)
    def reduceLeft[T1 >: T](op: (T1, T) => T1): T1 = underlying.toList.map(_.value).reduceLeft(op)
    def reduceLeftOption[T1 >: T](op: (T1, T) => T1): Option[T1] = underlying.toList.map(_.value).reduceLeftOption(op)
    def reduceOption[T1 >: T](op: (T1, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceOption(op)
    def reduceRight[T1 >: T](op: (T, T1) => T1): T1 = underlying.toList.map(_.value).reduceRight(op)
    def reduceRightOption[T1 >: T](op: (T, T1) => T1): Option[T1] = underlying.toList.map(_.value).reduceRightOption(op)
    def repr: SortedSet[EquaBox] = underlying
    def sameElements[T1 >: T](that: GenIterable[T1]): Boolean = underlying.toList.map(_.value).sameElements(that)
    def size: Int = underlying.size
    def slice(unc_from: Int, unc_until: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.slice(unc_from, unc_until))
    def sliding(size: Int): Iterator[thisEquaSets.SortedEquaSet] = underlying.sliding(size).map(new TreeEquaSet(_))
    def sliding(size: Int, step: Int): Iterator[thisEquaSets.SortedEquaSet] = underlying.sliding(size, step).map(new TreeEquaSet(_))
    def span(pred: T => Boolean): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet) = {
      val (trueSet, falseSet) = underlying.span((box: EquaBox) => pred(box.value))
      (new TreeEquaSet(trueSet), new TreeEquaSet(falseSet))
    }
    def splitAt(n: Int): (thisEquaSets.SortedEquaSet, thisEquaSets.SortedEquaSet) = {
      val (trueSet, falseSet) = underlying.splitAt(n)
      (new TreeEquaSet(trueSet), new TreeEquaSet(falseSet))
    }
    def stringPrefix: String = "TreeEquaSet"
    def subsetOf(that: thisEquaSets.EquaSet): Boolean = underlying.subsetOf(that.toSet)
    def subsets(len: Int): Iterator[thisEquaSets.SortedEquaSet] = underlying.subsets(len).map(new TreeEquaSet(_))
    def subsets: Iterator[thisEquaSets.SortedEquaSet] = underlying.subsets.map(new TreeEquaSet(_))
    def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
    def tail: thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.tail)
    def tails: Iterator[thisEquaSets.SortedEquaSet] = underlying.tails.map(new TreeEquaSet(_))
    def take(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.take(n))
    def takeRight(n: Int): thisEquaSets.SortedEquaSet = new TreeEquaSet(underlying.takeRight(n))
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, thisEquaSets.EquaBox, Col[thisEquaSets.EquaBox @uV]]): Col[thisEquaSets.EquaBox @uV] = underlying.to[Col]
    def toArray: Array[EquaBox] = underlying.toArray
    def toBuffer: scala.collection.mutable.Buffer[thisEquaSets.EquaBox] = underlying.toBuffer
    def toIndexedSeq: scala.collection.immutable.IndexedSeq[thisEquaSets.EquaBox] = underlying.toIndexedSeq
    def toIterable: GenIterable[thisEquaSets.EquaBox] = underlying.toIterable
    def toIterator: Iterator[thisEquaSets.EquaBox] = underlying.toIterator
    def toList: List[thisEquaSets.EquaBox] = underlying.toList
    def toSeq: GenSeq[thisEquaSets.EquaBox] = underlying.toSeq
    def toSet: TreeSet[thisEquaSets.EquaBox] = underlying
    def toStream: Stream[thisEquaSets.EquaBox] = underlying.toStream
    def toTraversable: GenTraversable[thisEquaSets.EquaBox] = underlying.toTraversable
    def toVector: Vector[thisEquaSets.EquaBox] = underlying.toVector
    override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaSets.SortedEquaSet = {
      val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
      new TreeEquaSet(TreeSet(listList.map(EquaBox(_)): _ *)(ordering))
    }
    def union(that: thisEquaSets.EquaSet): thisEquaSets.TreeEquaSet =
      new TreeEquaSet(underlying union that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def unzip[T1, T2](t1EquaSets: EquaSets[T1], t2EquaSets: EquaSets[T2])(implicit asPair: T => (T1, T2)): (t1EquaSets.EquaSet, t2EquaSets.EquaSet) = {
      val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
      (t1EquaSets.EquaSet(t1: _*), t2EquaSets.EquaSet(t2: _*))
    }
    def unzip3[T1, T2, T3](t1EquaSets: EquaSets[T1], t2EquaSets: EquaSets[T2], t3EquaSets: EquaSets[T3])(implicit asTriple: T => (T1, T2, T3)): (t1EquaSets.EquaSet, t2EquaSets.EquaSet, t3EquaSets.EquaSet) = {
      val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
      (t1EquaSets.EquaSet(t1: _*), t2EquaSets.EquaSet(t2: _*), t3EquaSets.EquaSet(t3: _*))
    }
  }
  object SortedEquaSet {
    def empty: SortedEquaSet = TreeEquaSet.empty
    def apply(elems: T*): SortedEquaSet = TreeEquaSet(elems: _*)
  }
  object TreeEquaSet {
    def empty: TreeEquaSet = new TreeEquaSet(TreeSet.empty(ordering))
    def apply(elems: T*): TreeEquaSet = 
      new TreeEquaSet(TreeSet(elems.map(EquaBox(_)): _*)(ordering))
  }
}

object SortedEquaSets {
  def apply[T](equality: OrderingEquality[T]): SortedEquaSets[T] = new SortedEquaSets(equality)
}

