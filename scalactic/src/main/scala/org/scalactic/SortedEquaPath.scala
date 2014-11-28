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

class SortedEquaPath[T](override val equality: OrderingEquality[T]) extends EquaPath[T](equality) { thisEquaPath =>

  val ordering: Ordering[thisEquaPath.EquaBox] =
    new Ordering[thisEquaPath.EquaBox] {
      def compare(a: thisEquaPath.EquaBox, b: thisEquaPath.EquaBox): Int =
        equality.compare(a.value, b.value)
    }

  class SortedEquaBridge[S](from: List[S]) extends EquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaPath.SortedEquaSet =
      thisEquaPath.SortedEquaSet.empty ++ (from collect pf)
    override def map(f: S => T): thisEquaPath.SortedEquaSet =
      thisEquaPath.SortedEquaSet.empty ++ (from map f)
    override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet =
      thisEquaPath.SortedEquaSet((from flatMap ((s: S) => f(s).toList)): _*)
    override def flatten(implicit cvt: S <:< thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet =
      flatMap((s: S) => cvt(s))
    override def scanLeft(z: T)(op: (T, S) => T): thisEquaPath.SortedEquaSet =
      thisEquaPath.SortedEquaSet(from.scanLeft(z)((t: T, s: S) => op(t, s)).toSeq: _*)
    override def scanRight(z: T)(op: (S, T) => T): thisEquaPath.SortedEquaSet =
      thisEquaPath.SortedEquaSet(from.scanRight(z)((s: S, t: T) => op(s, t)).toSeq: _*)
    override def filter(pred: S => Boolean): thisEquaPath.SortedEquaBridge[S] =
      new thisEquaPath.SortedEquaBridge(from.filter(pred))
    override def withFilter(pred: S => Boolean): SortedWithFilter = new SortedWithFilter(pred)

    /**
     * A class supporting filtered operations. Instances of this class are
     * returned by method `withFilter`.
     */
    class SortedWithFilter(p: S => Boolean) extends WithFilter(p) {

      /**
       * Builds a new `SortedEquaSet` by applying a function to all elements of the
       * outer `SortedEquaBridge` containing this `SortedWithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function to apply to each element.
       * @return a new `SortedEquaSet` resulting from applying
       * the given function `f` to each element of the outer `SortedEquaBridge`
       * that satisfies predicate `p` and collecting the results.
       *
       * @return a new `SortedEquaSet` resulting from applying the given function
       * `f` to each element of the outer `SortedEquaBridge` that satisfies
       * predicate `p` and collecting the results.
       */
      override def map(f: S => T): thisEquaPath.SortedEquaSet =
        filter(p).map(f)

      /**
       * Builds a new `SortedEquaSet` by applying a function to all elements of the
       * outer `SortedEquaBridge` containing this `SortedWithFilter` instance that satisfy
       * predicate `p` and concatenating the results.
       *
       * @param f the function to apply to each element.
       * @return a new `SortedEquaSet` resulting from applying
       * the given `EquaSet`-valued function `f` to each element
       * of the outer `SortedEquaBridge` that satisfies predicate `p` and
       * concatenating the results.
       *
       * @return a new `SortedEquaSet` resulting from applying the given
       * `EquaSet`-valued function `f` to each element of the
       * outer `SortedEquaBridge` that satisfies predicate `p` and concatenating
       * the results.
       */
      override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet =
        filter(p).flatMap(f)

      /**
       * Further refines the filter for this `SortedEquaBridge`.
       *
       * @param q the predicate used to test elements.
       * @return an object of class `WithFilter`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `SortedEquaBridge` which
       * satisfy the predicate `q` in addition to the predicate `p`.
       */
      override def withFilter(q: S => Boolean): SortedWithFilter =
        new SortedWithFilter(x => p(x) && q(x))
    }
  }

  class TreeEquaBridge[S](from: List[S]) extends SortedEquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaPath.TreeEquaSet =
      thisEquaPath.TreeEquaSet.empty ++ (from collect pf)
    override def map(f: S => T): thisEquaPath.TreeEquaSet =
      thisEquaPath.TreeEquaSet.empty ++ (from map f)
    override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      thisEquaPath.TreeEquaSet((from flatMap ((s: S) => f(s).toList)): _*)
    override def flatten(implicit cvt: S <:< thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      flatMap((s: S) => cvt(s))
    override def scanLeft(z: T)(op: (T, S) => T): thisEquaPath.TreeEquaSet =
      thisEquaPath.TreeEquaSet(from.scanLeft(z)((t: T, s: S) => op(t, s)).toSeq: _*)
    override def scanRight(z: T)(op: (S, T) => T): thisEquaPath.TreeEquaSet =
      thisEquaPath.TreeEquaSet(from.scanRight(z)((s: S, t: T) => op(s, t)).toSeq: _*)
    override def filter(pred: S => Boolean): thisEquaPath.TreeEquaBridge[S] =
      new thisEquaPath.TreeEquaBridge(from.filter(pred))
    override def withFilter(pred: S => Boolean): TreeWithFilter = new TreeWithFilter(pred)

    /**
     * A class supporting filtered operations. Instances of this class are
     * returned by method `withFilter`.
     */
    class TreeWithFilter(p: S => Boolean) extends SortedWithFilter(p) {

      /**
       * Builds a new `TreeEquaSet` by applying a function to all elements of the
       * outer `TreeEquaBridge` containing this `TreeWithFilter` instance that satisfy predicate `p`.
       *
       * @param f the function to apply to each element.
       * @return a new `TreeEquaSet` resulting from applying
       * the given function `f` to each element of the outer `TreeEquaBridge`
       * that satisfies predicate `p` and collecting the results.
       *
       * @return a new `TreeEquaSet` resulting from applying the given function
       * `f` to each element of the outer `TreeEquaBridge` that satisfies
       * predicate `p` and collecting the results.
       */
      override def map(f: S => T): thisEquaPath.TreeEquaSet =
        filter(p).map(f)

      /**
       * Builds a new `TreeEquaSet` by applying a function to all elements of the
       * outer `TreeEquaBridge` containing this `TreeWithFilter` instance that satisfy
       * predicate `p` and concatenating the results.
       *
       * @param f the function to apply to each element.
       * @return a new `TreeEquaSet` resulting from applying
       * the given `EquaSet`-valued function `f` to each element
       * of the outer `TreeEquaBridge` that satisfies predicate `p` and
       * concatenating the results.
       *
       * @return a new `TreeEquaSet` resulting from applying the given
       * `EquaSet`-valued function `f` to each element of the
       * outer `TreeEquaBridge` that satisfies predicate `p` and concatenating
       * the results.
       */
      override def flatMap(f: S => thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
        filter(p).flatMap(f)

      /**
       * Further refines the filter for this `TreeEquaBridge`.
       *
       * @param q the predicate used to test elements.
       * @return an object of class `TreeWithFilter`, which supports
       * `map`, `flatMap`, `foreach`, and `withFilter` operations.
       * All these operations apply to those elements of this `TreeEquaBridge` which
       * satisfy the predicate `q` in addition to the predicate `p`.
       */
      override def withFilter(q: S => Boolean): TreeWithFilter =
        new TreeWithFilter(x => p(x) && q(x))
    }
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
    def + (elem: T): thisEquaPath.SortedEquaSet

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
    def + (elem1: T, elem2: T, elems: T*): thisEquaPath.SortedEquaSet

    /** Creates a new `SortedEquaSet` by adding all elements contained in another collection to this `SortedEquaSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `SortedEquaSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquaPath.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` by adding elements contained in another `EquaSet`.
     *
     * @param that     the other `EquaSet` containing the added elements.
     * @return         a new `SortedEquaSet` with the given elements added.
     */
    def ++ (that: EquaSet): thisEquaPath.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` with a given element removed from this `SortedEquaSet`.
     *
     * @param elem the element to be removed
     * @return a new `SortedEquaSet` that contains all elements of this `SortedEquaSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquaPath.SortedEquaSet

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
    def - (elem1: T, elem2: T, elems: T*): thisEquaPath.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `SortedEquaSet` that contains all elements of the current `SortedEquaSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquaPath.SortedEquaSet

    /**
     * Creates a new `SortedEquaSet` from this `SortedEquaSet` by removing all elements of another `EquaSet`
     *
     * @param that       the other `EquaSet` containing the removed elements.
     * @return a new `SortedEquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
     */
    def --(that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

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
     *  @return  the result of inserting `op` between consecutive elements of this `SortedEquaSet`,
     *           going left to right with the start value `z` on the left:
     *           {{{
     *             op(...op(op(z, x_1), x_2), ..., x_n)
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this `SortedEquaSet`.
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
     *  @return  the result of inserting `op` between consecutive elements of this `SortedEquaSet`,
     *           going right to left with the start value `z` on the right:
     *           {{{
     *             op(x_1, op(x_2, ... op(x_n, z)...))
     *           }}}
     *           where `x,,1,,, ..., x,,n,,` are the elements of this `SortedEquaSet`.
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
    def | (that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def & (that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

    /**
     * The difference of this `SortedEquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def &~ (that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

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
    def collect(pf: PartialFunction[T, T]): thisEquaPath.SortedEquaSet

    def contains[U](elem: U)(implicit ev: U <:< T): Boolean

    /**
     * Computes the difference of this `SortedEquaSet` and another `SortedEquaSet`.
     *
     * @param that the `EquaSet` of elements to exclude.
     * @return a `SortedEquaSet` containing those elements of this
     * `SortedEquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def diff(that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

    /**
     * Selects all elements except first ''n'' ones.
     *
     * @param n the number of elements to drop from this `SortedEquaSet`.
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the first `n` ones, or else the
     * empty `SortedEquaSet`, if this `EquaSet` has less than `n` elements.
     */
    def drop(n: Int): thisEquaPath.SortedEquaSet

    /** Selects all elements except last ''n'' ones.
      *
      * @param n The number of elements to take
      * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet` except the last `n` ones, or else the
      * empty `SortedEquaSet`, if this `SortedEquaSet` has less than `n` elements.
      */
    def dropRight(n: Int): thisEquaPath.SortedEquaSet

    /**
     * Drops longest prefix of elements that satisfy a predicate.
     *
     * @param pred The predicate used to test elements.
     * @return the longest suffix of this `SortedEquaSet` whose first element
     * does not satisfy the predicate `p`.
     */
    def dropWhile(pred: T => Boolean): thisEquaPath.SortedEquaSet

    /**
     * Selects all elements of this `SortedEquaSet` which satisfy a predicate.
     *
     * @param pred the predicate used to test elements.
     * @return a new `SortedEquaSet` consisting of all elements of this `SortedEquaSet` that satisfy the given
     * predicate <code>pred</code>.
     */
    def filter(pred: T => Boolean): thisEquaPath.SortedEquaSet

    /**
     * Selects all elements of this `SortedEquaPath` which do not satisfy a predicate.
     *
     * @param pred the predicate used to test elements.
     * @return a new `SortedEquaPath` consisting of all elements of this `SortedEquaPath` that do not satisfy the given
     * predicate <code>pred</code>.
     */
    def filterNot(pred: T => Boolean): thisEquaPath.SortedEquaSet

    /**
     * Builds a new `SortedEquaSet` by applying a function to all elements of this `SortedEquaSet`
     * and using the elements of the resulting `EquaSet`.
     *
     * @param f the function to apply to each element.
     * @return a new `SortedEquaSet` resulting from applying the given `EquaSet`-valued function
     * `f` to each element of this `SortedEquaSet` and concatenating the results.
     *
     * For example:
     *
     * {{{
     * def getWords(lines: EquaSet[String]): EquaSet[String] = lines flatMap (line => equaSets.SortedEquaSet(line.split("\\W+"): _*))
     * }}}
     *
     * @return a new `SortedEquaSet` resulting from applying the given `EquaSet`-valued function
     * `f` to each element of this `SortedEquaSet` and concatenating the results.
     */
    def flatMap(f: T => thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

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
    def groupBy[K](f: T => K): GenMap[K, thisEquaPath.SortedEquaSet]

    /**
     * Partitions elements in fixed size `SortedEquaSet`s.
     * @see [[scala.collection.Iterator]], method `grouped`
     *
     * @param size the number of elements per group
     * @return An iterator producing `SortedEquaSet`s of size `size`, except the
     * last will be less than size `size` if the elements don't divide evenly.
     */
    def grouped(size: Int): Iterator[thisEquaPath.SortedEquaSet]

    /**
     * Selects all elements except the last.
     *
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
     * except the last one.
     * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
     */
    def init: thisEquaPath.SortedEquaSet

    /**
     * Iterates over the inits of this `SortedEquaSet`. The first value will be this
     * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
     * values the results of successive applications of `init`.
     *
     * @return an iterator over all the inits of this `SortedEquaSet`
     * @example SortedEquaSet(1,2,3).inits = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(1,2), SortedEquaSet(1), SortedEquaSet())
     */
    def inits: Iterator[thisEquaPath.SortedEquaSet]

    /**
     * Computes the intersection between this `SortedEquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` to intersect with.
     * @return a new `SortedEquaSet` consisting of all elements that are both in this
     * `SortedEquaSet` and in the given `EquaSet` `that`.
     */
    def intersect(that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

    /**
     * Make an `EquaBridge` between this `SortedEquaSet` and the given `thatEquaPath`.
     * `EquaBridge` enables this `SortedEquaSet` to transform into `thatEquaPath`.`EquaSet`
     * through `collect`, `map`, `flatMap`, `flatten`, `scanLeft`, `scanRight`.
     *
     * @param thatEquaPath that `EquaPath` to bridge to
     * @tparam U the type of `thatEquaPath`
     * @return an instance of `thatEquaPath`.`EquaBridge`
     */
    def into[U](thatEquaPath: EquaPath[U]): thatEquaPath.EquaBridge[T]

    /**
     * Make a `SortedEquaBridge` between this `SortedEquaSet` and the given `thatEquaPath`.
     * `SortedEquaBridge` enables this `SortedEquaSet` to transform into `thatEquaPath`.`SortedEquaSet`
     * through `collect`, `map`, `flatMap`, `flatten`, `scanLeft`, `scanRight`.
     *
     * @param thatEquaPath that `SortedEquaPath` to bridge to
     * @tparam U the type of `thatEquaPath`
     * @return an instance of `thatEquaPath`.`SortedEquaBridge`
     */
    def into[U](thatEquaPath: SortedEquaPath[U]): thatEquaPath.SortedEquaBridge[T]

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
     * Builds a new `SortedEquaSet` by applying a function to all elements of this `SortedEquaSet`.
     *
     * @param f the function to apply to each element.
     * @return a new `SortedEquaSet` resulting from applying the given function
     * `f` to each element of this `SortedEquaSet` and collecting the results.
     *
     * @return a new `SortedEquaSet` resulting from applying the given function
     * `f` to each element of this `SortedEquaSet` and collecting the results.
     */
    def map(f: T => T): thisEquaPath.SortedEquaSet

    /**
     * Partitions this `SortedEquaSet` in two `SortedEquaSet`s according to a predicate.
     *
     * @param pred the predicate on which to partition.
     * @return a pair of `SortedEquaSet`s: the first `SortedEquaSet` consists of all elements that
     * satisfy the predicate `p` and the second `SortedEquaSet` consists of all elements
     * that don't. The relative order of the elements in the resulting `SortedEquaSet`s
     * may not be preserved.
     */
    def partition(pred: T => Boolean): (thisEquaPath.SortedEquaSet, thisEquaPath.SortedEquaSet)

    /**
     * The `SortedSet[EquaBox]` underlying this `SortedEquaSet` object.
     */
    def repr: SortedSet[EquaBox]

    /**
     * Produces a collection containing cumulative results of applying the
     * operator going left to right.
     *
     * @param z the initial value
     * @param op the binary operator applied to the intermediate result and the element
     * @return `SortedEquaSet` with intermediate results
     */
    def scanLeft(z: T)(op: (T, T) => T): thisEquaPath.SortedEquaSet

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
    def scanRight(z: T)(op: (T, T) => T): thisEquaPath.SortedEquaSet

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
    def slice(unc_from: Int, unc_until: Int): thisEquaPath.SortedEquaSet

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
    def sliding(size: Int): Iterator[thisEquaPath.SortedEquaSet]

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
    def sliding(size: Int, step: Int): Iterator[thisEquaPath.SortedEquaSet]

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
    def span(pred: T => Boolean): (thisEquaPath.SortedEquaSet, thisEquaPath.SortedEquaSet)

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
    def splitAt(n: Int): (thisEquaPath.SortedEquaSet, thisEquaPath.SortedEquaSet)

    /**
     * An iterator over all subsets of this set of the given size.
     * If the requested size is impossible, an empty iterator is returned.
     *
     * @param len the size of the subsets.
     * @return the iterator.
     */
    def subsets(len: Int): Iterator[thisEquaPath.SortedEquaSet]

    /**
     * An iterator over all subsets of this set.
     *
     * @return the iterator.
     */
    def subsets: Iterator[thisEquaPath.SortedEquaSet]

    /**
     * Selects all elements except the first.
     *
     * @return a `SortedEquaSet` consisting of all elements of this `SortedEquaSet`
     * except the first one.
     * @throws `UnsupportedOperationException` if the `SortedEquaSet` is empty.
     */
    def tail: thisEquaPath.SortedEquaSet

    /**
     * Iterates over the tails of this `SortedEquaSet`. The first value will be this
     * `SortedEquaSet` and the final one will be an empty `SortedEquaSet`, with the intervening
     * values the results of successive applications of `tail`.
     *
     * @return an iterator over all the tails of this `SortedEquaSet`
     * @example `SortedEquaSet(1,2,3).tails = Iterator(SortedEquaSet(1,2,3), SortedEquaSet(2,3), SortedEquaSet(3), SortedEquaSet())`
     */
    def tails: Iterator[thisEquaPath.SortedEquaSet]

    /**
     * Selects first ''n'' elements.
     *
     * @param n the number of elements to take from this `SortedEquaSet`.
     * @return a `SortedEquaSet` consisting only of the first `n` elements of this `SortedEquaSet`,
     * or else the whole `SortedEquaSet`, if it has less than `n` elements.
     */
    def take(n: Int): thisEquaPath.SortedEquaSet

    /**
     * Selects last ''n'' elements.
     *
     *
     * @param n the number of elements to take
     * @return a `SortedEquaSet` consisting only of the last `n` elements of this `SortedEquaSet`, or else the
     * whole `SortedEquaSet`, if it has less than `n` elements.
     */
    def takeRight(n: Int): thisEquaPath.SortedEquaSet

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
    def toEquaBoxSet: SortedSet[thisEquaPath.EquaBox]

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
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaPath.SortedEquaSet

    /**
     * Computes the union between of set and another set.
     *
     * @param that the set to form the union with.
     * @return a new set consisting of all elements that are in this
     * set or in the given set `that`.
     */
    def union(that: thisEquaPath.EquaSet): thisEquaPath.SortedEquaSet

    override val path: thisEquaPath.type

    def copyInto(thatEquaPath: EquaPath[T]): thatEquaPath.EquaSet

    def copyInto(thatEquaPath: SortedEquaPath[T]): thatEquaPath.SortedEquaSet
  }

  class TreeEquaSet private[scalactic] (private val underlying: TreeSet[EquaBox]) extends SortedEquaSet { thisTreeEquaSet =>

    def + (elem: T): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying + EquaBox(elem))
    def + (elem1: T, elem2: T, elems: T*): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying + (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying ++ elems.toSeq.map(EquaBox(_)))
    def ++ (that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying ++ that.toEquaBoxSet)
    def - (elem: T): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying - EquaBox(elem))
    def - (elem1: T, elem2: T, elems: T*): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying - (EquaBox(elem1), EquaBox(elem2), elems.map(EquaBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying -- elems.toSeq.map(EquaBox(_)))
    def --(that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying -- that.toEquaBoxSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquaBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquaBox, b: B) => op(e.value, b))
    def | (that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet = this union that
    def & (that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet = this intersect that
    def &~ (that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.toList.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.toList.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.toList.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquaBox(elem))
    // What this one is saying is that two different EquaPath instances can contain equal EquaPath so
    // long as the Equality discriminator is the same instance.
    def canEqual(that: Any): Boolean =
      that match {
        case thatEquaSet: EquaPath[_]#EquaSet => thatEquaSet.path.equality eq thisEquaPath.equality
        case _ => false
      }
      // that.isInstanceOf[thisEquaPath.EquaSet] && equality == that.asInstanceOf[thisEquaPath.EquaSet].path.equality
    def collect(pf: PartialFunction[T, T]): thisEquaPath.TreeEquaSet = {
      implicit val ord: Ordering[thisEquaPath.EquaBox] = ordering
      new TreeEquaSet(underlying collect { case hb: thisEquaPath.EquaBox if pf.isDefinedAt(hb.value) => EquaBox(pf(hb.value)) })
    }
    def contains[U](elem: U)(implicit ev: U <:< T): Boolean = underlying.contains(EquaBox(elem))
    def copyToArray(xs: Array[thisEquaPath.EquaBox]): Unit = underlying.copyToArray(xs)
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int): Unit = underlying.copyToArray(xs, start)
    def copyToArray(xs: Array[thisEquaPath.EquaBox], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)
    def copyToBuffer(dest: mutable.Buffer[thisEquaPath.EquaBox]): Unit = underlying.copyToBuffer(dest)
    def count(p: T => Boolean): Int = underlying.map(_.value).count(p)
    def diff(that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying diff that.toEquaBoxSet)
    def drop(n: Int): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.drop(n))
    def dropRight(n: Int): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.dropRight(n))
    def dropWhile(pred: T => Boolean): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.dropWhile((p: EquaBox) => pred(p.value)))
    // Two EquaPath whose containing EquaPath have identical equalities can be equal
    override def equals(other: Any): Boolean =
      other match {
        case thatEquaSet: EquaPath[_]#EquaSet => 
          (thisEquaPath.equality eq thatEquaSet.path.equality) && underlying == thatEquaSet.toEquaBoxSet
        case _ => false
      }
/*
      other match {
        case equaSet: thisEquaPath.EquaSet => 
          underlying == equaSet.toSet
        case _ => false
      }
*/
    def exists(pred: T => Boolean): Boolean = underlying.exists((box: EquaBox) => pred(box.value))
    def filter(pred: T => Boolean): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.filter((box: EquaBox) => pred(box.value)))
    def filterNot(pred: T => Boolean): thisEquaPath.SortedEquaSet = new TreeEquaSet(underlying.filterNot((box: EquaBox) => pred(box.value)))
    def find(pred: T => Boolean): Option[T] = underlying.find((box: EquaBox) => pred(box.value)).map(_.value)
    def flatMap(f: T => thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet = {
      val set = underlying.flatMap((box: EquaBox) => f(box.value).toEquaBoxList)
      new TreeEquaSet(TreeSet(set.toList: _*)(ordering))
    }
    def fold[T1 >: T](z: T1)(op: (T1, T1) => T1): T1 = underlying.toList.map(_.value).fold[T1](z)(op)
    def foldLeft[B](z: B)(op: (B, T) => B): B = underlying.toList.map(_.value).foldLeft[B](z)(op)
    def foldRight[B](z: B)(op: (T, B) => B): B = underlying.toList.map(_.value).foldRight[B](z)(op)
    def forall(pred: T => Boolean): Boolean = underlying.toList.map(_.value).forall(pred)
    def foreach[U](f: T => U): Unit = underlying.toList.map(_.value).foreach(f)
    def groupBy[K](f: T => K): GenMap[K, thisEquaPath.TreeEquaSet] = underlying.groupBy((box: EquaBox) => f(box.value)).map(t => (t._1, new TreeEquaSet(t._2)))
    def grouped(size: Int): Iterator[thisEquaPath.TreeEquaSet] = underlying.grouped(size).map(new TreeEquaSet(_))
    def hasDefiniteSize: Boolean = underlying.hasDefiniteSize
    override def hashCode: Int = underlying.hashCode
    def head: T = underlying.head.value
    def headOption: Option[T] =
      underlying.headOption match {
        case Some(head) => Some(head.value)
        case None => None
      }
    def init: thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.init)
    def inits: Iterator[thisEquaPath.TreeEquaSet] = underlying.inits.map(new TreeEquaSet(_))
    def intersect(that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying intersect that.toEquaBoxSet)
    def into[U](thatEquaPath: EquaPath[U]): thatEquaPath.EquaBridge[T] = new thatEquaPath.EquaBridge[T](underlying.toList.map(_.value))
    def into[U](thatEquaPath: SortedEquaPath[U]): thatEquaPath.TreeEquaBridge[T] = new thatEquaPath.TreeEquaBridge[T](underlying.toList.map(_.value))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def last: T = underlying.last.value
    def lastOption: Option[T] =
      underlying.lastOption match {
        case Some(last) => Some(last.value)
        case None => None
      }
    def map(f: T => T): thisEquaPath.TreeEquaSet = TreeEquaSet(underlying.map((box: EquaBox) => f(box.value)).toList: _*)
    def max[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).max(ord)
    def maxBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).maxBy(f)
    def min[T1 >: T](implicit ord: Ordering[T1]): T = underlying.toList.map(_.value).min(ord)
    def minBy[B](f: T => B)(implicit cmp: Ordering[B]): T = underlying.toList.map(_.value).minBy(f)
    def mkString(start: String, sep: String, end: String): String = underlying.toList.map(_.value).mkString(start, sep, end)
    def mkString(sep: String): String = underlying.toList.map(_.value).mkString(sep)
    def mkString: String = underlying.toList.map(_.value).mkString
    def nonEmpty: Boolean = underlying.nonEmpty
    def partition(pred: T => Boolean): (thisEquaPath.TreeEquaSet, thisEquaPath.TreeEquaSet) = {
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
    def scanLeft(z: T)(op: (T, T) => T): thisEquaPath.TreeEquaSet = {
      val set = underlying.scanLeft(EquaBox(z))((b1: EquaBox, b2: EquaBox) => EquaBox(op(b1.value, b2.value)))
      new TreeEquaSet(TreeSet(set.toList: _*)(ordering))
    }
    def scanRight(z: T)(op: (T, T) => T): thisEquaPath.TreeEquaSet = {
      val set = underlying.scanRight(EquaBox(z))((b1: EquaBox, b2: EquaBox) => EquaBox(op(b1.value, b2.value)))
      new TreeEquaSet(TreeSet(set.toList: _*)(ordering))
    }
    def size: Int = underlying.size
    def slice(unc_from: Int, unc_until: Int): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.slice(unc_from, unc_until))
    def sliding(size: Int): Iterator[thisEquaPath.TreeEquaSet] = underlying.sliding(size).map(new TreeEquaSet(_))
    def sliding(size: Int, step: Int): Iterator[thisEquaPath.TreeEquaSet] = underlying.sliding(size, step).map(new TreeEquaSet(_))
    def span(pred: T => Boolean): (thisEquaPath.TreeEquaSet, thisEquaPath.TreeEquaSet) = {
      val (trueSet, falseSet) = underlying.span((box: EquaBox) => pred(box.value))
      (new TreeEquaSet(trueSet), new TreeEquaSet(falseSet))
    }
    def splitAt(n: Int): (thisEquaPath.TreeEquaSet, thisEquaPath.TreeEquaSet) = {
      val (trueSet, falseSet) = underlying.splitAt(n)
      (new TreeEquaSet(trueSet), new TreeEquaSet(falseSet))
    }
    def stringPrefix: String = "TreeEquaSet"
    def subsetOf(that: thisEquaPath.EquaSet): Boolean = underlying.subsetOf(that.toEquaBoxSet)
    def subsets(len: Int): Iterator[thisEquaPath.TreeEquaSet] = underlying.subsets(len).map(new TreeEquaSet(_))
    def subsets: Iterator[thisEquaPath.TreeEquaSet] = underlying.subsets.map(new TreeEquaSet(_))
    def sum[T1 >: T](implicit num: Numeric[T1]): T1 = underlying.map(_.value).sum(num)
    def tail: thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.tail)
    def tails: Iterator[thisEquaPath.TreeEquaSet] = underlying.tails.map(new TreeEquaSet(_))
    def take(n: Int): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.take(n))
    def takeRight(n: Int): thisEquaPath.TreeEquaSet = new TreeEquaSet(underlying.takeRight(n))
    def to[Col[_]](implicit cbf: CanBuildFrom[Nothing, thisEquaPath.EquaBox, Col[thisEquaPath.EquaBox @uV]]): Col[thisEquaPath.EquaBox @uV] = underlying.to[Col]
    def toArray: Array[T] = {
      // A workaround becauase underlying.map(_.value).toArray does not work due to this weird error message:
      // No ClassTag available for T
      val arr = new Array[Any](underlying.size)
      underlying.map(_.value).copyToArray(arr)
      arr.asInstanceOf[Array[T]]
    }
    def toEquaBoxArray: Array[thisEquaPath.EquaBox] = underlying.toArray
    def toBuffer: scala.collection.mutable.Buffer[T] = underlying.map(_.value).toBuffer
    def toEquaBoxBuffer: scala.collection.mutable.Buffer[thisEquaPath.EquaBox] = underlying.toBuffer
    def toIndexedSeq: scala.collection.immutable.IndexedSeq[T] = underlying.map(_.value).toIndexedSeq
    def toEquaBoxIndexedSeq: scala.collection.immutable.IndexedSeq[thisEquaPath.EquaBox] = underlying.toIndexedSeq
    def toIterable: GenIterable[T] = underlying.toIterable.map(_.value)
    def toEquaBoxIterable: GenIterable[thisEquaPath.EquaBox] = underlying.toIterable
    def toIterator: Iterator[T] = underlying.toIterator.map(_.value)
    def toEquaBoxIterator: Iterator[thisEquaPath.EquaBox] = underlying.toIterator
    def toEquaBoxList: List[thisEquaPath.EquaBox] = underlying.toList
    def toList: List[T] = underlying.toList.map(_.value)
    def toMap[K, V](implicit ev: T <:< (K, V)): Map[K, V] = underlying.map(_.value).toMap
    def toParArray: ParArray[T] = underlying.toParArray.map(_.value)
    def toEquaBoxParArray: ParArray[thisEquaPath.EquaBox] = underlying.toParArray
    def toSeq: GenSeq[T] = underlying.toSeq.map(_.value)
    def toEquaBoxSeq: GenSeq[thisEquaPath.EquaBox] = underlying.toSeq
    def toSet: TreeSet[T] = {
      val valueOrdering: Ordering[T] =
        new Ordering[T] {
          def compare(a: T, b: T): Int =
            equality.compare(a, b)
        }
      TreeSet(underlying.map(_.value).toList: _*)(valueOrdering)
    }
    def toEquaBoxSet: TreeSet[thisEquaPath.EquaBox] = underlying
    def toStream: Stream[T] = underlying.toStream.map(_.value)
    def toEquaBoxStream: Stream[thisEquaPath.EquaBox] = underlying.toStream
    def toTraversable: GenTraversable[T] = underlying.map(_.value)
    def toEquaBoxTraversable: GenTraversable[thisEquaPath.EquaBox] = underlying.toTraversable
    def toVector: Vector[T] = underlying.toVector.map(_.value)
    def toEquaBoxVector: Vector[thisEquaPath.EquaBox] = underlying.toVector
    override def toString: String = s"$stringPrefix(${underlying.toVector.map(_.value).mkString(", ")})"
    def transpose[B](implicit asTraversable: T => GenTraversableOnce[B]): thisEquaPath.TreeEquaSet = {
      val listList: List[T] = underlying.toList.map(_.value).transpose.asInstanceOf[List[T]]  // should be safe cast
      new TreeEquaSet(TreeSet(listList.map(EquaBox(_)): _ *)(ordering))
    }
    def union(that: thisEquaPath.EquaSet): thisEquaPath.TreeEquaSet =
      new TreeEquaSet(underlying union that.toEquaBoxSet)
    def unzip[T1, T2](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2])(implicit asPair: T => (T1, T2)): (t1EquaPath.EquaSet, t2EquaPath.EquaSet) = {
      val (t1, t2) =  underlying.toList.map(_.value).unzip(asPair)
      (t1EquaPath.EquaSet(t1: _*), t2EquaPath.EquaSet(t2: _*))
    }
    def unzip3[T1, T2, T3](t1EquaPath: EquaPath[T1], t2EquaPath: EquaPath[T2], t3EquaPath: EquaPath[T3])(implicit asTriple: T => (T1, T2, T3)): (t1EquaPath.EquaSet, t2EquaPath.EquaSet, t3EquaPath.EquaSet) = {
      val (t1, t2, t3) =  underlying.toList.map(_.value).unzip3(asTriple)
      (t1EquaPath.EquaSet(t1: _*), t2EquaPath.EquaSet(t2: _*), t3EquaPath.EquaSet(t3: _*))
    }
    def view = underlying.toList.toSet.view
    def view(from: Int, until: Int) = underlying.toList.toSet.view(from, until)
    def zip[U](that: GenIterable[U]) = underlying.toList.map(_.value).zip(that).toSet
    def zipAll[U, T1 >: T](that: GenIterable[U], thisElem: T1, thatElem: U) = underlying.toList.map(_.value).zipAll(that, thisElem, thatElem).toSet
    def zipWithIndex = underlying.toList.map(_.value).zipWithIndex.toSet
    val path: thisEquaPath.type = thisEquaPath
    def copyInto(thatEquaPath: EquaPath[T]): thatEquaPath.EquaSet = thisTreeEquaSet.into(thatEquaPath).map(t => t)
    def copyInto(thatEquaPath: SortedEquaPath[T]): thatEquaPath.TreeEquaSet =
      if (thatEquaPath eq thisEquaPath)
        thisTreeEquaSet.asInstanceOf[thatEquaPath.TreeEquaSet]
      else
        thisTreeEquaSet.into(thatEquaPath).map(t => t)
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

object SortedEquaPath {
  def apply[T](equality: OrderingEquality[T]): SortedEquaPath[T] = new SortedEquaPath(equality)
}

