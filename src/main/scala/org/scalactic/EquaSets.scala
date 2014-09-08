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

import scala.collection.GenTraversableOnce
import scala.collection.immutable.TreeSet
import scala.collection.immutable.SortedSet
import scala.language.higherKinds

class EquaSets[T](val equality: HashingEquality[T]) { thisEquaSets =>

  case class EquaBox(value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: EquaBox => equality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = equality.hashCodeFor(value)
    override def toString: String = s"EquaBox(${value.toString})"
  }

  class EquaBridge[S](from: List[S]) {
    def collect(pf: PartialFunction[S, T]): thisEquaSets.EquaSet =
      thisEquaSets.FastEquaSet.empty ++ (from collect pf) // { case s if pf.isDefinedAt(s) => pf(s) })
  }

  trait EquaSet extends Function1[T, Boolean] with Equals {

    /**
     * Creates a new `EquaSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `EquaSet` that contains all elements of this `EquaSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquaSets.EquaSet

    /**
     * Creates a new `EquaSet` with additional elements.
     *
     * This method takes two or more elements to be added. Another overloaded
     * variant of this method handles the case where a single element is added.
     *
     * @param elem1 the first element to add.
     * @param elem2 the second element to add.
     * @param elems the remaining elements to add.
     * @return a new `EquaSet` with the given elements added.
     */
    def + (elem1: T, elem2: T, elems: T*): thisEquaSets.EquaSet

    /** Creates a new `EquaSet` by adding all elements contained in another collection to this `EquaSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `EquaSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.EquaSet

    /**
     * Creates a new `EquaSet` by adding elements contained in another `EquaSet`.
     *
     * @param that     the other `EquaSet` containing the added elements.
     * @return         a new `EquaSet` with the given elements added.
     */
    def ++ (that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * Creates a new `EquaSet` with a given element removed from this `EquaSet`.
     *
     * @param elem the element to be removed
     * @return a new `EquaSet` that contains all elements of this `EquaSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquaSets.EquaSet

    /* * USE LATER
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another
     * collection.
     *
     * @param xs the collection containing the removed elements.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet`
     * except one less occurrence of each of the elements of `elems`.
     */

    /**
     * Creates a new `EquaSet` from this `EquaSet` with some elements removed.
     *
     * This method takes two or more elements to be removed. Another overloaded
     * variant of this method handles the case where a single element is
     * removed.
     * @param elem1 the first element to remove.
     * @param elem2 the second element to remove.
     * @param elems the remaining elements to remove.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet`
     * except one less occurrence of each of the given elements.
     */
    def - (elem1: T, elem2: T, elems: T*): thisEquaSets.EquaSet

    /**
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `EquaSet` that contains all elements of the current `EquaSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet

    /**
     * Creates a new `EquaSet` from this `EquaSet` by removing all elements of another `EquaSet`
     *
     * @param that       the other `EquaSet` containing the removed elements.
     * @return a new `EquaSet` that contains all elements of the current `EquaSet` minus elements contained in the passed in `EquaSet`.
     */
    def --(that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * Applies a binary operator to a start value and all elements of this `EquaSet`,
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
     * Applies a binary operator to all elements of this `EquaSet` and a start value,
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
     * Computes the union between this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquaSet` to form the union with.
     * @return a new `EquaSet` consisting of all elements that are in this
     * `EquaSet` or in the given `EquaSet` `that`.
     */
    def | (that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * Computes the intersection between this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquaSet` to intersect with.
     * @return a new `EquaSet` consisting of all elements that are both in this
     * `EquaSet` and in the given `EquaSet` `that`.
     */
    def & (that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * The difference of this `EquaSet` and another `EquaSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquaSet` of elements to exclude.
     * @return a `EquaSet` containing those elements of this
     * `EquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * Appends all elements of this `EquaSet` to a string builder.
     *  The written text consists of the string representations (w.r.t. the method
     * `toString`) of all elements of this `EquaSet` without any separator string.
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
     * Appends all elements of this `EquaSet` to a string builder using a separator string.
     *  The written text consists of the string representations (w.r.t. the method `toString`)
     *  of all elements of this $coll, separated by the string `sep`.
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

    /** Appends all elements of this $coll to a string builder using start, end, and separator strings.
     *  The written text begins with the string `start` and ends with the string `end`.
     *  Inside, the string representations (w.r.t. the method `toString`)
     *  of all elements of this `EquaSet` are separated by the string `sep`.
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

    /**
     * Tests if some element is contained in this set.
     *
     *  This method is equivalent to `contains`. It allows sets to be interpreted as predicates.
     *  @param elem the element to test for membership.
     *  @return  `true` if `elem` is contained in this set, `false` otherwise.
     */
    def apply(elem: T): Boolean

    /**
     * Builds a new collection by applying a partial function to all elements of this `EquaSet`
     * on which the function is defined.
     *
     * @param pf the partial function which filters and maps the `EquaSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `EquaSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collect(pf: PartialFunction[T, T]): thisEquaSets.EquaSet

    /**
     * Builds a new collection by applying a partial function to all elements of this `EquaSet`
     * on which the function is defined.
     *
     * @param thatEquaSets the `EquaSets` into which to filter and maps the `EquaSet`.
     * @param pf the partial function which filters and maps the `EquaSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `EquaSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`.
     * Copying will stop once either the end of the current `EquaSet` is reached,
     * or the end of the array is reached.
     *
     * @param xs the array to fill.
     *
     */
    def copyToArray(xs: Array[thisEquaSets.EquaBox]): Unit

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`, beginning at index `start`.
     * Copying will stop once either the end of the current `EquaSet` is reached,
     * or the end of the array is reached.
     *
     * @param xs the array to fill.
     * @param start the starting index.
     *
     */
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int): Unit

    /**
     * Copies values of this `EquaSet` to an array.
     * Fills the given array `xs` with values of this `EquaSet`, beginning at index `start`.
     * Copying will stop once the count of element copied reach <code>len</code>.
     *
     * @param xs the array to fill.
     * @param start the starting index.
     * @param len the length of elements to copy
     *
     */
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int, len: Int): Unit

    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.EquaBridge[T]
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.EquaBridge[T]

    /**
     * Computes the difference of this `EquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` of elements to exclude.
     * @return a `EquaSet` containing those elements of this
     * `EquaSet` that are not also contained in the given `EquaSet` `that`.
     */
    def diff(that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    /**
     * Computes the intersection between this `EquaSet` and another `EquaSet`.
     *
     * @param that the `EquaSet` to intersect with.
     * @return a new `EquaSet` consisting of all elements that are both in this
     * `EquaSet` and in the given `EquaSet` `that`.
     */
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.EquaSet
    def isEmpty: Boolean
    def iterator: Iterator[T]
    def size: Int
    def toSet: Set[thisEquaSets.EquaBox]
    def union(that: thisEquaSets.EquaSet): thisEquaSets.EquaSet

    private[scalactic] def owner: EquaSets[T] = thisEquaSets
  }

  class FastEquaBridge[S](from: List[S]) extends EquaBridge[S](from) {
    override def collect(pf: PartialFunction[S, T]): thisEquaSets.FastEquaSet =
      thisEquaSets.FastEquaSet.empty ++ (from collect pf)
  }

  class FastEquaSet private[scalactic] (private val underlying: Set[EquaBox]) extends EquaSet {
    def + (elem: T): thisEquaSets.FastEquaSet = new FastEquaSet(underlying + EquaBox(elem))
    def + (elem1: T, elem2: T, elem3: T*): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying + (EquaBox(elem1), EquaBox(elem2), elem3.map(EquaBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying ++ elems.toSeq.map(EquaBox(_)))
    def ++ (that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet = new FastEquaSet(underlying ++ that.toSet)
    def - (elem: T): thisEquaSets.FastEquaSet = new FastEquaSet(underlying - EquaBox(elem))
    def - (elem1: T, elem2: T, elem3: T*): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying - (EquaBox(elem1), EquaBox(elem2), elem3.map(EquaBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquaSets.EquaSet =
      new FastEquaSet(underlying -- elems.toSeq.map(EquaBox(_)))
    def --(that: thisEquaSets.EquaSet): thisEquaSets.EquaSet =
      new FastEquaSet(underlying -- that.toSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquaBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquaBox, b: B) => op(e.value, b))
    def | (that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet = this union that
    def & (that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet = this intersect that
    def &~ (that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquaBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquaBox(elem))
    def canEqual(that: Any): Boolean = that.isInstanceOf[thisEquaSets.EquaSet] && equality == that.asInstanceOf[thisEquaSets.EquaSet].owner.equality
    def collect(pf: PartialFunction[T, T]): thisEquaSets.EquaSet =
      new FastEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => EquaBox(pf(hb.value)) })
    def collectInto[U](thatEquaSets: EquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.EquaSet =
      new thatEquaSets.FastEquaSet(underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) })
    def collectInto[U](thatEquaSets: SortedEquaSets[U])(pf: PartialFunction[T, U]): thatEquaSets.SortedEquaSet =
      new thatEquaSets.TreeEquaSet(TreeSet.empty(thatEquaSets.ordering) ++ (underlying collect { case hb: thisEquaSets.EquaBox if pf.isDefinedAt(hb.value) => thatEquaSets.EquaBox(pf(hb.value)) }))
    def copyToArray(xs: Array[thisEquaSets.EquaBox]): Unit = underlying.copyToArray(xs)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int): Unit = underlying.copyToArray(xs, start)
    def copyToArray(xs: Array[thisEquaSets.EquaBox], start: Int, len: Int): Unit = underlying.copyToArray(xs, start, len)

    def into[U](thatEquaSets: EquaSets[U]): thatEquaSets.FastEquaBridge[T] = new thatEquaSets.FastEquaBridge[T](underlying.toList.map(_.value))
    def into[U](thatEquaSets: SortedEquaSets[U]): thatEquaSets.FastEquaBridge[T] = new thatEquaSets.FastEquaBridge[T](underlying.toList.map(_.value))

    def diff(that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying diff that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquaSets.FastEquaSet => 
          underlying == equiSet.underlying
        case _ => false
      }
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying intersect that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def size: Int = underlying.size
    def toSet: Set[thisEquaSets.EquaBox] = underlying
    // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
    override def toString: String = s"EquaSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquaSets.EquaSet): thisEquaSets.FastEquaSet =
      new FastEquaSet(underlying union that.toSet.map((eb: EquaBox) => EquaBox(eb.value)))
  }
  object FastEquaSet {
    def empty: FastEquaSet = new FastEquaSet(Set.empty)
    def apply(elems: T*): FastEquaSet = 
      new FastEquaSet(Set(elems.map(EquaBox(_)): _*))
  }
  object EquaSet {
    def empty: EquaSet = FastEquaSet.empty
    def apply(elems: T*): EquaSet = FastEquaSet(elems: _*)
  }
}

object EquaSets {
  def apply[T](equality: HashingEquality[T]): EquaSets[T] = new EquaSets(equality)
}
