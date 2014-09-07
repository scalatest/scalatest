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

class EquiSets[T](val equality: HashingEquality[T]) { thisEquiSets =>

  case class EquiBox(value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: EquiBox => equality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = equality.hashCodeFor(value)
    override def toString: String = s"EquiBox(${value.toString})"
  }

/*
  case class EquaBridge[U](thatEquiSets: EquiSets[U]) {
    def collect[U](pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
      new thatEquiSets.FastEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => thatEquiSets.EquiBox(pf(hb.value)) })
  }
*/

  trait EquiSet extends Function1[T, Boolean] with Equals {

    /**
     * Creates a new `EquiSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `EquiSet` that contains all elements of this `EquiSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquiSets.EquiSet

    /**
     * Creates a new `EquiSet` with additional elements.
     *
     * This method takes two or more elements to be added. Another overloaded
     * variant of this method handles the case where a single element is added.
     *
     * @param elem1 the first element to add.
     * @param elem2 the second element to add.
     * @param elems the remaining elements to add.
     * @return a new `EquiSet` with the given elements added.
     */
    def + (elem1: T, elem2: T, elems: T*): thisEquiSets.EquiSet

    /** Creates a new `EquiSet` by adding all elements contained in another collection to this `EquiSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `EquiSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquiSets.EquiSet

    /**
     * Creates a new `EquiSet` by adding elements contained in another `EquiSet`.
     *
     * @param that     the other `EquiSet` containing the added elements.
     * @return         a new `EquiSet` with the given elements added.
     */
    def ++ (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * Creates a new `EquiSet` with a given element removed from this `EquiSet`.
     *
     * @param elem the element to be removed
     * @return a new `EquiSet` that contains all elements of this `EquiSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquiSets.EquiSet

    /* * USE LATER
     * Creates a new `EquiSet` from this `EquiSet` by removing all elements of another
     * collection.
     *
     * @param xs the collection containing the removed elements.
     * @return a new `EquiSet` that contains all elements of the current `EquiSet`
     * except one less occurrence of each of the elements of `elems`.
     */

    /**
     * Creates a new `EquiSet` from this `EquiSet` with some elements removed.
     *
     * This method takes two or more elements to be removed. Another overloaded
     * variant of this method handles the case where a single element is
     * removed.
     * @param elem1 the first element to remove.
     * @param elem2 the second element to remove.
     * @param elems the remaining elements to remove.
     * @return a new `EquiSet` that contains all elements of the current `EquiSet`
     * except one less occurrence of each of the given elements.
     */
    def - (elem1: T, elem2: T, elems: T*): thisEquiSets.EquiSet

    /**
     * Creates a new `EquiSet` from this `EquiSet` by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `EquiSet` that contains all elements of the current `EquiSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquiSets.EquiSet

    /**
     * Creates a new `EquiSet` from this `EquiSet` by removing all elements of another `EquiSet`
     *
     * @param that       the other `EquiSet` containing the removed elements.
     * @return a new `EquiSet` that contains all elements of the current `EquiSet` minus elements contained in the passed in `EquiSet`.
     */
    def --(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * Applies a binary operator to a start value and all elements of this `EquiSet`,
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
     * Applies a binary operator to all elements of this `EquiSet` and a start value,
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
     * Computes the union between this `EquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquiSet` to form the union with.
     * @return a new `EquiSet` consisting of all elements that are in this
     * `EquiSet` or in the given `EquiSet` `that`.
     */
    def | (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * Computes the intersection between this `EquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquiSet` to intersect with.
     * @return a new `EquiSet` consisting of all elements that are both in this
     * `EquiSet` and in the given `EquiSet` `that`.
     */
    def & (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * The difference of this `EquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquiSet` of elements to exclude.
     * @return a `EquiSet` containing those elements of this
     * `EquiSet` that are not also contained in the given `EquiSet` `that`.
     */
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * Appends all elements of this `EquiSet` to a string builder.
     *  The written text consists of the string representations (w.r.t. the method
     * `toString`) of all elements of this `EquiSet` without any separator string.
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
     * Appends all elements of this `EquiSet` to a string builder using a separator string.
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
     *  of all elements of this `EquiSet` are separated by the string `sep`.
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
     * Builds a new collection by applying a partial function to all elements of this `EquiSet`
     * on which the function is defined.
     *
     * @param pf the partial function which filters and maps the `EquiSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `EquiSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collect(pf: PartialFunction[T, T]): thisEquiSets.EquiSet

    /**
     * Builds a new collection by applying a partial function to all elements of this `EquiSet`
     * on which the function is defined.
     *
     * @param thatEquiSets the `EquiSets` into which to filter and maps the `EquiSet`.
     * @param pf the partial function which filters and maps the `EquiSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `EquiSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collectInto[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet
    def collectInto[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet

    //def into[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet
    //def into[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet

    /**
     * Computes the difference of this `EquiSet` and another `EquiSet`.
     *
     * @param that the `EquiSet` of elements to exclude.
     * @return a `EquiSet` containing those elements of this
     * `EquiSet` that are not also contained in the given `EquiSet` `that`.
     */
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    /**
     * Computes the intersection between this `EquiSet` and another `EquiSet`.
     *
     * @param that the `EquiSet` to intersect with.
     * @return a new `EquiSet` consisting of all elements that are both in this
     * `EquiSet` and in the given `EquiSet` `that`.
     */
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def isEmpty: Boolean
    def iterator: Iterator[T]
    def size: Int
    def toSet: Set[thisEquiSets.EquiBox]
    def union(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet

    private[scalactic] def owner: EquiSets[T] = thisEquiSets
  }

  class FastEquiSet private[scalactic] (private val underlying: Set[EquiBox]) extends EquiSet {
    def + (elem: T): thisEquiSets.FastEquiSet = new FastEquiSet(underlying + EquiBox(elem))
    def + (elem1: T, elem2: T, elem3: T*): thisEquiSets.FastEquiSet =
      new FastEquiSet(underlying + (EquiBox(elem1), EquiBox(elem2), elem3.map(EquiBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquiSets.EquiSet =
      new FastEquiSet(underlying ++ elems.toSeq.map(EquiBox(_)))
    def ++ (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet = new FastEquiSet(underlying ++ that.toSet)
    def - (elem: T): thisEquiSets.FastEquiSet = new FastEquiSet(underlying - EquiBox(elem))
    def - (elem1: T, elem2: T, elem3: T*): thisEquiSets.FastEquiSet =
      new FastEquiSet(underlying - (EquiBox(elem1), EquiBox(elem2), elem3.map(EquiBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquiSets.EquiSet =
      new FastEquiSet(underlying -- elems.toSeq.map(EquiBox(_)))
    def --(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet =
      new FastEquiSet(underlying -- that.toSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquiBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquiBox, b: B) => op(e.value, b))
    def | (that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet = this union that
    def & (that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet = this intersect that
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquiBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquiBox(elem))
    def canEqual(that: Any): Boolean = that.isInstanceOf[thisEquiSets.EquiSet] && equality == that.asInstanceOf[thisEquiSets.EquiSet].owner.equality
    def collect(pf: PartialFunction[T, T]): thisEquiSets.EquiSet =
      new FastEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => EquiBox(pf(hb.value)) })
    def collectInto[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
      new thatEquiSets.FastEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => thatEquiSets.EquiBox(pf(hb.value)) })
    def collectInto[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
      new thatEquiSets.TreeEquiSet(TreeSet.empty(thatEquiSets.ordering) ++ (underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => thatEquiSets.EquiBox(pf(hb.value)) }))
    //def into[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
    //  new thatEquiSets.FastEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => thatEquiSets.EquiBox(pf(hb.value)) })
    //def into[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
    //  new thatEquiSets.FastEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => thatEquiSets.EquiBox(pf(hb.value)) })
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet =
      new FastEquiSet(underlying diff that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquiSets.FastEquiSet => 
          underlying == equiSet.underlying
        case _ => false
      }
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet =
      new FastEquiSet(underlying intersect that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def size: Int = underlying.size
    def toSet: Set[thisEquiSets.EquiBox] = underlying
    // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
    override def toString: String = s"EquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquiSets.EquiSet): thisEquiSets.FastEquiSet =
      new FastEquiSet(underlying union that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
  }
  object FastEquiSet {
    def empty: FastEquiSet = new FastEquiSet(Set.empty)
    def apply(elems: T*): FastEquiSet = 
      new FastEquiSet(Set(elems.map(EquiBox(_)): _*))
  }
  object EquiSet {
    def empty: EquiSet = FastEquiSet.empty
    def apply(elems: T*): EquiSet = FastEquiSet(elems: _*)
  }
}

object EquiSets {
  def apply[T](equality: HashingEquality[T]): EquiSets[T] = new EquiSets(equality)
}
