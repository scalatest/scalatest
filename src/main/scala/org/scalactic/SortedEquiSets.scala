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
import scala.collection.immutable.SortedSet
import scala.collection.immutable.TreeSet

class SortedEquiSets[T](override val equality: OrderingEquality[T]) extends EquiSets[T](equality) { thisEquiSets =>

  val ordering: Ordering[thisEquiSets.EquiBox] =
    new Ordering[thisEquiSets.EquiBox] {
      def compare(a: thisEquiSets.EquiBox, b: thisEquiSets.EquiBox): Int =
        equality.compare(a.value, b.value)
    }

  trait SortedEquiSet extends EquiSet {

    /**
     * Creates a new `SortedEquiSet` with an additional element, unless the element is
     * already present.
     *
     * @param elem the element to be added
     * @return a new `SortedEquiSet` that contains all elements of this `SortedEquiSet` and that also
     * contains `elem`.
     */
    def + (elem: T): thisEquiSets.SortedEquiSet

    /**
     * Creates a new `SortedEquiSet` with additional elements.
     *
     * This method takes two or more elements to be added. Another overloaded
     * variant of this method handles the case where a single element is added.
     *
     * @param elem1 the first element to add.
     * @param elem2 the second element to add.
     * @param elems the remaining elements to add.
     * @return a new `SortedEquiSet` with the given elements added.
     */
    def + (elem1: T, elem2: T, elems: T*): thisEquiSets.SortedEquiSet

    /** Creates a new `SortedEquiSet` by adding all elements contained in another collection to this `SortedEquiSet`.
      *
      *  @param elems     the collection containing the added elements.
      *  @return          a new `SortedEquiSet` with the given elements added.
      */
    def ++ (elems: GenTraversableOnce[T]): thisEquiSets.EquiSet

    /**
     * Creates a new `SortedEquiSet` by adding elements contained in another `EquiSet`.
     *
     * @param that     the other `EquiSet` containing the added elements.
     * @return         a new `SortedEquiSet` with the given elements added.
     */
    def ++ (that: EquiSet): thisEquiSets.SortedEquiSet

    /**
     * Creates a new `SortedEquiSet` with a given element removed from this `SortedEquiSet`.
     *
     * @param elem the element to be removed
     * @return a new `SortedEquiSet` that contains all elements of this `SortedEquiSet` but that does not
     * contain `elem`.
     */
    def - (elem: T): thisEquiSets.SortedEquiSet

    /* * USE LATER
     * Creates a new `SortedEquiSet` from this `SortedEquiSet` by removing all elements of another
     * collection.
     *
     * @param xs the collection containing the removed elements.
     * @return a new `SortedEquiSet` that contains all elements of the current `SortedEquiSet`
     * except one less occurrence of each of the elements of `elems`.
     */

    /**
     * Creates a new `SortedEquiSet` from this `SortedEquiSet` with some elements removed.
     *
     * This method takes two or more elements to be removed. Another overloaded
     * variant of this method handles the case where a single element is
     * removed.
     * @param elem1 the first element to remove.
     * @param elem2 the second element to remove.
     * @param elems the remaining elements to remove.
     * @return a new `SortedEquiSet` that contains all elements of the current `SortedEquiSet`
     * except one less occurrence of each of the given elements.
     */
    def - (elem1: T, elem2: T, elems: T*): thisEquiSets.SortedEquiSet

    /**
     * Creates a new `SortedEquiSet` from this $coll by removing all elements of another
     *  collection.
     *
     *  @param elems     the collection containing the removed elements.
     *  @return a new `SortedEquiSet` that contains all elements of the current `SortedEquiSet`
     *  except one less occurrence of each of the elements of `elems`.
     */
    def --(elems: GenTraversableOnce[T]): thisEquiSets.EquiSet

    /**
     * Creates a new `SortedEquiSet` from this `SortedEquiSet` by removing all elements of another `EquiSet`
     *
     * @param that       the other `EquiSet` containing the removed elements.
     * @return a new `SortedEquiSet` that contains all elements of the current `EquiSet` minus elements contained in the passed in `EquiSet`.
     */
    def --(that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    /**
     * Applies a binary operator to a start value and all elements of this `SortedEquiSet`,
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
     * Applies a binary operator to all elements of this `SortedEquiSet` and a start value,
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
     * Computes the union between this `SortedEquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `union`.
     * @param that the `EquiSet` to form the union with.
     * @return a new `SortedEquiSet` consisting of all elements that are in this
     * `SortedEquiSet` or in the given `EquiSet` `that`.
     */
    def | (that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    /**
     * Computes the intersection between this `SortedEquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `intersect`.
     * @param that the `EquiSet` to intersect with.
     * @return a new `SortedEquiSet` consisting of all elements that are both in this
     * `SortedEquiSet` and in the given `EquiSet` `that`.
     */
    def & (that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    /**
     * The difference of this `SortedEquiSet` and another `EquiSet`.
     *
     * '''Note:''' Same as `diff`.
     * @param that the `EquiSet` of elements to exclude.
     * @return a `SortedEquiSet` containing those elements of this
     * `SortedEquiSet` that are not also contained in the given `EquiSet` `that`.
     */
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    /**
     * Builds a new collection by applying a partial function to all elements of this `SortedEquiSet`
     * on which the function is defined.
     *
     * @param pf the partial function which filters and maps the `SortedEquiSet`.
     * @return a new collection of type `That` resulting from applying the partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     *
     * @return a new `SortedEquiSet` resulting from applying the given partial function
     * `pf` to each element on which it is defined and collecting the results.
     * The order of the elements is preserved.
     */
    def collect(pf: PartialFunction[T, T]): thisEquiSets.SortedEquiSet

    def collectInto[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet
    def collectInto[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.SortedEquiSet
    //def into[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet
    //def into[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.SortedEquiSet

    /**
     * Computes the difference of this `SortedEquiSet` and another `EquiSet`.
     *
     * @param that the `EquiSet` of elements to exclude.
     * @return a `SortedEquiSet` containing those elements of this
     * `SortedEquiSet` that are not also contained in the given `EquiSet` `that`.
     */
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    /**
     * Computes the intersection between this `SortedEquiSet` and another `EquiSet`.
     *
     * @param that the `EquiSet` to intersect with.
     * @return a new `SortedEquiSet` consisting of all elements that are both in this
     * `SortedEquiSet` and in the given `EquiSet` `that`.
     */
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet
    def isEmpty: Boolean
    def iterator: Iterator[T]
    def size: Int
    def toSet: SortedSet[thisEquiSets.EquiBox]
    def union(that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet

    private[scalactic] override def owner: SortedEquiSets[T] = thisEquiSets
  }

  class TreeEquiSet private (private val underlying: TreeSet[EquiBox]) extends SortedEquiSet {
    def + (elem: T): thisEquiSets.TreeEquiSet = new TreeEquiSet(underlying + EquiBox(elem))
    def + (elem1: T, elem2: T, elems: T*): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying + (EquiBox(elem1), EquiBox(elem2), elems.map(EquiBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying ++ elems.toSeq.map(EquiBox(_)))
    def ++ (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = new TreeEquiSet(underlying ++ that.toSet)
    def - (elem: T): thisEquiSets.TreeEquiSet = new TreeEquiSet(underlying - EquiBox(elem))
    def - (elem1: T, elem2: T, elems: T*): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying - (EquiBox(elem1), EquiBox(elem2), elems.map(EquiBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquiSets.EquiSet =
      new TreeEquiSet(underlying -- elems.toSeq.map(EquiBox(_)))
    def --(that: thisEquiSets.EquiSet): thisEquiSets.SortedEquiSet =
      new TreeEquiSet(underlying -- that.toSet)
    def /:[B](z: B)(op: (B, T) => B): B =
      underlying./:(z)((b: B, e: EquiBox) => op(b, e.value))
    def :\[B](z: B)(op: (T, B) => B): B =
      underlying.:\(z)((e: EquiBox, b: B) => op(e.value, b))
    def | (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this union that
    def & (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this intersect that
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this diff that
    def addString(b: StringBuilder): StringBuilder = underlying.map(_.value).addString(b)
    def addString(b: StringBuilder, sep: String): StringBuilder = underlying.map(_.value).addString(b, sep)
    def addString(b: StringBuilder, start: String, sep: String, end: String): StringBuilder = underlying.map(_.value).addString(b, start, sep, end)
    def aggregate[B](z: =>B)(seqop: (B, T) => B, combop: (B, B) => B): B = underlying.aggregate(z)((b: B, e: EquiBox) => seqop(b, e.value), combop)
    def apply(elem: T): Boolean = underlying.apply(EquiBox(elem))
    def canEqual(that: Any): Boolean = that.isInstanceOf[thisEquiSets.EquiSet] && equality == that.asInstanceOf[thisEquiSets.EquiSet].owner.equality
    def collect(pf: PartialFunction[T, T]): thisEquiSets.SortedEquiSet = {
      implicit val ord: Ordering[thisEquiSets.EquiBox] = ordering
      new TreeEquiSet(underlying collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => EquiBox(pf(hb.value)) })
    }
    def collectInto[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
      thatEquiSets.EquiSet(underlying.toList collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => pf(hb.value) }: _*)
    def collectInto[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.SortedEquiSet =
      thatEquiSets.SortedEquiSet(underlying.toList collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => pf(hb.value) }: _*)
    //def into[U](thatEquiSets: EquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.EquiSet =
    //  thatEquiSets.EquiSet(underlying.toList collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => pf(hb.value) }: _*)
    //def into[U](thatEquiSets: SortedEquiSets[U])(pf: PartialFunction[T, U]): thatEquiSets.SortedEquiSet =
    //  thatEquiSets.SortedEquiSet(underlying.toList collect { case hb: thisEquiSets.EquiBox if pf.isDefinedAt(hb.value) => pf(hb.value) }: _*)
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying diff that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquiSets.EquiSet => 
          underlying == equiSet.toSet
        case _ => false
      }
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying intersect that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def size: Int = underlying.size
    def toSet: TreeSet[thisEquiSets.EquiBox] = underlying
    override def toString: String = s"SortedEquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying union that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
  }
  object SortedEquiSet {
    def empty: SortedEquiSet = TreeEquiSet.empty
    def apply(elems: T*): SortedEquiSet = TreeEquiSet(elems: _*)
  }
  object TreeEquiSet {
    def empty: TreeEquiSet = new TreeEquiSet(TreeSet.empty(ordering))
    def apply(elems: T*): TreeEquiSet = 
      new TreeEquiSet(TreeSet(elems.map(EquiBox(_)): _*)(ordering))
  }
}

object SortedEquiSets {
  def apply[T](equality: OrderingEquality[T]): SortedEquiSets[T] = new SortedEquiSets(equality)
}

