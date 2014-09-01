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
  }

  private class TreeEquiSet private (private val underlying: TreeSet[EquiBox]) extends SortedEquiSet {
    def + (elem: T): thisEquiSets.TreeEquiSet = new TreeEquiSet(underlying + EquiBox(elem))
    def + (elem1: T, elem2: T, elems: T*): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying + (EquiBox(elem1), EquiBox(elem2), elems.map(EquiBox(_)): _*))
    def ++ (elems: GenTraversableOnce[T]): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying ++ elems.toSeq.map(EquiBox(_)))
    def - (elem: T): thisEquiSets.TreeEquiSet = new TreeEquiSet(underlying - EquiBox(elem))
    def - (elem1: T, elem2: T, elems: T*): thisEquiSets.TreeEquiSet =
      new TreeEquiSet(underlying - (EquiBox(elem1), EquiBox(elem2), elems.map(EquiBox(_)): _*))
    def --(elems: GenTraversableOnce[T]): thisEquiSets.EquiSet =
      new TreeEquiSet(underlying -- elems.toSeq.map(EquiBox(_)))
    def | (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this union that
    def & (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this intersect that
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.TreeEquiSet = this diff that
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
  private object TreeEquiSet {
    def empty: TreeEquiSet = new TreeEquiSet(TreeSet.empty(ordering))
    def apply(elems: T*): TreeEquiSet = 
      new TreeEquiSet(TreeSet(elems.map(EquiBox(_)): _*)(ordering))
  }
}

object SortedEquiSets {
  def apply[T](equality: OrderingEquality[T]): SortedEquiSets[T] = new SortedEquiSets(equality)
}

