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

  trait EquiSet {

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
  }

  private class HashEquiSet private (private val underlying: Set[EquiBox]) extends EquiSet {
    def + (elem: T): thisEquiSets.HashEquiSet = new HashEquiSet(underlying + EquiBox(elem))
    def + (elem1: T, elem2: T, elem3: T*): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying + (EquiBox(elem1), EquiBox(elem2), elem3.map(EquiBox(_)): _*))
    def - (elem: T): thisEquiSets.HashEquiSet = new HashEquiSet(underlying - EquiBox(elem))
    def - (elem1: T, elem2: T, elem3: T*): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying - (EquiBox(elem1), EquiBox(elem2), elem3.map(EquiBox(_)): _*))
    def | (that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet = this union that
    def & (that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet = this intersect that
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet = this diff that
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying diff that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisEquiSets.HashEquiSet => 
          underlying == equiSet.underlying
        case _ => false
      }
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying intersect that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
    def isEmpty: Boolean = underlying.isEmpty
    def iterator: Iterator[T] = underlying.iterator.map(_.value)
    def size: Int = underlying.size
    def toSet: Set[thisEquiSets.EquiBox] = underlying
    // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
    override def toString: String = s"EquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying union that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
  }
  private object HashEquiSet {
    def empty: HashEquiSet = new HashEquiSet(Set.empty)
    def apply(elems: T*): HashEquiSet = 
      new HashEquiSet(Set(elems.map(EquiBox(_)): _*))
  }
  object EquiSet {
    def empty: EquiSet = HashEquiSet.empty
    def apply(elems: T*): EquiSet = HashEquiSet(elems: _*)
  }
}

object EquiSets {
  def apply[T](equality: HashingEquality[T]): EquiSets[T] = new EquiSets(equality)
}

