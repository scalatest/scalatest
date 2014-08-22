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

class SortedEquiSets[T](val hashingEquality: HashingEquality[T]) { thisSortedEquiSets =>
  case class OrdBox(value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: OrdBox => hashingEquality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = hashingEquality.hashCodeFor(value)
    override def toString: String = s"OrdBox(${value.toString})"
  }
  class SortedEquiSet private (private val underlying: Set[OrdBox]) {
    def + (elem: T): thisSortedEquiSets.SortedEquiSet = new SortedEquiSet(underlying + OrdBox(elem))
    def + (elem1: T, elem2: T, elem3: T*): thisSortedEquiSets.SortedEquiSet =
      new SortedEquiSet(underlying + (OrdBox(elem1), OrdBox(elem2), elem3.map(OrdBox(_)): _*))
    def - (elem: T): thisSortedEquiSets.SortedEquiSet = new SortedEquiSet(underlying - OrdBox(elem))
    def - (elem1: T, elem2: T, elem3: T*): thisSortedEquiSets.SortedEquiSet =
      new SortedEquiSet(underlying - (OrdBox(elem1), OrdBox(elem2), elem3.map(OrdBox(_)): _*))
    def | (that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet = this union that
    def & (that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet = this intersect that
    def &~ (that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet = this diff that
    def diff(that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet =
      new SortedEquiSet(underlying diff that.underlying)
    override def equals(other: Any): Boolean =
      other match {
        case equiSet: thisSortedEquiSets.SortedEquiSet => 
          underlying == equiSet.underlying
        case _ => false
      }
    override def hashCode: Int = underlying.hashCode
    def intersect(that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet =
      new SortedEquiSet(underlying intersect that.underlying)
    def isEmpty: Boolean = underlying.isEmpty
    def size: Int = underlying.size
    def toSet: Set[thisSortedEquiSets.OrdBox] = underlying
    override def toString: String = s"SortedEquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisSortedEquiSets.SortedEquiSet): thisSortedEquiSets.SortedEquiSet =
      new SortedEquiSet(underlying union that.underlying)
  }
  object SortedEquiSet {
    def empty: SortedEquiSet = new SortedEquiSet(Set.empty)
    def apply(elems: T*): SortedEquiSet = 
      new SortedEquiSet(Set(elems.map(OrdBox(_)): _*))
  }
  override def toString: String = s"SortedEquiSets($hashingEquality)"
}

object SortedEquiSets {
  def apply[T](hashingEquality: HashingEquality[T]): SortedEquiSets[T] = new SortedEquiSets(hashingEquality)
}

