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
    def + (elem: T): thisEquiSets.EquiSet
    def + (elem1: T, elem2: T, elem3: T*): thisEquiSets.EquiSet
    def - (elem: T): thisEquiSets.EquiSet
    def - (elem1: T, elem2: T, elem3: T*): thisEquiSets.EquiSet
    def | (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def & (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def &~ (that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def diff(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def intersect(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
    def isEmpty: Boolean
    def size: Int
    def toSet: Set[thisEquiSets.EquiBox]
    def union(that: thisEquiSets.EquiSet): thisEquiSets.EquiSet
  }

  class HashEquiSet private (private val underlying: Set[EquiBox]) extends EquiSet {
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
    def size: Int = underlying.size
    def toSet: Set[thisEquiSets.EquiBox] = underlying
    // Be consistent with standard library. HashSet's toString is Set(1, 2, 3)
    override def toString: String = s"EquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
    def union(that: thisEquiSets.EquiSet): thisEquiSets.HashEquiSet =
      new HashEquiSet(underlying union that.toSet.map((eb: EquiBox) => EquiBox(eb.value)))
  }
  object HashEquiSet {
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

