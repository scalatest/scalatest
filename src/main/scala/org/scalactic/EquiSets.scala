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

class EquiSets[T](val hashingEquality: HashingEquality[T]) { thisEquiSets =>
  case class Wrapped(value: T) {
    override def equals(o: Any): Boolean = 
      o match {
        case other: Wrapped => hashingEquality.areEqual(value, other.value)
        case _ => false
      }
    override def hashCode: Int = hashingEquality.hashCodeFor(value)
    override def toString: String = s"Wrapped(${value.toString})"
  }
  class EquiSet private (underlying: Set[Wrapped]) {
    def isEmpty: Boolean = underlying.isEmpty
    def size: Int = underlying.size
    // def union[E](that: thisEquiSets.EquiSet[T]) = 1
    override def toString: String = s"EquiSet(${underlying.toVector.map(_.value).mkString(", ")})"
  }
  object EquiSet {
    def empty: EquiSet = new EquiSet(Set.empty)
    def apply(elems: T*): EquiSet = 
      new EquiSet(Set(elems.map(Wrapped(_)): _*))
  }
}

object EquiSets {
  def apply[T](hashingEquality: HashingEquality[T]): EquiSets[T] = new EquiSets(hashingEquality)
}

